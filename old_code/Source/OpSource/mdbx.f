      SUBROUTINE MDBX(TYPE,REQ,OUT,NOBS_USER,NELEMS_USER,IX,CSTR,CREP)
      IMPLICIT NONE

!----------------------------------------------------------------------
!
! PROGRAM       : MDBX
!
! PURPOSE       : To combine data from different models
!                 (Request as for MDB except for MODEL after ELEMENTS,
!                  but request period is split into hours, so order of
!                  obs returned will be different.)
!
! STRUCTURE     : 1. Split input request into requests for the various
!                    models involved.
!                 2. Look for data an hour at a time until there is
!                    something to return (i.e. skip any hour with no
!                    data without returning to the user), keeping all
!                    the hour's data in core so that each model's data
!                    can be sorted into the same order & merged as it's
!                    put in the output array.
!                 3. Fill the user's array & set return code.
!
! DATA TYPES    : any retrievable by MDB
!
! CALLED BY     : user
!
! CALLS         : MDBXDAY, MDBXREQ, ONEHOUR, MDB, SORTR
!
! PARAMETERS    : as for MDB call, except for MODEL after ELEMENTS
!                  (but no entities - and no nested replication,
!                   best avoided anyway...)
!                 (1) data type
!                 (2) request (as for MDB, but with MODEL keyword
!                      allowed in element list to get following
!                      elements from that model rather than model
!                      mentioned or implicit before ELEMENTS)
!                 (3) array dimensioned as for MDB
!                 (4) NOBS as for MDB except that it can be returned
!                      as less than the input value even when there
!                      is more data to come (for next hour)
!                 (5) NELEM as for MDB
!                 (6) return code (4 if more to come for this hour
!                      or another hour to come, 0 if finished)
!                 (7) & (8) character & report strings as for MDB
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 28/02/2006 11:44:15$
! $Source: /data/us0400/mdb/op/lib/merge/RCS/mdbx.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 11:44:15    Sheila Needham  
! $
! Revision 1.2  2005/07/05 10:42:48  usmdb
! 1.2.  18 July 2005.  Brian Barwell.  INC156293.
! Parameter MAX_NOBS increased from 999 to 2000 for BUOY data.
!
! Revision 1.1 2003/04/15 14:10:18 14:10:18  usmdb (MetDB account c/o J C Ward)
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2005 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of Software Development.
!
!----------------------------------------------------------------------

      INTEGER   MAX_NMODELS           ! max number of models (array size
      INTEGER   NMODELS               ! actual number of models
      INTEGER   MAX_NELEMS            ! max element count (MASK size)
      INTEGER   MAX_NOBS              ! max number of obs from MDB
      INTEGER   MAX_STRING            ! string length for MDB characters

      PARAMETER (MAX_NOBS=2000)       ! increased from 999 for BUOYs!1.2
      PARAMETER (MAX_NELEMS=99)
      PARAMETER (MAX_STRING=21)       ! enough for ident, TTAAii, CCCC
      PARAMETER (MAX_NMODELS=2)

      INTEGER   NOBS(MAX_NMODELS)     ! NOBS from MDB for each model
      INTEGER   IOB(MAX_NMODELS)      ! pointer to data from M-th model
      INTEGER   NELEMS(MAX_NMODELS)   ! NELEM for MDB for each model
      INTEGER   MAP(MAX_NMODELS)      ! number of elements for user
                                      !  before those from M-th model
      INTEGER   I                     ! ob subscript in output array
      INTEGER   J                     ! elemnt subscript in output array
      INTEGER   IJ                    ! coordinate equivalent to (I,J)
      INTEGER   NREQ_ELEMS            ! number of elements found in REQ
      INTEGER   IX                    ! return code from MDBX to user
      INTEGER   IELEM                 ! element subscript
      INTEGER   KOORD                 ! coordinate subscript
      INTEGER   MODEL                 ! model subscript (1,NMODEL)
      INTEGER   MOST_NOBS_MODEL       ! model with greatest NOBS
      INTEGER   NOBS_RETURNED         ! number of obs for this hour
                                      !  already returned to user
                                      ! (to cope with NOBSOUT<NOBSGT)
      INTEGER   NOB_ONE_OR_ZERO       ! 1 if ob this time, 0 if no more
                                      !  (if one ob at a time from MDB)
      INTEGER   MOST_NOBS             ! greatest of NOBS(M)
      INTEGER   NOBS_USER             ! NOBS for user
      INTEGER   MAX_NOBS_USER         ! keep user's NOBS for next hour
      INTEGER   NSORT_COORDS          ! number of coordinates for sort
      INTEGER   NELEMS_USER           ! keep users NELEM for check
                                      !  after splitting up request
      INTEGER   ONEHOUR_RC            ! return code from ONEHOUR
                                      !  (0 if lat hour, 4 if not)
      INTEGER   ZEND_TIME             ! points to Z after END TIME

      CHARACTER REQ*(*)               ! input request
      CHARACTER REQS*2000(MAX_NMODELS)! requests for different models
      CHARACTER CSTR*(*)(*)
      CHARACTER CREP*1(*)
      CHARACTER TYPE*8                ! data type (input)
      CHARACTER*(MAX_STRING) IDENT(MAX_NOBS)  ! GLOBL_AIR ident etc
      CHARACTER*132 HEAD

      REAL      OUT(*)                ! array to return to user
      REAL      A(MAX_NOBS*MAX_NELEMS,MAX_NMODELS)
      REAL      MASK(MAX_NELEMS)      ! mask for sort
      LOGICAL   SKIP                  ! set if no data from this model
      DATA      MASK/MAX_NELEMS*0/    ! some zeros will be set =1 later
      DATA      NOBS_RETURNED/0/
      DATA      MOST_NOBS/0/
      DATA      ONEHOUR_RC/-1/        ! -1 till ONEHOUR called

      COMMON /MDBXCOM/ A, IDENT                                     !1.2

!**********************************************************************
! First time only: split user's request up into requests for
! different models.   First replace any TODAY by explict date.

      IF (ONEHOUR_RC.EQ.-1) THEN
        HEAD='$RCSfile: mdbx.f,v $ ' //
     &       '$Revision: 1$ $Date: 28/02/2006 11:44:15$'

        REQS(1)=REQ                 ! copy request
        CALL MDBXDAY(REQS(1),REQ)   ! copy back replacing TODAY

        CALL MDBXREQ(REQ,REQS,NELEMS,NMODELS,NREQ_ELEMS,NSORT_COORDS)

! Check that number of element names found in user's request is
! consistent with input NELEM.  If not, or too many elements for
! declared size of work array, stop now.

        IF (NREQ_ELEMS.NE.NELEMS_USER) THEN
          PRINT *,'NELEM & request inconsistent:',NREQ_ELEMS,NELEMS_USER
          PRINT *,' are there any entities among the elements?'
          STOP
        ENDIF

        IF (NREQ_ELEMS.GT.MAX_NELEMS) THEN
          PRINT *,' Too many elements requested! ',MAX_NELEMS,'allowed'
          STOP
        ENDIF

! MAP(M) is number of elements for user before those from M-th model

        MAP(1)=0
        DO MODEL=2,NMODELS
          MAP(MODEL)=MAP(MODEL-1)+NELEMS(MODEL-1)-NSORT_COORDS
        ENDDO

! Set mask to sort on all mandatory coordinates (in order of appearance,
! not counting slot for identifier sequence number)

        DO IELEM=1,NSORT_COORDS-1
          MASK(IELEM)=IELEM
        ENDDO

        MAX_NOBS_USER=NOBS_USER       ! keep user's dimension
      ENDIF                           ! end of first-call-only code

!**********************************************************************
! Retrieve data one hour at a time.  Successive calls to ONEHOUR
! set successive hours in the start & end times of the request.
! Copy changed start & end times to requests for other models.
! Loop if no data for an hour & more hours to try.

      DO WHILE (MOST_NOBS.EQ.0 .AND. ONEHOUR_RC.NE.0)
        CALL ONEHOUR(REQS(1),ONEHOUR_RC)
        ZEND_TIME=INDEX(REQS(1)(30:),'Z ')      ! find Z after END TIME
        DO MODEL=2,NMODELS
          REQS(MODEL)(1:30+ZEND_TIME)=REQS(1)(1:30+ZEND_TIME)
        ENDDO

        NOBS_USER=MAX_NOBS_USER       ! reset in case =0 after last hour
        DO MODEL=1,NMODELS
          IOB(MODEL)=0
        ENDDO

! Retrieve all obs for this hour. If the work array's too small, so
! there are obs left to come, then this approach won't work, so stop.
!  Retrieve one ob at a time to keep data right way round for sort,
! all elements for an ob together rather than all values of an element.

        DO MODEL=1,NMODELS
          NOBS(MODEL)=0
          NOB_ONE_OR_ZERO=1
          IX=0
          DO WHILE (NOB_ONE_OR_ZERO.EQ.1 .AND. IX.LE.4 .AND.       !1.1
     &              NOBS(MODEL).LT.MAX_NOBS*MAX_NELEMS/NELEMS(MODEL)
     &              .AND. .NOT.(IX.EQ.0 .AND. NOBS(MODEL).GT.0))
            CALL MDB(TYPE,REQS(MODEL),
     &               A(NOBS(MODEL)*NELEMS(MODEL)+1,MODEL),
     &               NOB_ONE_OR_ZERO,NELEMS(MODEL),IX,CSTR,CREP)
            NOBS(MODEL)=NOBS(MODEL)+NOB_ONE_OR_ZERO

            IF (IX.EQ.16) THEN                                     !1.1
              PRINT *,'Fatal MDB error'                            !1.1
              STOP                                                 !1.1
            ENDIF                                                  !1.1

! After incrementing NOBS keep ident etc in array if first model
! (GLOBL_AIR) and put current NOBS in second minute slot for sort
! so that right ident etc can be found for user after sort.

            IF (MODEL.EQ.1 .AND. NOB_ONE_OR_ZERO.EQ.1) THEN
              IF (NOBS(MODEL).LE.MAX_NOBS) THEN                    !1.2
                IDENT(NOBS(MODEL))=CSTR(1)
              ELSE                                                 !1.2
                PRINT *,' IDENT array too small for 1 hour''s obs' !1.2
                PRINT *,' - increase MAX_NOBS in MDBX source'      !1.2
                STOP                                               !1.2
              END IF                                               !1.2
              A((NOBS(MODEL)-1)*NELEMS(MODEL)+4,MODEL)=NOBS(MODEL)
            ENDIF
          ENDDO

          IF (NOB_ONE_OR_ZERO.EQ.1 .AND. IX.NE.0) THEN
            PRINT *,' Work array too small for all data for this hour'
            PRINT *,' - get MAX_NOBS or MAX_NELEMS changed in MDBX'
            STOP
          ENDIF

! Sort into coordinate order.

          CALL SORTR(A(1,MODEL),NELEMS(MODEL),NOBS(MODEL),MASK)

! Find biggest NOBS from retrievals. (Assume any smaller NOBS implies
! a subset of the biggest.)

          IF (NOBS(MODEL).GT.MOST_NOBS) THEN
            MOST_NOBS=NOBS(MODEL)
            MOST_NOBS_MODEL=MODEL
          ENDIF
        ENDDO                         ! end of loop round models
      ENDDO                           ! end of retrievals for hour

!**********************************************************************
! Any intervening hours without data have been skipped, so now there is
! either data to return to the user or no more data to come.
! Put values from the sorted arrays into the user's array,
! skipping any model with no data for the ob which comes next in order
! of sorted coordinates.
! (The loop round the coordinates added for sorting to set SKIP is done
!  whenever the model changes (NMODEL times) during the element loop.)

      DO I=NOBS_RETURNED+1,MIN(NOBS_RETURNED+MAX_NOBS_USER,MOST_NOBS)

! Then get real values from appropriate model array.

        MODEL=0                       ! First values from first model.
        DO J=1,NELEMS_USER            ! Loop round user's elements.
          IF (MODEL.EQ.0 .OR.
     &        J-MAP(MODEL).GT.NELEMS(MODEL)-NSORT_COORDS) THEN
            MODEL=MODEL+1             ! Next model

! If this is not the model with the biggest NOBS, compare sort
! coordinates with those for that model & assume no data from
! this model if they're not equal.
! (Unless there's no data at all, in which case skip it anyway)

            IF (NOBS(MODEL).EQ.0) THEN  ! If no data from this model,
              SKIP=.TRUE.             ! skip it.
            ELSE
              SKIP=.FALSE.            ! (If no data, skip it.)
              IF (MODEL.NE.MOST_NOBS_MODEL .AND. MODEL.LE.NMODELS) THEN
                KOORD=1               ! Has model got data for this ob?
                DO WHILE (.NOT.SKIP .AND. KOORD.LT.NSORT_COORDS)
                  IF (A(IOB(MODEL)*NELEMS(MODEL)+KOORD,MODEL).GT.
     &           A((I-1)*NELEMS(MOST_NOBS_MODEL)+KOORD,MOST_NOBS_MODEL))
     &              SKIP=.TRUE.
                  KOORD=KOORD+1       ! next coordinate to check
                ENDDO                 ! End of loop round coordinates
              ENDIF                   ! SKIP now set for this ob & model
            ENDIF
            IF (.NOT.SKIP) IOB(MODEL)=IOB(MODEL)+1

! Get I-th ident etc from unsorted array using pointer in sorted real
! array.  (Assume character elements only retrieved from first model)

            IF (MODEL.EQ.1) THEN
              IF (.NOT.SKIP) THEN
                CSTR(I-NOBS_RETURNED)=
     &                IDENT(A((I-1)*NELEMS(MODEL)+4,MODEL))
              ELSE
                CSTR(I-NOBS_RETURNED)='         '
              ENDIF
            ENDIF
          ENDIF                       ! Coped with change of model

! Set J-th value for I-th ob in output array.

          IF (.NOT.SKIP) THEN         ! If model has data for this ob...
            IJ=(IOB(MODEL)-1)*NELEMS(MODEL)+NSORT_COORDS+J-MAP(MODEL)
            OUT(MAX_NOBS_USER*(J-1)+I-NOBS_RETURNED)=A(IJ,MODEL)
          ELSE
            OUT(MAX_NOBS_USER*(J-1)+I-NOBS_RETURNED)=-9999999.
          ENDIF
        ENDDO                         ! end of loop round elements
      ENDDO                           ! end of loop round batch of obs

!**********************************************************************
! Finally set the return code for the user & return.
! There may be more obs for this hour than room in the user's array.
! If so, return MAX_NOBS_USER obs at a time.
! Set NOBS for user (either full array or last few obs for this hour).
! Adjust NOBS_RETURNED by this number or reset it to zero for next hour.
! If no more data & no more hours, set ONEHOUR_RC as it was at the start
! to allow further calls to MDBX with different requests.

      NOBS_USER=MIN(MAX_NOBS_USER,MOST_NOBS-NOBS_RETURNED)
      IF (NOBS_USER.LE.MOST_NOBS) THEN
        NOBS_RETURNED=NOBS_RETURNED+MAX_NOBS_USER
      ENDIF

      IF (NOBS_RETURNED.LT.MOST_NOBS) THEN
        IX=4                          ! 4 if more data for this hour
      ELSE
        IX=ONEHOUR_RC                 ! 4 if more hours to do, 0 if not
        NOBS_RETURNED=0               ! reset for next hour
        MOST_NOBS=0                   ! reset for DO round hours
        IF (ONEHOUR_RC.EQ.0) ONEHOUR_RC=-1 ! as when MDBX first called
      ENDIF
      RETURN
      END
