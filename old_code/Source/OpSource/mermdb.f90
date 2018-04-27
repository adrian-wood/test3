SUBROUTINE MERMDB(DATA,MODE,REQUESTS,LMLIST,NMDB,MAXNOBS,&
                  NOBSMDB,DBDATA,DIM,CSTR,CSTRDIM,REPTXT,IVERT,MDBRC)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                     !
! PROGRAM       : MERMDB                                              !
!                                                                     !
! PURPOSE       : To get more MDB data for a merge,                   !
!                 MDB is called with request(s) in the input string   !
!                 (which are stored for use later by the sweep).      !
!                  For a sweep, MDB is called with any requests for   !
!                 the period concerned which were stored by earlier   !
!                 merges (with 'RECEIVED BEFORE' changed to 'RECEIVED !
!                 AFTER') or requests without cutoffs for any times   !
!                 when no merge was done.                             !
!                  (Requests with no cutoff are used to sweep LNDSYN  !
!                 because its merge only retrieves for a subperiod.)  !
!                                                                     !
!                  The sweep is for a 24-hour period.  Assume the     !
!                 merge is submitted daily some time after 23Z: at    !
!                 any time in the 24-hour period 2300-2259 it will    !
!                 sweep the 24-hour period ending 2059 on the day     !
!                 before the 23Z above.                               !
!                  (If a sweep is needed for an earlier 24-hour       !
!                 period after an outage, it should be possible       !
!                 to run with an appropriate date/time in a DATIM     !
!                 subroutine to override the system clock.)           !
!                                                                     !
! DATA TYPE(S)  : all those merged                                    !
!                                                                     !
! CALLED BY     : MERGE                                               !
!                                                                     !
! CALLS         : MDB, MEREQS                                         !
!                                                                     !
! PARAMETERS    : (1) data type                                    (i)!
!                 (2) merge/sweep flag                             (i)!
!                 (3) string of requests, only input if merge      (i)!
!                 (4) element list                                 (i)!
!                 (5) number of elements in list                   (i)!
!                 (6) max number of obs in retrieval arrays        (i)!
!                 (7) number of obs retrieved                     (i/o)
!                 (8) data array for MDB call                      (o)!
!                 (9) character strings for MDB call               (o)!
!                (10) character string for LNDSYN (9-groups)       (o)!
!                (11) subscript of SATOB flag used to select obs   (i)!
!                (12) return code (4 if more data to come,         (o)!
!                                  0 if no more)                      !
!                                                                     !
! CHANGE RECORD : OPERATIONAL FROM ........                           !
!  06/02/98 - Make LNDSYN sweep do all obs, not just merge period    !a
!  17/08/98 - Make sweep cover the same 24 hours starting any time   !b
!              in a 24-hour period                                    !
!  21/09/98 - Avoid putting MAXNOBS in the last message swept!     !1.2
!             Store request after data has been merged (in case    !1.2
!             merge fails!) rather than before                        !
!                                                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$Log:
! 2    MetDB_Refresh 1.1         02/06/2011 14:38:18    Sheila Needham  Tidied
!      up
! 1    MetDB_Refresh 1.0         11/04/2011 14:54:22    Sheila Needham  Initial
!       F77 versions
!$
! Revision 1.4  2001/01/08  14:30:02  14:30:02  usmdb (Generic MDB account)
! 22 Jan 2001    C Long
! 1.4  Don't start new MDB call with DBFLAG=8
! 
! Revision 1.3  99/07/12  16:06:47  16:06:47  usmdb (Generic MDB account)
! Keep the 9-groups (if any) as a character string with the merged message
!
! Revision 1.2  98/09/17  10:44:41  10:44:41  usmdb (Generic MDB account)
! Avoid putting MAXNOBS in the last message swept!
!
USE zpdate_mod
USE datim_mod
USE mdb_mod
USE mereqs_mod
IMPLICIT  NONE

CHARACTER(*) :: DATA
CHARACTER(*) :: MODE
CHARACTER(*) :: REQUESTS
CHARACTER(*) :: LMLIST
INTEGER   :: NMDB
INTEGER   :: MAXNOBS
INTEGER   :: NOBSMDB
INTEGER   :: DIM
REAL      :: DBDATA(DIM)
INTEGER   :: CSTRDIM
CHARACTER(*)  :: CSTR(CSTRDIM)
CHARACTER*(*) :: REPTXT
INTEGER   :: IVERT
INTEGER   :: MDBRC
!
! Local variables
!
INTEGER,PARAMETER   :: AMSTAR=3
INTEGER   :: CENDAY
CHARACTER(1000):: CREP(1)   ! for synops
CHARACTER(1):: CREPO(1000)  ! for other types
INTEGER   :: DBFLAG=0
INTEGER   :: ENDDAY
INTEGER   :: ENDHOUR
INTEGER   :: ENDMONTH
INTEGER   :: ENDYEAR
INTEGER   :: HOURS_BACK
INTEGER   :: INDEXHR          ! end of sweep period
INTEGER   :: IREQ
INTEGER   :: I1,I2,II
INTEGER   :: N1,N2
LOGICAL   :: NEW_REQUEST=.FALSE.
INTEGER   :: NOW(8)
INTEGER   :: REQRC
INTEGER   :: REQUEST_LENGTH
INTEGER   :: REQUEST_SLASH
INTEGER   :: REQUEST_START=0
CHARACTER(50) :: SKELREQ
CHARACTER(5000) :: CREQ   ! local version of request string
INTEGER   :: STARTIME(4)
REAL, ALLOCATABLE :: WORK(:,:)
INTEGER,PARAMETER   :: XHOURS=6
CHARACTER(14):: YMDHM     ! date/time group (start) from request

DATA SKELREQ/'START TIME yyyymmdd/hh00Z END TIME yyyymmdd/hh59Z '/

SAVE

! If more MDB data is needed, call MDB for up to NOBSMAX soundings.
! (MAXNOBS is from merge table, so e.g. =1 for U/A)
! There may be more than one request for the merge data, so
! look for a further request when a retrieval is completed.
! Store any request delimited for use by the sweep later.
! >>> Branch back to 10 either to reject SATOB or with new request <<<
! Dummy string for report text except for LNDSYN, to get 9-groups  !1.3


10    CONTINUE
IF (NEW_REQUEST.OR.DBFLAG.EQ.4) THEN                         !1.2
  NOBSMDB=MAXNOBS                           !1.2
  IF (DBFLAG.EQ.8) DBFLAG=0                                  !1.4
  N1=NOBSMDB
  N2=NMDB
  ALLOCATE(WORK(N1,N2))
  IF (DATA.EQ.'LNDSYN') THEN                                 !1.3
    IF (MAXNOBS.GT.1) THEN                                   !1.3
      PRINT *,' MERMDB: cannot retrieve more than one ob '   !1.3
      PRINT *,' at a time if LNDSYN',MAXNOBS,'asked for '    !1.3
      STOP                                                   !1.3
    ENDIF                                                    !1.3
    CREQ=REQUESTS(REQUEST_START:REQUEST_START+REQUEST_LENGTH-1)&
         //' MESSAGE '//LMLIST
    CALL MDB(DATA,                                  &
         CREQ,                                                 &
         WORK,NOBSMDB,NMDB,DBFLAG,CSTR,CREP)          !1.3
         REPTXT=CREP(1)
  ELSE                                                       !1.3

         CREQ=REQUESTS(REQUEST_START:REQUEST_START+REQUEST_LENGTH-1)&
         //' MESSAGE '//LMLIST
    CALL MDB(DATA,CREQ, &
         WORK,NOBSMDB,NMDB,DBFLAG,CSTR,CREPO)
  ENDIF                                                      !1.3
  II=1
  if (n1*n2 > DIM)print*,'MERMDB:DBDATA array too small'
  DO I1=1,N2
    DO I2=1,N1
      DBDATA(II)=WORK(I2,I1)
      II=II+1
    END DO
  END DO
  DEALLOCATE(WORK)
  NEW_REQUEST=.FALSE.
ENDIF

! For SATOBs the obs retrieved depend on the elements requested. The
! full element list gets more than current (July 97) model retrieval,
! so call MDB again when an unwanted section is returned.
! We want sections 2,3,4,5, but section 4 is stored without section
! number.  All sections except 7 have a vertical significance set, and
! 7 (RH) is the one we don't want, so use that as an unsatisfactory
! way of distinguishing between sections.

IF (DATA.EQ.'SATOB' .AND. DBFLAG.EQ.4) THEN
  IF (DBDATA((IVERT-1)*MAXNOBS+1).LT.0) GO TO 10
ENDIF

! If it's the first time or there's no more data to come from this
! request (DBFLAG=0 means data this time but no more to come!)
! (REQUEST_START should go from 0 to 1 the first time, then up by more
! if there's another request.  Don't store a request until looking for
! the next one, in case the merge fails: if no request has been stored
! the sweep will store the data without model values rather than only
! asking for data after the cutoff.)

IF (DBFLAG.GE.8 .OR. DBFLAG.EQ.0 .OR. NOBSMDB.EQ.0) THEN
  IF (MODE.EQ.'MERGE') THEN
    IF (REQUEST_START.GT.0) THEN                             !1.2
      REQUEST_SLASH=REQUEST_START+              &
                    INDEX(REQUESTS(REQUEST_START:),'/')-1

      YMDHM=REQUESTS(REQUEST_SLASH-8:REQUEST_SLASH+5)
      READ (YMDHM( 1: 4),'(I4)') STARTIME(1)
      READ (YMDHM( 5: 6),'(I2)') STARTIME(2)
      READ (YMDHM( 7: 8),'(I2)') STARTIME(3)
      READ (YMDHM(10:11),'(I2)') STARTIME(4)

      REQRC=-1       ! set return code to store request for sweep
      CALL MEREQS(STARTIME,REQUESTS(REQUEST_START:),   &
                  REQUEST_LENGTH,REQRC)
    ENDIF                                                    !1.2

    IREQ=INDEX(REQUESTS(REQUEST_START+1:),'START TIME')

    IF (IREQ.GT.0) THEN
      REQUEST_START=REQUEST_START+IREQ
      REQUEST_LENGTH=INDEX(REQUESTS(REQUEST_START:),' ELEMENT')
    ENDIF

    IF (IREQ.EQ.0 .OR. REQUEST_LENGTH.EQ.0) THEN
      MDBRC=0
      RETURN
    ENDIF

! For sweep: take current date/time, assume sweep is normally submitted
! before midnight (23mmZ), so change century-day to yesterday if hour<23
! (assuming job submitted late) and sweep a period ending 21Z on the day
! before that.  So first set century-hour to 21Z on that day & then
! go back in 6-hour steps to cover the sweep period.
! N.B. hour H on century-day D is century-hour (D-1)*24+H, so add 1
! after dividing century-hour by 24 to get back to century-day - and
! use (CENDAY-2)*24 to get a century-hour for the day before!

  ELSE IF (MODE.EQ.'SWEEP') THEN
    IF (NOW(8).EQ.0) THEN
      CALL DATIM(NOW)
      CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY) ! day,month,year
      IF (NOW(5).LT.23) CENDAY=CENDAY-1                        !b
      INDEXHR=(CENDAY-2)*24+21                                 !b
      HOURS_BACK=XHOURS
      REQRC=1 ! set return code to get first request for sweep
    ENDIF

! Get next sweep request for this time (1st if REQRC=1, next if REQRC>1.

    IF (HOURS_BACK.LE.24) THEN
      CALL DATE13((INDEXHR-HOURS_BACK)/24+1,    &
            STARTIME(3),STARTIME(2),STARTIME(1))
      STARTIME(4)=MOD(INDEXHR-HOURS_BACK,24)
      CALL MEREQS(STARTIME,REQUESTS,REQUEST_LENGTH,REQRC)

! Ignore the merge request for LNDSYN.  Land SYNOPs are merged for only
! a 1-hour or 3-hour period, but the sweep needs everything, so get all
! 6 hours' data & let the duplicate check reject what has been merged.

      IF (DATA.EQ.'LNDSYN') REQRC=0                            !a

! If there is no request for this time, make one up (no cutoff).
! (Start time is known, end time is XHOURS on less a minute.)
! If there are no more requests for this time, go back XHOURS.

      IF (REQRC.EQ.0) THEN                    ! no request at all
        REQUESTS=SKELREQ
        REQUEST_LENGTH=LEN(SKELREQ)

        WRITE (REQUESTS(12:15),'(I4.4)') STARTIME(1) ! year
        WRITE (REQUESTS(16:17),'(I2.2)') STARTIME(2) ! month
        WRITE (REQUESTS(18:19),'(I2.2)') STARTIME(3) ! day
        WRITE (REQUESTS(21:22),'(I2.2)') STARTIME(4) ! hour

        CALL DATE13((INDEXHR-HOURS_BACK+XHOURS-1)/24+1, &
                   ENDDAY,ENDMONTH,ENDYEAR)
        ENDHOUR=MOD(INDEXHR-HOURS_BACK+XHOURS-1,24)

        WRITE (REQUESTS(36:39),'(I4.4)') ENDYEAR
        WRITE (REQUESTS(40:41),'(I2.2)') ENDMONTH
        WRITE (REQUESTS(42:43),'(I2.2)') ENDDAY
        WRITE (REQUESTS(45:46),'(I2.2)') ENDHOUR
      ENDIF
      REQUEST_START=1
                                              ! if REQRC=0 or =1,
      IF (REQRC.LE.1) THEN                    ! no more after this
        HOURS_BACK=HOURS_BACK+XHOURS          ! back for next time
        REQRC=1 ! set return code to get first request for sweep
      ENDIF                                   ! if REQRC>1, goto10
    ELSE
      MDBRC=0            ! can't go back any further!
      RETURN             ! no more data to come
    ENDIF
  ENDIF

! Go back to MDB call with new request if there was no data this time
! (not if DBFLAG=0! - return last ob(s) & use new request next time)

  NEW_REQUEST=.TRUE.
  IF (DBFLAG.EQ.8 .OR. NOBSMDB.EQ.0) GO TO 10                 !1.2
ENDIF

MDBRC=4                  ! more data to come

RETURN
END SUBROUTINE MERMDB
