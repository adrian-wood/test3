      SUBROUTINE MDBXREQ(REQ,REQS,NELEMS,NMODELS,
     &                   NREQ_ELEMS,NSORT_COORDS)
      IMPLICIT NONE

!----------------------------------------------------------------------
!
! PROGRAM       : MDBXREQ
!
! PURPOSE       : To split a request with MODEL in the ELEMENTS list
!                 into requests for the various models concerned.
!
! DATA TYPES    : any retrievable by MDB
!
! CALLED BY     : MDBX
!
! CALLS         :
!
! PARAMETERS    : (1) input request
!                 (2) output requests
!                 (3) number of elements in each output request
!                 (4) number of models & hence output requests
!                 (5) number of elements found in input request
!                      (returned for checks)
!
!----------------------------------------------------------------------
      INTEGER   NELEMS(*)             ! NELEM (for MDB) for each model
      INTEGER   NMODELS               ! actual number of models
      INTEGER   NREQ_ELEMS            ! number of elements found in REQ
      INTEGER   NSORT_COORDS          ! number of coords for sorting
      INTEGER   MAX_NELEMS            ! max element count (MASK size)
      INTEGER   MAX_NOBS              ! max number of obs from MDB

      INTEGER   I                     ! pointer to input request
      INTEGER   J                     ! short-term pointer to input req
      INTEGER   ELEMENT_START         ! start of ELEMENTS in user's req
      INTEGER   M                     ! pointer to single-model request
      INTEGER   LSORT_COORDS          ! length of coord list for sort
      INTEGER   REPL_START            ! points to 1st replicated element
      INTEGER   FIRST_MODEL_START     ! points to any MODEL before ELEME
      INTEGER   MODEL_END             ! points to end of model in output
      INTEGER   LFIRST_MODEL          ! length of MODEL before ELEMENTS
      INTEGER   LNEXT_MODEL           ! length of MODEL after ELEMENTS
      INTEGER   L                     ! length between model & elements
      INTEGER   MODEL                 ! model subscript (1,NMODEL)
      INTEGER   NREPL_ELEMS           ! number of values replicated
      INTEGER   NTIMES                ! number of times replicated
      INTEGER   NELEMS_USER           ! keep user's NELEM for check
                                      !  after splitting up request
      CHARACTER REQ*(*)               ! input request
      CHARACTER*(*) REQS(*)           ! requests for different models
      CHARACTER SORT_COORDS*500       !

! Start each element list with sort coordinates.
! (Double space marks end of SORT_COORDS; MINT retrieved twice to
!  leave slot in array to be filled by sequence number for sort.
!  This initial version is to be used operationally for ships;
!  the code is meant to be general, but e.g AMDARs may need extra
!  coordinates for the sort to give repeatable results.)
! for AMDARs: ??      SORT_COORDS='ELEMENTS LTTD LNGD MINT SCND ALTD  '

      SORT_COORDS='ELEMENTS LTTD LNGD MINT MINT  '
      LSORT_COORDS=INDEX(SORT_COORDS,'  ')
      NSORT_COORDS=4

! The DO block below makes requests for the different models involved.
! Everything before ELEMENTS goes into each request.
! If MODEL is found among the elements, then a new request is started,
!  with MODEL put before ELEMENTS.
! Element names are transferred letter by letter, and elements are
!  counted, allowing for replications of single names (no brackets)
!  & bracketed sets of elements, but assuming no entities (names
!  corresponding to more than one element) or nested replications.

      ELEMENT_START=INDEX(REQ,' ELEMENTS ')
      FIRST_MODEL_START=INDEX(REQ(:ELEMENT_START),' MODEL ')

      MODEL=1
      REQS(MODEL)=REQ(1:ELEMENT_START)
      REQS(MODEL)(ELEMENT_START+1:)=SORT_COORDS
      NELEMS(MODEL)=NSORT_COORDS      ! NELEM for first request

      NREQ_ELEMS=0                    ! to count input elements
      M=ELEMENT_START+LSORT_COORDS
      I=ELEMENT_START+9               ! start at space after ELEMENTS

! Loop round characters of element list in user's request,
! copying each to a request for a single model.

      DO WHILE (I.LT.LEN(REQ) .AND. M.LE.LEN(REQS(1)))
        REQS(MODEL)(M:M)=REQ(I:I)

! If MODEL is found in the element list, start another model request.

        IF (REQ(I:I+6).EQ.' MODEL ') THEN
          MODEL=MODEL+1               ! next model/request
          REQS(MODEL)=REQ(1:ELEMENT_START)
          LNEXT_MODEL=INDEX(REQ(I+7:),' ')

! If there was a MODEL keyword before ELEMENTS, overwrite it with
! the new model; otherwise just put MODEL before ELEMENTS.

          IF (FIRST_MODEL_START.GT.0) THEN
            LFIRST_MODEL=
     &                INDEX(REQ(FIRST_MODEL_START+8:ELEMENT_START),' ')
            MODEL_END=
     &               FIRST_MODEL_START+8+MAX(LFIRST_MODEL,LNEXT_MODEL)
            REQS(MODEL)(FIRST_MODEL_START:MODEL_END)=
     &                               REQ(I:I+7+LNEXT_MODEL)

! If the new model name is longer, anything between it and ELEMENTS
! (already copied to REQS) must be copied again to follow it.

            IF (LNEXT_MODEL.GT.LFIRST_MODEL) THEN
              L=ELEMENT_START-(FIRST_MODEL_START+8+LFIRST_MODEL)
              IF (L.GT.0) THEN
                REQS(MODEL)(MODEL_END+1:MODEL_END+L)
     &          =REQ(ELEMENT_START-L:ELEMENT_START-1)
              ENDIF
            ENDIF

! If the first model was the default, just add MODEL before ELEMENTS.

          ELSE
            REQS(MODEL)(ELEMENT_START+1:ELEMENT_START+7+LNEXT_MODEL)=
     &                               REQ(I:I+7+LNEXT_MODEL)
          ENDIF

! Again start the element list with the sort coordinates.

          REQS(MODEL)(ELEMENT_START+7+LNEXT_MODEL+1:)=SORT_COORDS
          M=ELEMENT_START+7+LNEXT_MODEL+LSORT_COORDS-1
          I=I+7+LNEXT_MODEL-2
          NELEMS(MODEL)=NSORT_COORDS

! If a space is followed by a non-space, this is the start of a name,
! so increment the number of elements.

        ELSE IF (REQ(I:I).EQ.' ' .AND. REQ(I+1:I+1).NE.' ') THEN
          NREQ_ELEMS=NREQ_ELEMS+1     ! one more input element
          NELEMS(MODEL)=NELEMS(MODEL)+1

! Brackets imply replication.  Assuming no nested replication,
! get count after brackets & adjust number of elements using it
! and number of names between the brackets.
! Replication (of a single element) is also possible without brackets.

        ELSE IF (REQ(I:I).EQ.'(') THEN
          REPL_START=NREQ_ELEMS       ! first replicated element
        ELSE IF (REQ(I:I).EQ.')') THEN
          NREPL_ELEMS=NREQ_ELEMS-REPL_START+1
        ELSE IF (REQ(I:I).EQ.'*') THEN           ! count follows
          IF (REQ(I-1:I-1).NE.')') NREPL_ELEMS=1 ! *n after one name    e

! Find number of times figure by figure from count after *

          NTIMES=0
          J=I+1                       ! point to first figure after *
          DO WHILE (REQ(J:J).GE.'0' .AND. REQ(J:J).LE.'9')
            NTIMES=NTIMES*10+ICHAR(REQ(J:J))-ICHAR('0')
            J=J+1
          ENDDO

! Update input & output element counts.  Number of elements replicated
! was either set when ')' found, or set to 1 when * found without ')'.

          NREQ_ELEMS=NREQ_ELEMS+NREPL_ELEMS*(NTIMES-1)
          NELEMS(MODEL)=NELEMS(MODEL)+NREPL_ELEMS*(NTIMES-1)
        ENDIF
        I=I+1                         ! past input character
        M=M+1                         ! & output
      ENDDO
      NMODELS=MODEL

! If the model request strings declared in this program are too short
! for one of the models, the above loop will end prematurely.  Print
! an error message calling for longer strings.

      IF (M.GT.LEN(REQS(1))) THEN
        PRINT *,'MDBX request string too short for model',MODEL
        STOP
      ENDIF
      RETURN
      END
