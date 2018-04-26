SUBROUTINE SKLSUBN(IDESC,NELEM,LMINDEX,NINDEX,QCREQ, &
     SEGMENT,IVAL,IDSC,NELREQ,LFLAG)

!-----------------------------------------------------------------------
!
! subroutine    : SKLSUBN - MDB Retrieval
!
! purpose       : prepare table of array subscripts relative to segment
!                 starts for given list of elements (& nested sequence)
!
! description   : element list is one-to-one with users array EXCEPT
!                 if user asks for QCFLAGS.  The output table is always
!                 one-to-one with users array.
!
! called by     : BUFINDX
!
! calls         : nothing
!
! arguments     : IDESC    (ip) : array of users element numbers
!               : NELEM    (ip) : no. of elements in list
!               : LMINDEX  (ip) : array of index entries
!               : NINDEX   (ip) : no. of index entries
!               : QCREQ    (ip) : true if QC flags wanted for each elem
!                 The next 3 arrays make up the index table:
!               : SEGMENT  (op) : segment number (-999 if not found)
!               : IVAL     (op) : subscript of data value
!               : IDSC     (op) : subscript of descriptor
!               : NELREQ   (op) : no of elements in output table
!               : LFLAG    (ip) : true for diagnostics
!
! written by    : S.M.Needham - based on SKLDSP
!
! REVISION INFO :
!
! CHANGE RECORD :
! $Log:
!  6    MetDB_Refresh 1.5         18/11/2010 12:03:13    Stan Kellett
!       Changed copyright to 2010
!  5    MetDB_Refresh 1.4         11/11/2010 11:45:57    Richard Weedon  rework
!        completed
!  4    MetDB_Refresh 1.3         02/11/2010 14:25:55    Richard Weedon
!       removed old change information
!  3    MetDB_Refresh 1.2         28/10/2010 16:30:10    Richard Weedon
!       Updated
!  2    MetDB_Refresh 1.1         25/10/2010 10:15:22    Richard Weedon
!       updated
!  1    MetDB_Refresh 1.0         22/10/2010 16:47:44    Richard Weedon
!       initial version
! $
!
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER,INTENT(IN)            ::  NELEM
INTEGER,INTENT(IN)            ::  IDESC(NELEM)
! INTEGER                     ::  IREPL(NELEM)
INTEGER,INTENT(IN)            ::  NINDEX
INTEGER,INTENT(OUT)           ::  SEGMENT(:)
INTEGER,INTENT(OUT)           ::  IVAL(:)
INTEGER,INTENT(OUT)           ::  IDSC(:)
INTEGER,INTENT(OUT)           ::  NELREQ

INTEGER                       ::  QCWID
INTEGER                       ::  I,J
INTEGER                       ::  NDEX
INTEGER                       ::  NN
INTEGER                       ::  MSEG
INTEGER                       ::  LASTONE
INTEGER                       ::  DIRN

LOGICAL                       ::  FOUND
LOGICAL,INTENT(IN)            ::  LFLAG
LOGICAL,INTENT(IN)            ::  QCREQ

CHARACTER(LEN=*),INTENT(IN)   :: LMINDEX(:)   !- element index array
CHARACTER(LEN=6)              :: ELMNUM


! First see if q/c flags are wanted and are available: QCWID=0 if there
! are no QC bits - or if there are, but the user doesn't want them.

QCWID=0
NDEX=NINDEX
IF (LMINDEX(NINDEX)(1:8) == 'QC_FLAGS') THEN
  NDEX=NINDEX-1
  IF (QCREQ) QCWID=-1
END IF

! Loop round elements in user's request

NELREQ=0        ! number of elements found in request
LASTONE=0       ! last element number found
NN=0

DO I=1,NELEM
  WRITE (ELMNUM,'(I6)') IDESC(I)

! Find the element number in the index.
! In case elements are not in numerical order in both index & request.
! compare next element number with that just found & look backwards or
! forwards accordingly.

  IF (IDESC(I) >  LASTONE) THEN
    DIRN=1
  ELSE IF (IDESC(I) == LASTONE) THEN
    DIRN=0
  ELSE
    DIRN=-1
  END IF

  J=0
  FOUND=.FALSE.
  DO WHILE (.NOT.FOUND .AND. J <  NDEX)
    J=J+1

    NN=NN+DIRN
    IF (NN >  NDEX) THEN
      NN=1
    ELSE IF (NN < 1) THEN
      NN=NDEX
    END IF
    IF_CONSTR1 : &
    IF (ELMNUM == LMINDEX(NN)(1:6)) THEN
      FOUND=.TRUE.

! Got it, so read rest of entry to set up a row of the output table.
! (2 lines per element, q/c flags first, if q/c flags are wanted.

      IF (QCWID /= 0) THEN
        NELREQ=NELREQ+2
      ELSE
        NELREQ=NELREQ+1
      END IF

      READ (LMINDEX(NN)(8:),*) SEGMENT(NELREQ),IVAL(NELREQ),      &
           IDSC(NELREQ)

! Not all segment numbers have QC elements attached. e.g. -99 report
! text, 99 trailer, -n special elements, so set missing data for them.

      IF (QCWID /= 0) THEN
        MSEG=SEGMENT(NELREQ)
        IF (MSEG >  0 .AND. MSEG /= 99) THEN
          SEGMENT(NELREQ-1)=SEGMENT(NELREQ)
          IVAL(NELREQ-1)=IVAL(NELREQ)-1
          IDSC(NELREQ-1)=IDSC(NELREQ)-1
        ELSE
          SEGMENT(NELREQ-1)=-999
        END IF
      END IF

      LASTONE=IDESC(I)
    END IF IF_CONSTR1
  END DO

! If element not found, print error message & set missing data.

  IF (.NOT.FOUND) THEN
    IF (LFLAG) PRINT *,'SKLSUBN did not find element',IDESC(I)

    IF (QCWID == 0) THEN
      NELREQ=NELREQ+1
      SEGMENT(NELREQ)=-999
    ELSE
      NELREQ=NELREQ+2
      SEGMENT(NELREQ)=-999
      SEGMENT(NELREQ-1)=-999
    END IF
  END IF
END DO

IF (LFLAG) THEN
  PRINT *,'SKLSUBN found',NELREQ,'elements in request'
  PRINT *,'SKLSUBN: segment number, value & descriptor subscripts'
  DO I=1,NELREQ
    PRINT *,SEGMENT(I),IVAL(I),IDSC(I)
  END DO
END IF

RETURN
END SUBROUTINE SKLSUBN
