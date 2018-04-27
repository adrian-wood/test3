SUBROUTINE SKLSUB(IDESC,NELEM,IREPL,LMINDEX,NINDEX,MESTRUCT,     &
QCREQ,SEGMENT,IVAL,IDSC,NELREQ,NSEG,STYP,                        &
NVALEN,NDSLEN,MAXELM,MAXSEG,LFLAG)

!-----------------------------------------------------------------------
!
! subroutine    : SKLSUB - MDB Retrieval
!
! purpose       : prepare table of array subscripts relative to segment
!                 starts for a given list of elements.
!
! description   : element list is one-to-one with users array EXCEPT
!                 if user asks for QCFLAGS.  The output table is always
!                 one-to-one with users array.
!                 If an element is not found, return -999 in the segment
!                 array.
!
! called by     : BUFINDX
!
! calls         : nothing
!
! arguments     : IDESC    (ip) : array of users element numbers
!               : NELEM    (ip) : no. of elements in list
!               : IREPL    (ip) : counts to go with IDESC
!               : LMINDEX  (ip) : array of index entries
!               : NINDEX   (ip) : no. of index entries
!               : MESTRUCT (ip) : message structure
!               : QCREQ    (ip) : true if QC flags wanted for each elem
!             The next 3 arrays make up the index table
!               : SEGMENT  (op) : segment number
!               : IVAL     (op) : subscript of data value
!               : IDSC     (op) : subscript of descriptor
!               : NELREQ   (op) : no of elements in output table
!             The next 4 variables are describe the segment structure
!               : NSEG     (op) : no of segments
!               : STYP     (op) : type of segment (mandatory/optional)
!               : NVALEN   (op) : no of subscripts in values array
!               : NDSLEN   (op) : no of subscripts in descriptor array
!               : MAXELM   (ip) : max size of index table
!               : MAXSEG   (ip) : max size of segment table
!               : LFLAG    (ip) : true for diagnostics
!
! written by    : S.M.Needham - based on SKLDSP
!
!Y2K  26.06.1997  SKLSUB is Year 2000 compliant.
!
! revision info :
!
! change record :
!
! $Log:
!  5    MetDB_Refresh 1.4         11/11/2010 11:18:59    Richard Weedon
!       updated
!  4    MetDB_Refresh 1.3         02/11/2010 14:24:08    Richard Weedon
!       removed old change information
!  3    MetDB_Refresh 1.2         28/10/2010 16:29:57    Richard Weedon
!       Updated
!  2    MetDB_Refresh 1.1         25/10/2010 10:05:24    Richard Weedon
!       updated
!  1    MetDB_Refresh 1.0         22/10/2010 16:47:44    Richard Weedon
!       initial version
! $
!
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare arguments - those used as dimensions first
!-----------------------------------------------------------------------

INTEGER,INTENT(IN)            ::    NELEM
INTEGER,INTENT(IN)            ::    MAXELM
INTEGER,INTENT(IN)            ::    MAXSEG


INTEGER                       ::    IDESC(NELEM)
INTEGER                       ::    IREPL(NELEM)
CHARACTER(LEN=*),INTENT(IN)   ::    LMINDEX(:) !- ELEMENT INDEX ARRAY
INTEGER,INTENT(IN)            ::    NINDEX
CHARACTER(LEN=*),INTENT(IN)   ::    MESTRUCT
LOGICAL,INTENT(IN)            ::    QCREQ
INTEGER,INTENT(OUT)           ::    SEGMENT(MAXELM)
INTEGER,INTENT(OUT)           ::    IVAL(MAXELM)
INTEGER,INTENT(OUT)           ::    IDSC(MAXELM)
INTEGER,INTENT(OUT)           ::    NELREQ
INTEGER,INTENT(OUT)           ::    NSEG
INTEGER,INTENT(OUT)           ::    STYP(MAXSEG)
INTEGER,INTENT(OUT)           ::    NVALEN(MAXSEG)
INTEGER,INTENT(OUT)           ::    NDSLEN(MAXSEG)
LOGICAL,INTENT(IN)            ::    LFLAG

!----------------------------------------------------------------------
! local variables
!----------------------------------------------------------------------

INTEGER          ::    QCWID
INTEGER          ::    INDEX(500)   !- array of element numbers
INTEGER          ::    I,J
INTEGER          ::    NDEX
INTEGER          ::    NN
INTEGER          ::    MSEG
INTEGER          ::    LASTONE
INTEGER          ::    DIRN

!----------------------------------------------------------------------
! Dynamic common
!----------------------------------------------------------------------

COMMON/SKL1/INDEX

! CHARACTER*132 HEAD

! ----------------------------------------------------------------------
! first see if q/c flags are wanted and are available: QCWID=0 if there
! are no QC elements or there are, but the user doesn't want them.
! Assume the qc element is immediately before the data element.
! ----------------------------------------------------------------------

IF (LFLAG) WRITE(*,*)'In SKLSUB: QCREQ ',QCREQ
QCWID=0
NDEX=NINDEX
IF (LMINDEX(NINDEX)(1:8) == 'QC_FLAGS') THEN
  NDEX=NINDEX-1
  IF (QCREQ)THEN
    QCWID=-1
  END IF
END IF

! ----------------------------------------------------------------------
! extract element numbers from LMINDEX string into INDEX array.
! ----------------------------------------------------------------------

DO I=1,NDEX
  READ(LMINDEX(I)(1:6),*)INDEX(I)
END DO

READ (MESTRUCT,*) NSEG,(STYP(I),NVALEN(I),NDSLEN(I),I=1,NSEG)
IF (LFLAG) THEN
  WRITE(*,*)'In SKLSUB: MESTRUCT:'
  WRITE(*,'(I5,/(3I6))')NSEG,(STYP(I),NVALEN(I),NDSLEN(I),       &
  I=1,NSEG)
END IF

NELREQ=0        ! no. of elements required
LASTONE=0       ! index of previous element found
NN=0

! ----------------------------------------------------------------------
! loop round names in users request
! ----------------------------------------------------------------------
DO 100 I=1,NELEM

! ----------------------------------------------------------------------
! now look up the name in the index, where a value name is followed by
! 3 numbers to complete a line in the table.
! Assume the elements are in numerical order of index number, in both
! the index and the request.  Compare this element number with the found
! previously to search forward or backward through the index.
! ----------------------------------------------------------------------

  IF (IDESC(I) >  LASTONE) THEN
    DIRN=1
  ELSE IF (IDESC(I) == LASTONE) THEN
    DIRN=0
  ELSE
    DIRN=-1
  END IF

  DO 50 J=1,NDEX
    NN=NN+DIRN
    IF (NN >  NDEX) THEN
      NN=1
    ELSE IF (NN < 1) THEN
      NN=NDEX
    END IF
    IF (IDESC(I) == INDEX(NN)) THEN

! ----------------------------------------------------------------------
! Got it, so set up a row of the output table
! ----------------------------------------------------------------------
! if q/c flags are wanted, there are 2 lines per element, the q/c flags
! coming first
! ----------------------------------------------------------------------

      IF (QCWID == -1) THEN
        NELREQ=NELREQ+2
      ELSE
        NELREQ=NELREQ+1
      END IF

      READ (LMINDEX(NN)(8:),*) SEGMENT(NELREQ),IVAL(NELREQ),     &
       IDSC(NELREQ)
      IF(IREPL(I) >  1)THEN
        IVAL(NELREQ)=IVAL(NELREQ)+                               &
           (IREPL(I)-1) * NVALEN(SEGMENT(NELREQ))
      END IF

!----------------------------------------------------------------------
! Not all segment numbers have QC elements attached. e.g. -99 report
! text, 99 trailer, -n special elements.
!----------------------------------------------------------------------

      IF (QCWID == -1) THEN
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

      GOTO 100      ! next users element
    END IF
  50      CONTINUE

! ----------------------------------------------------------------------
! if a name can't be found, print an error message and carry on with
! the elements recognised so far.
! ----------------------------------------------------------------------

  IF (LFLAG) THEN                                               !C
  WRITE(*,*)'MDB WARNING: DID NOT FIND ELEMENT ',IDESC(I)
  END IF                                                        !C

  IF (QCWID == 0) THEN
    NELREQ=NELREQ+1
    SEGMENT(NELREQ)=-999
  ELSE
    NELREQ=NELREQ+2
    SEGMENT(NELREQ)=-999
    SEGMENT(NELREQ-1)=-999
  END IF

100   CONTINUE

IF (LFLAG) THEN
  WRITE(*,*)'END OF SKLSUB: NELREQ=',NELREQ
  WRITE(*,'(3I8)')(SEGMENT(I),IVAL(I),IDSC(I),I=1,NELREQ)
END IF

RETURN
END SUBROUTINE SKLSUB
