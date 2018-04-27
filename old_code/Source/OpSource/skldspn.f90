SUBROUTINE SKLDSPN(IDESC,NELEM,LMINDEX,NINDEX,QCREQ, &
                  SEGMENT,IBEFOR,WIDTH,SCALE,REFVAL,NELREQ,LFLAG)

!-----------------------------------------------------------------------
!
! ROUTINE       : SKLDSPN
!
! PURPOSE       : Prepare table of displacements & Table B details
!                 for values in BUFR message from user's element list
!                 and element index (allowing for nesting, unlike
!                 SKLDSP), leaving any replication to be handled
!                 by MSGDSPN.
!
! DESCRIPTION   : Find element number in index, add line to output
!                 table, add another line if QC bits.
!
! CALLED BY     : BITINDX (if there is nesting)
!
! CALLS         : nothing
!
! ARGUMENTS     : IDESC    (ip) : array of users element numbers
!               : NELEM    (ip) : no. of elements in list
!               : LMINDEX  (ip) : array of index entries
!               : NINDEX   (ip) : no. of index entries
!               : QCREQ    (ip) : true if QC flags wanted for each elem
!             The next 5 arrays make up the output table
!               : SEGMENT  (op) : segment number
!               : IBEFOR   (op) : bits before element in this segment
!               : WIDTH    (op) : bit width
!               : SCALE    (op) : BUFR scale
!               : REFVAL   (op) : BUFR reference value
!               : NELREQ   (op) : no of elements in output table
!               : LFLAG    (ip) : true for diagnostics
!
! REVISION INFO :
!
! $Workfile: skldspn.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 22/11/2010 15:39:34$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         22/11/2010 15:39:34    Stan Kellett    array
!       argument dimensions changed to (*)
!  2    MetDB_Refresh 1.1         10/11/2010 15:05:43    Brian Barwell   Large
!       DO and IF blocks labelled after review.
!  1    MetDB_Refresh 1.0         04/11/2010 15:56:37    Brian Barwell   MetDB
!       Refresh batch 11.  Files for review.
! $
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

!-----------------------------------------------------------------------
! Subroutine arguments
!-----------------------------------------------------------------------

INTEGER,      INTENT(IN)  :: IDESC(*)  ! element nos. (NOT descriptors!)
INTEGER,      INTENT(IN)  :: NELEM     ! input no. of elements requested
CHARACTER(*), INTENT(IN)  :: LMINDEX(*)
INTEGER,      INTENT(IN)  :: NINDEX    ! number of index entries
LOGICAL,      INTENT(IN)  :: QCREQ     ! set if QC bits requested
INTEGER,      INTENT(OUT) :: SEGMENT(*)
INTEGER,      INTENT(OUT) :: IBEFOR(*)
INTEGER,      INTENT(OUT) :: WIDTH(*)
INTEGER,      INTENT(OUT) :: SCALE(*)
INTEGER,      INTENT(OUT) :: REFVAL(*)
INTEGER,      INTENT(OUT) :: NELREQ    ! no. of elements in output table
LOGICAL,      INTENT(IN)  :: LFLAG     ! flag fof diagnostics printout

!-----------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------

INTEGER :: DIRN         ! forward or backward step in search
INTEGER :: I
INTEGER :: J
INTEGER :: LASTONE      ! last element number found
INTEGER :: MSEG         ! segment number for checks
INTEGER :: NDEX         ! NINDEX-1 if QC flags, NINDEX if not
!                              (NELREQ=NELEM (or NELEM*2 if QC flags)
!                               if all elements found in index)
INTEGER :: NN           ! index pointer in loop
INTEGER :: QCWID        ! number of QC bits per element

LOGICAL :: FOUND        ! set if element found in index

CHARACTER(6) :: ELMNUM  ! number in figures for index search

!----------------------------------------------------------------------
! First see if QC flags are wanted and are available: QCWID=0 if there
! are no QC bits - or if there are, but the user does not want them.
!----------------------------------------------------------------------

QCWID=0
NDEX=NINDEX
IF (LMINDEX(NINDEX)(1:8) == 'QC_FLAGS') THEN
  NDEX=NINDEX-1
  IF (QCREQ) READ (LMINDEX(NINDEX)(9:),*) QCWID
END IF
IF (LFLAG) PRINT *,'In SKLDSPN: QCREQ=',QCREQ,'  QCWID=',QCWID

!----------------------------------------------------------------------
! Loop round elements in user's request, first converting element
! number to figures for comparison with start of index entry.
! N.B. Initialise NN here (NOT just before the inner loop) to carry
!      on from where the last element was found.
!----------------------------------------------------------------------

NELREQ=0        ! number of elements found in request
LASTONE=0       ! last element number found
NN=0

ELEM_LOOP: &
DO I=1,NELEM
  WRITE (ELMNUM,'(I6)') IDESC(I)

!----------------------------------------------------------------------
! Find element number in index.
! In case elements are not in numerical order in both index & request,
! compare next element number with that just found & look backwards or
! forwards accordingly.
!----------------------------------------------------------------------

  IF (IDESC(I) > LASTONE) THEN
    DIRN=1
  ELSE IF (IDESC(I) == LASTONE) THEN
    DIRN=0
  ELSE
    DIRN=-1
  END IF

  J=0
  FOUND=.FALSE.
INDX_SEARCH: &
  DO WHILE (.NOT.FOUND .AND. J < NDEX)
    J=J+1

    NN=NN+DIRN
    IF (NN > NDEX) THEN
      NN=1
    ELSE IF (NN < 1) THEN
      NN=NDEX
    END IF
GOT_INDX: &
    IF (ELMNUM == LMINDEX(NN)(1:6)) THEN
      FOUND=.TRUE.

!----------------------------------------------------------------------
! Got it, so read rest of entry to set up a row of the output table
! (or two rows if QC flags are wanted & data has QC flags).
!----------------------------------------------------------------------

      IF (QCWID > 0) THEN
        NELREQ=NELREQ+2
      ELSE
        NELREQ=NELREQ+1
      END IF
      READ (LMINDEX(NN)(8:),*) SEGMENT(NELREQ),IBEFOR(NELREQ), &
                   WIDTH(NELREQ),SCALE(NELREQ),REFVAL(NELREQ)

!----------------------------------------------------------------------
! If q/c flags are wanted, there are 2 lines per element, the q/c flags
! coming first (from a fixed number of bits in the same segment).
!  Not all elements have QC bits, e.g. -99 (report text), 99 (trailer),
! -n ("special elements"), so set missing data for them.
!  TOR elements are from BUFR section 1, so segment number is 1.
! If there are <200 bits before the last element, it's not from
! BUFR section 4.  But only do this check for seqment 1, because
! only segment 1 has values from both sections 1 & 4 of a message.
!----------------------------------------------------------------------

      MSEG=SEGMENT(NELREQ)
      IF (QCWID > 0) THEN
        IF ((MSEG > 1 .AND. MSEG /= 99) .OR. &
            (MSEG == 1 .AND. IBEFOR(NELREQ) > 200)) THEN
          SEGMENT(NELREQ-1)=SEGMENT(NELREQ)
          IBEFOR(NELREQ-1)=IBEFOR(NELREQ)-QCWID
          WIDTH(NELREQ-1)=QCWID
          SCALE(NELREQ-1)=0
          REFVAL(NELREQ-1)=0
        ELSE
          SEGMENT(NELREQ-1)=-999
        END IF
      END IF

      LASTONE=IDESC(I)
    END IF GOT_INDX
  END DO INDX_SEARCH    ! end of loop round index entries

!----------------------------------------------------------------------
! If element not found, set missing data.
!----------------------------------------------------------------------

  IF (.NOT.FOUND) THEN
    IF (LFLAG) PRINT *,'SKLDSPN did not find element',IDESC(I)
    IF (QCWID == 0) THEN
      NELREQ=NELREQ+1
      SEGMENT(NELREQ)=-999
    ELSE
      NELREQ=NELREQ+2
      SEGMENT(NELREQ)=-999
      SEGMENT(NELREQ-1)=-999
    END IF
  END IF
END DO ELEM_LOOP   ! end of loop round elements requested

IF (LFLAG) THEN
  PRINT *,'SKLDSPN found',NELREQ,'elements in request'
  PRINT *,'In SKLDSPN: SEGMENT, IBEFOR, WIDTH, SCALE, REFVAL = '
  DO I=1,NELREQ
    PRINT *,SEGMENT(I),IBEFOR(I),WIDTH(I),SCALE(I),REFVAL(I)
  END DO
END IF

RETURN
END SUBROUTINE SKLDSPN
