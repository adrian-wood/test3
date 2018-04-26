SUBROUTINE SKLDSP(IDESC,NELEM,IREPL,LMINDEX,NINDEX,MESTRUCT,    &
                  QCREQ,SEGMENT,IBEFOR,WIDTH,SCALE,REFVAL,      &
                  NELREQ,NSEG,NWID,NREPLEN,MAXELM,MAXSEG,LFLAG, &
                  BINMAX)

!-----------------------------------------------------------------------
!
! ROUTINE       : SKLDSP
!
! PURPOSE       : prepare table of displacements & table b details
!               : for values in bufr message from user's element list
!
! DESCRIPTION   : element list is one-to-one with users array EXCEPT
!               : if user asks for QCFLAGS.  The output table is always
!               : one-to-one with users array.
!               : If an element is not found, return -999 in the segment
!               : array.
!
! CALLED BY     : BITINDX
!
! CALLS         : nothing
!
! ARGUMENTS     : IDESC    (ip) : array of users element numbers
!               : NELEM    (ip) : no. of elements in list
!               : IREPL    (ip) : counts to go with IDESC
!               : LMINDEX  (ip) : array of index entries
!               : NINDEX   (ip) : no. of index entries
!               : MESTRUCT (ip) : message structure
!               : QCREQ    (ip) : true if QC flags wanted for each elem
!             The next 5 arrays make up the index table
!               : SEGMENT  (op) : segment number
!               : IBEFOR   (op) : bits before element in this segment
!               : WIDTH    (op) : bit width
!               : SCALE    (op) : BUFR scale
!               : REFVAL   (op) : BUFR reference value
!               : NELREQ   (op) : no of elements in output table
!             The next 3 variables are describe the segment structure
!               : NSEG     (op) : no of segments
!               : NWID     (op) : width of count fields
!               : NREPLEN  (op) : segment lengths
!               : MAXELM   (ip) : max size of index table
!               : MAXSEG   (ip) : max size of segment table
!               : LFLAG    (ip) : true for diagnostics
!               : BINMAX   (ip) : size of LMINDEX
!
! REVISION INFO :
!
! $Workfile: skldsp.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 10/11/2010 15:02:07$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         10/11/2010 15:02:07    Brian Barwell   Large
!       DO and IF blocks labelled after review.
!  1    MetDB_Refresh 1.0         04/11/2010 15:55:50    Brian Barwell   MetDB
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

INTEGER,       INTENT(IN)  ::  NELEM
INTEGER,       INTENT(IN)  ::  IDESC(NELEM)
INTEGER,       INTENT(IN)  ::  IREPL(NELEM)
INTEGER,       INTENT(IN)  ::  BINMAX
CHARACTER(*),  INTENT(IN)  ::  LMINDEX(BINMAX)
INTEGER,       INTENT(IN)  ::  NINDEX
CHARACTER(*),  INTENT(IN)  ::  MESTRUCT
LOGICAL,       INTENT(IN)  ::  QCREQ
INTEGER,       INTENT(IN)  ::  MAXELM
INTEGER,       INTENT(OUT) ::  SEGMENT(MAXELM)
INTEGER,       INTENT(OUT) ::  IBEFOR(MAXELM)
INTEGER,       INTENT(OUT) ::  WIDTH(MAXELM)
INTEGER,       INTENT(OUT) ::  SCALE(MAXELM)
INTEGER,       INTENT(OUT) ::  REFVAL(MAXELM)
INTEGER,       INTENT(OUT) ::  NELREQ
INTEGER,       INTENT(OUT) ::  NSEG
INTEGER,       INTENT(IN)  ::  MAXSEG
INTEGER,       INTENT(OUT) ::  NWID(MAXSEG)
INTEGER,       INTENT(OUT) ::  NREPLEN(MAXSEG)
LOGICAL,       INTENT(IN)  ::  LFLAG

!-----------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------

INTEGER  ::  DIRN
INTEGER  ::  I
INTEGER  ::  INDEX(999)
INTEGER  ::  J
INTEGER  ::  LASTONE
INTEGER  ::  MSEG
INTEGER  ::  NDEX
INTEGER  ::  NN
INTEGER  ::  QCWID

!-----------------------------------------------------------------------
! Dynamic common
!-----------------------------------------------------------------------

COMMON /SKLDSP1/INDEX

!-----------------------------------------------------------------------
! First see if q/c flags are wanted and are available: QCWID=0 if there
! are no QC bits or there are, but the user does not want them.
!-----------------------------------------------------------------------

IF (LFLAG) WRITE(*,*)'In SKLDSP: QCREQ = ',QCREQ
QCWID=0
NDEX=NINDEX
IF (LMINDEX(NINDEX)(1:8) == 'QC_FLAGS') THEN
  NDEX=NINDEX-1
  IF (QCREQ) THEN
    READ (LMINDEX(NINDEX)(9:),*) QCWID
  END IF
END IF

!-----------------------------------------------------------------------
! Extract element numbers from LMINDEX string into INDEX array.
!-----------------------------------------------------------------------

DO I=1,NDEX
  READ(LMINDEX(I)(1:6),*)INDEX(I)
END DO
READ (MESTRUCT,*) NSEG,(NWID(I),NREPLEN(I),I=1,NSEG)
IF (LFLAG) THEN
  WRITE(*,*)'In SKLDSP : MESTRUCT = '
  WRITE(*,'(I5,/(2I6))')NSEG,(NWID(I),NREPLEN(I),I=1,NSEG)
END IF

NELREQ=0        ! no. of elements required
LASTONE=0       ! index of previous element found
NN=0

!-----------------------------------------------------------------------
! Loop round names in users request
!-----------------------------------------------------------------------

ELEM_LOOP: &
DO I=1,NELEM

!-----------------------------------------------------------------------
! Now look up the name in the index, where a value name is followed by
! 5 numbers to complete a line in the table.
! Assume the elements are in numerical order of index number, in both
! the index and the request.  Compare this element number with the found
! previously to search forward or backward through the index.
!-----------------------------------------------------------------------

  IF (IDESC(I) > LASTONE) THEN
    DIRN=1
  ELSEIF (IDESC(I) == LASTONE) THEN
    DIRN=0
  ELSE
    DIRN=-1
  END IF

INDEX_LOOP: &
  DO J=1,NDEX
    NN=NN+DIRN
    IF (NN > NDEX) THEN
      NN=1
    ELSEIF (NN < 1) THEN
      NN=NDEX
    END IF
    
GOT_INDEX: &
    IF (IDESC(I) == INDEX(NN)) THEN

!-----------------------------------------------------------------------
! Got it, so set up a row of the output table
!-----------------------------------------------------------------------
! If q/c flags are wanted, there are 2 lines per element, the q/c flags
! coming first (from a fixed number of bits in the same segment).
!-----------------------------------------------------------------------

      IF (QCREQ) THEN
        NELREQ=NELREQ+2
      ELSE
        NELREQ=NELREQ+1
      END IF

      READ (LMINDEX(NN)(8:),*) SEGMENT(NELREQ),IBEFOR(NELREQ), &
            WIDTH(NELREQ),SCALE(NELREQ),REFVAL(NELREQ)
      IF (IREPL(I) > 1) THEN
        IBEFOR(NELREQ)=IBEFOR(NELREQ)+ &
              (IREPL(I)-1) * NREPLEN(SEGMENT(NELREQ))
      END IF

!----------------------------------------------------------------------
! Not all segment numbers have QC elements attached. e.g. -99 report
! text, 99 trailer, -n special elements. !A An additional check has
! been added. TOR elements are from BUFR section 1, and so have a
! segment no. of 1. If the number of bits before the last element is
! less than 200, the element is not from BUFR section 4, e.g. TOR!
! The above check (!A) should only be done for segment 1, because
! only segment 1 has values from both sections 1 & 4 of a message.
!----------------------------------------------------------------------

      IF (QCREQ) THEN
        IF (QCWID > 0) THEN
          MSEG=SEGMENT(NELREQ)
          IF (MSEG > 1 .AND. MSEG /= 99 .OR.   &
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
      END IF

      LASTONE=IDESC(I)

      GO TO 100      ! next users element
    END IF GOT_INDEX
  END DO INDEX_LOOP

!-----------------------------------------------------------------------
! If a name can't be found, print an error message and carry on with
! the elements recognised so far.
!-----------------------------------------------------------------------

  IF (LFLAG) THEN
    WRITE(*,*)'MDB WARNING: In SKLDSP: DID NOT FIND ELEMENT ',IDESC(I)
  END IF

  IF (.NOT.QCREQ) THEN
    NELREQ=NELREQ+1
    SEGMENT(NELREQ)=-999
  ELSE
    NELREQ=NELREQ+2
    SEGMENT(NELREQ)=-999
    SEGMENT(NELREQ-1)=-999
  END IF

100   CONTINUE
END DO ELEM_LOOP

IF (LFLAG) THEN
  WRITE(*,*)'In SKLDSP: END OF SKLDSP, NELREQ= ',NELREQ
  WRITE(*,*)'In SKLDSP: SEGMENT, IBEFOR, WIDTH, SCALE, REFVAL = '
  WRITE(*,'(5I8)')(SEGMENT(I),IBEFOR(I),WIDTH(I),SCALE(I), &
                  REFVAL(I),I=1,NELREQ)
END IF

RETURN
END SUBROUTINE SKLDSP
