SUBROUTINE SKLARR(IDESC,NELEM,IREPL,QCREQ,ELMNUM,SEGNUM,SUBNUM, &
                  NROWS,SEGMENT,IVAL,NELREQ,QCINC,NSEGS,SEGLEN, &
                  NVALEN,LFLAG)

!-----------------------------------------------------------------------
!
! subroutine    : SKLARR - MDB Retrieval
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
! called by     : ARRINDX
!
! calls         : nothing
!
! arguments     : IDESC    (ip) : array of users element numbers
!               : NELEM    (ip) : no. of elements in list
!               : IREPL    (ip) : counts to go with IDESC
!               : QCREQ    (ip) : true if QC flags wanted for each elem
! The next three arrays define the element index for a fixed array
!               : ELMNUM   (ip) : element number
!               : SEGNUM   (ip) : segment number
!               : SUBNUM   (ip) : subscript relative to seg start
!               : NROWS    (ip) : no of rows in index table
! The next 2 arrays give the index tablefor just the required elements
!               : SEGMENT  (ip/op) : segment number
!               : IVAL     (ip/op) : subscript relative to seg start
!               : NELREQ   (op) : no of elements in output table
!               : QCINC    (ip) : 2 if QC elements are available (or 1)
!               : NSEGS    (ip) : no of segments
!               : SEGLEN   (ip) : no of elements per segment
!               : NVALEN   (op) : actual segment lengths
!               : LFLAG    (ip) : true for diagnostics
!
! REVISION INFO :
!
! $Workfile: sklarr.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 26/11/2010 13:49:58$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         26/11/2010 13:49:58    Rosemary Lavery INTENT
!        updated
!  2    MetDB_Refresh 1.1         10/11/2010 15:01:22    Brian Barwell   Large
!       DO and IF blocks labelled after review.
!  1    MetDB_Refresh 1.0         04/11/2010 15:55:29    Brian Barwell   MetDB
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
!  Subroutine arguments
!-----------------------------------------------------------------------

INTEGER,  INTENT(IN)    :: NELEM
INTEGER,  INTENT(IN)    :: IDESC(NELEM)
INTEGER,  INTENT(IN)    :: IREPL(NELEM)
LOGICAL,  INTENT(IN)    :: QCREQ
INTEGER,  INTENT(IN)    :: NROWS
INTEGER,  INTENT(IN)    :: ELMNUM(NROWS)
INTEGER,  INTENT(IN)    :: SEGNUM(NROWS)
INTEGER,  INTENT(IN)    :: SUBNUM(NROWS)
INTEGER,  INTENT(INOUT) :: SEGMENT(:)
INTEGER,  INTENT(INOUT) :: IVAL(:)
INTEGER,  INTENT(OUT)   :: NELREQ
INTEGER,  INTENT(IN)    :: QCINC
INTEGER,  INTENT(IN)    :: NSEGS
INTEGER,  INTENT(IN)    :: SEGLEN(NSEGS)
INTEGER,  INTENT(OUT)   :: NVALEN(NSEGS)
LOGICAL,  INTENT(IN)    :: LFLAG

!----------------------------------------------------------------------
! Local variables
!----------------------------------------------------------------------

INTEGER  ::  QCWID
INTEGER  ::  I,J
INTEGER  ::  NN
INTEGER  ::  MSEG
INTEGER  ::  LASTONE
INTEGER  ::  DIRN

!----------------------------------------------------------------------
! First see if q/c flags are wanted and are available: QCWID=0 if there
! are no QC elements or there are, but the user doesn't want them.
! Assume the qc element is immediately before the data element.
!----------------------------------------------------------------------

IF(LFLAG)PRINT*,' SKLARR:QCREQ ',QCREQ
QCWID=0
IF (QCINC == 2) THEN
  IF (QCREQ) THEN
    QCWID=-1
  END IF
END IF
NELREQ=0        ! no. of elements required
LASTONE=0       ! index of previous element found
NN=0

!-----------------------------------------------------------------------
! Set up actual segment lengths
!-----------------------------------------------------------------------
DO I=1,NSEGS
  NVALEN(I)=SEGLEN(I)*QCINC
END DO

!----------------------------------------------------------------------
! Loop round names in users request
!----------------------------------------------------------------------

ELEM_LOOP: &
DO I=1,NELEM

!----------------------------------------------------------------------
! Now look up the name in the index, where an element no. is followed
! by 2 numbers to complete a line in the table.
! Assume the elements are in numerical order of index number, in both
! the index and the request.  Compare this element number with the
! found previously to search forward or backward through the index.
!----------------------------------------------------------------------

  IF(IDESC(I) > LASTONE)THEN
    DIRN=1
  ELSE IF (IDESC(I) == LASTONE)THEN
    DIRN=0
  ELSE
    DIRN=-1
  END IF

INDX_SEARCH: &
  DO J=1,NROWS
    NN=NN+DIRN
    IF(NN > NROWS)THEN
      NN=1
    ELSE IF (NN < 1)THEN
      NN=NROWS
    END IF

GOT_INDEX: &
    IF(IDESC(I) == ELMNUM(NN))THEN

!----------------------------------------------------------------------
! Got it, so set up a row of the output table
!----------------------------------------------------------------------
! If q/c flags are wanted, there are 2 lines per element, the q/c flags
! coming first
!----------------------------------------------------------------------

      IF (QCWID == -1) THEN
        NELREQ=NELREQ+2
      ELSE
        NELREQ=NELREQ+1
      END IF

      SEGMENT(NELREQ)=SEGNUM(NN)
      IVAL(NELREQ)=SUBNUM(NN)
      IF(IREPL(I) > 1)THEN
        IVAL(NELREQ)=IVAL(NELREQ)+ &
            (IREPL(I)-1) * NVALEN(SEGMENT(NELREQ))
      END IF

!----------------------------------------------------------------------
! Not all segment numbers have QC elements attached. e.g. -99 report
! text, -1 fo integer elements from the header/trailer etc.
!----------------------------------------------------------------------

      IF (QCWID == -1) THEN
        MSEG=SEGMENT(NELREQ)
        IF(MSEG > 0.AND.MSEG /= 99)THEN
          SEGMENT(NELREQ-1)=SEGMENT(NELREQ)
          IVAL(NELREQ-1)=IVAL(NELREQ)-1
        ELSE
          SEGMENT(NELREQ-1)=-999
        END IF
      END IF

      LASTONE=IDESC(I)

      GO TO 100      ! next users element
    END IF GOT_INDEX
  END DO INDX_SEARCH

!----------------------------------------------------------------------
! If a name can't be found, set the segment no. to missing
!----------------------------------------------------------------------

  IF (QCWID == 0) THEN
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
  PRINT*,' END OF SKLARR: NELREQ=',NELREQ
  write(6,*)' seg      disp      seg      disp '
  WRITE(6,'(4I8)')(SEGMENT(I),IVAL(I),I=1,NELREQ)
END IF

RETURN
END SUBROUTINE SKLARR
