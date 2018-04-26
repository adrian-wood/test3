SUBROUTINE ARRSUB(FIXARR,SEGMENT,IVAL,NELREQ,DISPL,SOURCE,NVALEN, &
                  STYP,SEGST,NSEGS,QCINC,LFLAG)  

!-----------------------------------------------------------------------
!
! subroutine    : ARRSUB in mdb retrieval
!
! purpose       : work out subscripts of requested elements in a fixed
!                 array
!
! description   : For each element there is an element number, segment
!                 number and segment subscript. The segment structure
!                 consists of a pointer to a segment replication count
!                 (0 for mandatory segments), the subscript of the first
!                 element in the fixed array (not the qc element) and
!                 the number of elements in the segment (excluding qc)
!
!
! called by     : ARRINDX
!
! calls         : nothing
!
! arguments     : FIXARR   (ip) : fixed array of data
!               : SEGMENT  (ip) : table column segment number
!               : IVAL     (ip) : table column subscript for FIXARR
!                                  (relative to segment)
!               : NELREQ   (ip) : no of elements required
!               : DISPL    (op) : table column subscript in FIXARR array
!                                  (from the start)
!               : SOURCE   (op) : source of data indicator
!               : NVALEN   (ip) : actual segment lengths
!               : STYP     (ip) : pointer to segment repl. count
!               : SEGST    (ip) : subscript of first elem in segment
!               : NSEGS    (ip) : no of segments
!               : QCINC    (ip) : 2 if qc elements in fixed array
!               : LFLAG    (ip) : true for diagnostics
!
! written by    : S.M.Needham 12-09-96
!
!Y2K  26.06.1997  ARRSUB is Year 2000 compliant.
!
! change record :
!
! 09-03-97      : Removed declaration of IRDES(500) - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  3    MetDB_Refresh 1.2         20/12/2010 11:24:29    Sheila Needham  Change
!        INTENT to IN on NVALEN
!  2    MetDB_Refresh 1.1         17/11/2010 15:48:16    Stan Kellett
!       Removed old revision info and change copyright to 2010
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
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

! Use statements:
! <Interfaces>

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

REAL,         INTENT(IN)  ::  FIXARR(*)
INTEGER,      INTENT(IN)  ::  NELREQ
INTEGER,      INTENT(IN)  ::  SEGMENT(NELREQ)
INTEGER,      INTENT(IN)  ::  IVAL(NELREQ)
INTEGER,      INTENT(OUT) ::  DISPL(NELREQ)
INTEGER,      INTENT(OUT) ::  SOURCE(NELREQ)
INTEGER,      INTENT(IN)  ::  NSEGS
INTEGER,      INTENT(IN) ::  NVALEN(NSEGS)
INTEGER,      INTENT(IN)  ::  STYP(NSEGS)
INTEGER,      INTENT(IN)  ::  SEGST(NSEGS)
INTEGER,      INTENT(IN)  ::  QCINC
LOGICAL,      INTENT(IN)  ::  LFLAG

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!----------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

INTEGER      ::  NREPL(99)    ! replication counts for each seg.
INTEGER      ::  I,J
INTEGER      ::  MSEG
INTEGER      ::  MDI = -999

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! First find replication counts for each segment
!-----------------------------------------------------------------------

DO I=1,NSEGS
 IF (STYP(I) == 0) THEN
   NREPL(I)=1
 ELSE
   NREPL(I)=FIXARR(STYP(I))
 END IF
END DO
IF(LFLAG)THEN
  PRINT*,' ARRSUB - Replication counts'
  WRITE(6,*)'  seg   ptr   repl'
  WRITE(6,'(3I6)')(I,STYP(I),NREPL(I),I=1,NSEGS)
END IF

!-----------------------------------------------------------------------
! Now find subscripts for the required elements.  Return two parallel
! arrays DISPL giving the subscript of the element in a particular
! array, SOURCE identifying that array.
! N.B. DISPL is not always needed.
!-----------------------------------------------------------------------

DOLABEL1: &
DO J=1,NELREQ
  MSEG=SEGMENT(J)

!-----------------------------------------------------------------------
! MSEG = -99 indicates report text. Just pass this indicator on.
!-----------------------------------------------------------------------

IFLABEL1: &
  IF (MSEG == -99) THEN
    SOURCE(J)=-99

!-----------------------------------------------------------------------
! MSEG = -999 indicates missing data.  Maybe the element was not found,
! or it is for a QC flag that does not exist (e.g. no QC for report txt)
! Return missing data indicator,
!-----------------------------------------------------------------------

  ELSE IF (MSEG == -999) THEN
    SOURCE(J)=MDI

!----------------------------------------------------------------------
! If MSEG = - 1 then the data is from an integer array held in
! the calling program
!-----------------------------------------------------------------------

  ELSE IF (MSEG == -1)THEN
    DISPL(J)=IVAL(J)
    SOURCE(J)=1

!----------------------------------------------------------------------
! If MSEG = - 2 then the data is from a character array held in
! the calling program.
!-----------------------------------------------------------------------

  ELSE IF (MSEG == -2)THEN
    DISPL(J)=IVAL(J)
    SOURCE(J)=2

!-----------------------------------------------------------------------
! Now process the 'real' segment numbers
!-----------------------------------------------------------------------

  ELSE

!-----------------------------------------------------------------------
! The element is from the fixed array.  But check whether there are
! enough replications.
!-----------------------------------------------------------------------

    IF(NREPL(MSEG) == 0)THEN
      SOURCE(J)=MDI
    ELSE IF(IVAL(J) > NREPL(MSEG)*NVALEN(MSEG))THEN
      SOURCE(J)=MDI
    ELSE
      DISPL(J)=IVAL(J)+SEGST(MSEG)-QCINC
      SOURCE(J)=10
    END IF

  END IF IFLABEL1

END DO DOLABEL1

RETURN
END SUBROUTINE ARRSUB
