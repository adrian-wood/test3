SUBROUTINE ARRSUB(FIXARR,SEGMENT,IVAL,NELREQ,DISPL,SOURCE,NVALEN,&
           &STYP,SEGST,NSEGS,QCINC,LFLAG)               !2.0

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
!               : NVALEN   (op) : actual segment lengths
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
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:27  usmdb
! Unused variables deleted. Unused dummy argument
! SEGLEN removed. Added copyright - S.Cox
!
! Revision 1.2  97/09/22  12:49:38  12:49:38  uspm (Pat McCormack)
! Reorder variable type definitions to satisfy NAG F90 compiler
!
! Revision 1.1  1997/08/04 15:17:52  uspm
! Initial revision
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
! declare variables used to dimension arrays first, then remaining
! interface variables
!-----------------------------------------------------------------------

INTEGER   NELREQ
INTEGER   NSEGS

REAL      FIXARR(*)
INTEGER   SEGMENT(NELREQ)
INTEGER   IVAL(NELREQ)
INTEGER   DISPL(NELREQ)
INTEGER   SOURCE(NELREQ)
INTEGER   NVALEN(NSEGS)
INTEGER   STYP(NSEGS)
INTEGER   SEGST(NSEGS)
INTEGER   QCINC
LOGICAL   LFLAG

!----------------------------------------------------------------------
! local variables
!---------------------------------------------------------------------

INTEGER   NREPL(99)           ! replication counts for each seg.
INTEGER   I,J
INTEGER   MSEG
INTEGER   MDI
CHARACTER HEAD*132

DATA    MDI/-999/          ! changed from -9999999  S.Cox 18-10-96

SAVE

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/arrsub.F,v $&
&'//'$ $Date: 26/01/2010 10:18:13$ $Revision: 1$'

!-----------------------------------------------------------------------
! First find replication counts for each segment
!-----------------------------------------------------------------------

DO I=1,NSEGS
 IF (STYP(I).EQ.0) THEN
   NREPL(I)=1
 ELSE
   NREPL(I)=FIXARR(STYP(I))
 ENDIF
ENDDO
IF(LFLAG)THEN
  PRINT*,' ARRSUB - Replication counts'
  WRITE(6,*)'  seg   ptr   repl'
  WRITE(6,'(3I6)')(I,STYP(I),NREPL(I),I=1,NSEGS)
ENDIF

!-----------------------------------------------------------------------
! Now find subscripts for the required elements.  Return two parallel
! arrays DISPL giving the subscript of the element in a particular
! array, SOURCE identifying that array.
! N.B. DISPL is not always needed.
!-----------------------------------------------------------------------


DO J=1,NELREQ
  MSEG=SEGMENT(J)

!-----------------------------------------------------------------------
! MSEG = -99 indicates report text. Just pass this indicator on.
!-----------------------------------------------------------------------

  IF (MSEG.EQ.-99) THEN
    SOURCE(J)=-99

!-----------------------------------------------------------------------
! MSEG = -999 indicates missing data.  Maybe the element was not found,
! or it is for a QC flag that does not exist (e.g. no QC for report txt)
! Return missing data indicator,
!-----------------------------------------------------------------------

  ELSEIF (MSEG.EQ.-999) THEN
    SOURCE(J)=MDI

!----------------------------------------------------------------------
! If MSEG = - 1 then the data is from an integer array held in
! the calling program
!-----------------------------------------------------------------------

  ELSEIF (MSEG.EQ.-1)THEN
    DISPL(J)=IVAL(J)
    SOURCE(J)=1

!----------------------------------------------------------------------
! If MSEG = - 2 then the data is from a character array held in
! the calling program.
!-----------------------------------------------------------------------

  ELSEIF (MSEG.EQ.-2)THEN
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

    IF(NREPL(MSEG).EQ.0)THEN
      SOURCE(J)=MDI
    ELSEIF(IVAL(J).GT.NREPL(MSEG)*NVALEN(MSEG))THEN
      SOURCE(J)=MDI
    ELSE
      DISPL(J)=IVAL(J)+SEGST(MSEG)-QCINC
      SOURCE(J)=10
    ENDIF

  ENDIF

END DO

RETURN
END SUBROUTINE ARRSUB
