SUBROUTINE MSGDSP(MESSAGE,SEGMENT,IBEFOR,DISPL,NELEM,NSEG,  &
                        NWIDTH,NREPLEN,LFLAG)

!-----------------------------------------------------------------------
!
! ROUTINE       : MSGDSP in mdb retrieval
!
! PURPOSE       : work out displacements of elements (bits to skip
!                 before value) in bufr message from segment info
!                 in index & counts in a particular message.
!
! DESCRIPTION   : The segment info consists of a count followed by
!                 that many pairs of numbers.  The first number is
!                 the width of a count field (0 implies a count of 1),
!                 the second is the number of bits replicated.
!                 The elements required are referred to by a table
!                 consisting of segment number and bits before the
!                 element relative to the start of the segment.  The
!                 segment number may be an indicator such as -99 for
!                 report text.
!
! CALLED BY     : BITINDX
!
! CALLS         : VALUE
!
! ARGUMENTS     : MESSAGE  (ip) : BUFR message
!               : SEGMENT  (ip) : table column segment number
!               : IBEFOR   (iop) : table column bits before value in
!                               : index
!               : DISPL    (op) : table column bits before in message.
!               : NELEM    (ip) : number of lines in table
!               : NSEG     (ip) : number of segments
!               : NWIDTH   (ip) : width of count field for each seg.
!               : NREPLEN  (ip) : length of each segment.
!               : LFLAG    (ip) : true for diagnostics
!
! REVISION INFO :
!
! $Workfile: msgdsp.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 25/11/2010 16:43:44$
!
! CHANGE RECORD :
!
! $Log:
!  10   MetDB_Refresh 1.9         25/11/2010 16:43:44    Brian Barwell
!       Argument of call to VALUE changed to variable rather than expression.
!  9    MetDB_Refresh 1.8         22/11/2010 17:28:02    Stan Kellett
!       function value declaration removed as already declared in mod file
!  8    MetDB_Refresh 1.7         18/11/2010 10:43:29    Stan Kellett    use
!       value_mod added. IBEFORE argument changed to intent (inout) and line
!       using IBEFORE readded after merging with basic test code.
!  7    MetDB_Refresh 1.6         12/11/2010 17:13:17    Rosemary Lavery remove
!        old header
!  6    MetDB_Refresh 1.5         04/11/2010 15:41:00    Rosemary Lavery
!       Corrections after review 
!  5    MetDB_Refresh 1.4         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  4    MetDB_Refresh 1.3         18/10/2010 17:55:52    Rosemary Lavery corr
!       to argument list
!  3    MetDB_Refresh 1.2         18/10/2010 16:22:49    Rosemary Lavery
!       Further amendments before review
!  2    MetDB_Refresh 1.1         13/10/2010 13:33:29    Rosemary Lavery F90
!       amendments (before review)
!  1    MetDB_Refresh 1.0         12/10/2010 17:34:46    Rosemary Lavery
!       Initial F90 conversion
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE value_mod

IMPLICIT NONE

!----------------------------------------------------------------------
! interface arguments
!----------------------------------------------------------------------

CHARACTER (LEN=*), INTENT(IN)    :: MESSAGE
INTEGER,           INTENT(IN)    :: NELEM
INTEGER,           INTENT(IN)    :: SEGMENT(NELEM)
INTEGER,           INTENT(INOUT) :: IBEFOR(NELEM)
INTEGER,           INTENT(OUT)   :: DISPL(NELEM)
INTEGER,           INTENT(IN)    :: NSEG
INTEGER,           INTENT(IN)    :: NWIDTH(NSEG)
INTEGER,           INTENT(IN)    :: NREPLEN(NSEG)
LOGICAL,           INTENT(IN)    :: LFLAG

!----------------------------------------------------------------------
! local variables
!----------------------------------------------------------------------

INTEGER              :: NREPL(99)    ! replication counts for each seg.
INTEGER              :: SEGSTAR(99)
INTEGER              :: I,J
INTEGER              :: IBEFOUR
INTEGER              :: MSEG
INTEGER              :: MDI = -9999999

SAVE

!-----------------------------------------------------------------------
! NWIDTH(1) is always equal to 0 !!
!
! First make an array of displacements of segment starts in the message
! building these up by adding count times bits replicated plus bits in
! count itself each time round the loop.
!
! VALUE is called to return the replication count for each segment
! if there is a replication - and the count is in the data.
! But the count may be in the descriptor rather than the data
! - if so, MESTRUCT sets NWIDTH to minus the count, so use that.
!
! NREPL is 1 if NWIDTH is 0, but NREPL can be 0 or >1 if NWIDTH = 8.
!-----------------------------------------------------------------------

SEGSTAR(1)=0

DO_SEG: &
DO I=1,NSEG
 IF (NWIDTH(I) == 0) THEN
   NREPL(I)=1
 ELSE IF (NWIDTH(I) > 0) THEN
   IBEFOUR=SEGSTAR(I)-NWIDTH(I)
   NREPL(I)=VALUE(MESSAGE,IBEFOUR,NWIDTH(I))
 ELSE
   NREPL(I)=-NWIDTH(I)
 END IF

! Ignore negative NWIDTH (no count in data) when calculating next
! segment start.

 IF (I < NSEG) THEN
   SEGSTAR(I+1)=SEGSTAR(I)+NREPL(I)*NREPLEN(I)
   IF (NWIDTH(I+1) > 0) THEN
     SEGSTAR(I+1)=SEGSTAR(I+1)+NWIDTH(I+1)
   END IF
 END IF
END DO DO_SEG

!-----------------------------------------------------------------------
! values with the segment set to 99 are in the trailer which follows the
! message, so we need the length of the data section to add to those of
! the other sections (assumed to be minimum) of the bufr message.
!
! length of section 0 = 4 (must be BUFR edition 0), section 1 = 18,
! section 2 = not used, section 3 = 10 (must be using a local D descr-
! iptor). Therefore start of section 4 (data section) is 32 octets
! = 256 bits (8*(4+18+10)) = IBEFOUR. The start of the trailer is 256 +
! length of data section + length of section 5 = 4 octets. Length of
! data section calculated using VALUE function.
!-----------------------------------------------------------------------

IBEFOUR=256
SEGSTAR(99)=8*(4+18+10+VALUE(MESSAGE,IBEFOUR,24)+4)
IF(LFLAG)THEN
  PRINT*,'IN MSGDSP: NSEG',NSEG
  WRITE(6,'(10I6)')(SEGSTAR(I),I=1,NSEG),SEGSTAR(99)
END IF

!-----------------------------------------------------------------------
! now we can easily work out the displacements of the values!
! Deal with all the special indicators first.
!-----------------------------------------------------------------------

DO_ELEM: &
DO J=1,NELEM
  MSEG=SEGMENT(J)
  IF(LFLAG)PRINT*,'MSGDSP: J,MSEG',J,MSEG

!-----------------------------------------------------------------------
! MSEG = -99 indicates report text. Just pass this indicator on.
!
!-----------------------------------------------------------------------

IF_MSEG: &
  IF (MSEG == -99) THEN
    DISPL(J)=-99

!-----------------------------------------------------------------------
! MSEG = -999 indictaes missing data.  Maybe the element was not found,
! or it is for a QC flag that does not exist (e.g. no QC for report txt)
! Return missing data indicator,
!-----------------------------------------------------------------------

  ELSE IF (MSEG == -999) THEN
    DISPL(J)=MDI

!-----------------------------------------------------------------------
! if MSEG=99, data in trailer.  SEGSTAR(99) set up already
!
!-----------------------------------------------------------------------

  ELSE IF (MSEG == 99) THEN
    DISPL(J)=SEGSTAR(MSEG)+IBEFOR(J)

!----------------------------------------------------------------------
! If MSEG = - n ( but not -99 already tested) then the data is from
! BUFR section 1 which is held in an array in the main retrieval
! program.  Pass on the -n indicator.
!-----------------------------------------------------------------------

  ELSE IF (MSEG < 0)THEN

    DISPL(J)=MSEG

!-----------------------------------------------------------------------
! zero-replicated element, set to missing.
!-----------------------------------------------------------------------

  ELSE IF (MSEG /= 99 .AND. NREPL(MSEG) == 0) THEN
    DISPL(J)=MDI

!-----------------------------------------------------------------------
! Set to missing if replications requested greater than replications
! in segment (only if replication count is in data - hence check
! on NWIDTH (>0 if count in data, <0 if count in descriptor)).
!-----------------------------------------------------------------------

  ELSE IF (MSEG /= 99 .AND. NWIDTH(MSEG) > 0 .AND.  &
        IBEFOR(J) >= NREPL(MSEG)*NREPLEN(MSEG)) THEN
    DISPL(J)=MDI

!-----------------------------------------------------------------------
! otherwise, element in BUFR section 4.
!-----------------------------------------------------------------------

  ELSE
    DISPL(J)=SEGSTAR(MSEG)+IBEFOR(J)

  END IF IF_MSEG

ENDDO DO_ELEM

RETURN
END SUBROUTINE MSGDSP
