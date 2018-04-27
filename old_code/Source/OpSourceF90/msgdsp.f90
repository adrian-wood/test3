SUBROUTINE MSGDSP(MESSAGE,SEGMENT,IBEFOR,DISPL,NELEM,NSEG,&
     &NWIDTH,NREPLEN,LFLAG)

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
!
! CALLED BY     : BITINDX
!
! CALLS         : VALUE
!
! ARGUMENTS     : MESSAGE  (ip) : BUFR message
!               : SEGMENT  (ip) : table column segment number
!               : IBEFOR   (ip) : table column bits before value in
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
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/msgdsp.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2001/10/03 15:10:04  usmdb
! 15 Oct 2001    C Long
! 2.1  Allow for replication count in descriptor rather than data
!      (by NWIDTH=-count in MESTRUCT in bit index)
!
! Revision 2.0  2001/07/03  10:43:38  10:43:38  usmdb (Generic MetDB account)
! Moved declaration of variable NELEM before declaration of arrays of
! size NELEM. Added copyright and modified header - S.Cox
!
! Revision 1.1  97/08/04  15:21:35  15:21:35  uspm (Pat McCormack)
! Initial revision
!
! Operational from aug 27 1996
!
! written by : S.M.Needham 29-07-96 - based on DISPLM by S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER   NELEM                                               !2.0
CHARACTER*(*) MESSAGE
INTEGER   SEGMENT(NELEM)
INTEGER   IBEFOR(NELEM)
INTEGER   DISPL(NELEM)
INTEGER   NSEG
INTEGER   NWIDTH(NSEG)
INTEGER   NREPLEN(NSEG)
LOGICAL   LFLAG

!----------------------------------------------------------------------
! local variables
!----------------------------------------------------------------------

CHARACTER HEAD*132
INTEGER   NREPL(99)           ! replication counts for each seg.
INTEGER   SEGSTAR(99)
INTEGER   VALUE               ! function
INTEGER   I,J
INTEGER   IBEFOUR
INTEGER   MSEG
INTEGER   MDI

DATA    MDI/-9999999/

SAVE

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/msgdsp.f,v $&
&'//'$ $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

!-----------------------------------------------------------------------
! NWIDTH(1) is always equal to 0 !!
!
! First make an array of displacements of segment starts in the message
! building these up by adding count times bits replicated plus bits in
! count itself each time round the loop.
!
! VALUE is called to return the replication count for each segment
! if there is a replication - and the count is in the data.        !2.1
! But the count may be in the descriptor rather than the data      !2.1
! - if so, MESTRUCT sets NWIDTH to minus the count, so use that.   !2.1
!
! NREPL is 1 if NWIDTH is 0, but NREPL can be 0 or >1 if NWIDTH = 8.
!-----------------------------------------------------------------------

SEGSTAR(1)=0
DO I=1,NSEG
 IF (NWIDTH(I).EQ.0) THEN
   NREPL(I)=1
 ELSE IF (NWIDTH(I).GT.0) THEN                               !2.1
   NREPL(I)=VALUE(MESSAGE,SEGSTAR(I)-NWIDTH(I),NWIDTH(I))
 ELSE                                                        !2.1
   NREPL(I)=-NWIDTH(I)                                       !2.1
 ENDIF

! Ignore negative NWIDTH (no count in data) when calculating next  !2.1
! segment start.                                                   !2.1

 IF (I.LT.NSEG) THEN
   SEGSTAR(I+1)=SEGSTAR(I)+NREPL(I)*NREPLEN(I)               !2.1
   IF (NWIDTH(I+1).GT.0) THEN                                !2.1
     SEGSTAR(I+1)=SEGSTAR(I+1)+NWIDTH(I+1)                   !2.1
   ENDIF                                                     !2.1
 ENDIF
END DO

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
ENDIF

!-----------------------------------------------------------------------
! now we can easily work out the displacements of the values!
! Deal with all the special indicators first.
!-----------------------------------------------------------------------

DO J=1,NELEM
  MSEG=SEGMENT(J)
  IF(LFLAG)PRINT*,'MSGDSP: J,MSEG',J,MSEG

!-----------------------------------------------------------------------
! MSEG = -99 indicates report text. Just pass this indicator on.
!
!-----------------------------------------------------------------------

  IF (MSEG.EQ.-99) THEN
    DISPL(J)=-99

!-----------------------------------------------------------------------
! MSEG = -999 indictaes missing data.  Maybe the element was not found,
! or it is for a QC flag that does not exist (e.g. no QC for report txt)
! Return missing data indicator,
!-----------------------------------------------------------------------

  ELSEIF (MSEG.EQ.-999) THEN
    DISPL(J)=MDI

!-----------------------------------------------------------------------
! if MSEG=99, data in trailer.  SEGSTAR(99) set up already
!
!-----------------------------------------------------------------------

  ELSEIF (MSEG.EQ.99) THEN
    DISPL(J)=SEGSTAR(MSEG)+IBEFOR(J)

!----------------------------------------------------------------------
! If MSEG = - n ( but not -99 already tested) then the data is from
! BUFR section 1 which is held in an array in the main retrieval
! program.  Pass on the -n indicator.
!-----------------------------------------------------------------------

  ELSEIF (MSEG.LT.0)THEN

    DISPL(J)=MSEG

!-----------------------------------------------------------------------
! zero-replicated element, set to missing.
!-----------------------------------------------------------------------

  ELSE IF (MSEG.NE.99 .AND. NREPL(MSEG).EQ.0) THEN
    DISPL(J)=MDI

!-----------------------------------------------------------------------
! Set to missing if replications requested greater than replications
! in segment (only if replication count is in data - hence check   !2.1
! on NWIDTH (>0 if count in data, <0 if count in descriptor)).     !2.1
!-----------------------------------------------------------------------

  ELSE IF (MSEG.NE.99 .AND. NWIDTH(MSEG).GT.0 .AND.&         !2.1
  &IBEFOR(J).GE.NREPL(MSEG)*NREPLEN(MSEG)) THEN
    DISPL(J)=MDI

!-----------------------------------------------------------------------
! otherwise, element in BUFR section 4.
!-----------------------------------------------------------------------

  ELSE
    DISPL(J)=SEGSTAR(MSEG)+IBEFOR(J)

  ENDIF

END DO

RETURN
END SUBROUTINE MSGDSP
