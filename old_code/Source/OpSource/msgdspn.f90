SUBROUTINE MSGDSPN(MESSAGE,SEGMENT,IBEFOR,IREPL,QCREQ,           &
                         DISPL,NELEM,INPUT_NSEG,INPUT_SEGNUM,    &
                         INPUT_NWIDTH,INPUT_NREPLEN,LFLAG)

!-----------------------------------------------------------------------
!
! ROUTINE       : MSGDSPN in mdb retrieval
!
! PURPOSE       : Work out displacements of elements (bits to skip
!                 before value) in BUFR message from segment info
!                 in index & counts in a particular message
!                 (allowing for nesting, unlike MSGDSP)
!
! DESCRIPTION   : The input is the message structure (segment lengths
!                 & displacements of any replication counts) and the
!                 "skeleton" element table made by SKLDSPN, giving
!                 segment numbers & displacements within segments.
!                 (IREPL is logically a column in the element table,
!                 but needs QCREQ to keep values in step.)
!                  The output is a table with no reference to segments
!                 or replication left, only element displacements
!
! CALLED BY     : BITINDX (if there is nesting)
!
! CALLS         : VALUE
!
! ARGUMENTS
!          : MESSAGE       (i) BUFR message
!          : SEGMENT       (i) table column segment number
!          : IBEFOR        (i) table column bits before value in index
!          : IREPL         (i)
!          : QCREQ         (i)
!          : DISPL         (o) table column bits before in message.
!          : NELEM         (i) number of lines in table
!          : INPUT_NSEG    (i) number of segments
!          : INPUT_SEGNUM  (i)
!          : INPUT_NWIDTH  (i) width of count field for each seg.
!          : INPUT_NREPLEN (i) length of each segment.
!          : LFLAG         (i) true for diagnostics
!
! REVISION INFO :
!
!
! $Workfile: msgdspn.f90$ $Folder: OpSource$
! $Revision: 11$ $Date: 22/11/2010 17:29:59$
!
! CHANGE RECORD :
!
! $Log:
!  11   MetDB_Refresh 1.10        22/11/2010 17:29:59    Stan Kellett
!       function declaration for value removed as already declared in mod file
!  10   MetDB_Refresh 1.9         22/11/2010 15:46:49    Stan Kellett    array
!       argument dimensions changed to (*)
!  9    MetDB_Refresh 1.8         18/11/2010 10:46:47    Stan Kellett    added
!       use value_mod after checking with basic test code
!  8    MetDB_Refresh 1.7         12/11/2010 17:13:17    Rosemary Lavery remove
!        old header
!  7    MetDB_Refresh 1.6         04/11/2010 15:41:00    Rosemary Lavery
!       Corrections after review 
!  6    MetDB_Refresh 1.5         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  5    MetDB_Refresh 1.4         18/10/2010 17:55:52    Rosemary Lavery corr
!       to argument list
!  4    MetDB_Refresh 1.3         18/10/2010 16:22:49    Rosemary Lavery
!       Further amendments before review
!  3    MetDB_Refresh 1.2         18/10/2010 14:47:20    Rosemary Lavery Corr
!       to Integer declarations having variable dimensions
!  2    MetDB_Refresh 1.1         13/10/2010 13:33:29    Rosemary Lavery F90
!       amendments (before review)
!  1    MetDB_Refresh 1.0         12/10/2010 17:34:46    Rosemary Lavery
!       Initial F90 conversion
! $
!
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

! Element table:
!  (IREPL is dimensioned differently because IREPL(I) only corresponds
!   to DISPL(I) if QCREQ is false; otherwise IREPL(I/2) corresponds...

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)   :: MESSAGE
INTEGER,           INTENT(IN)   :: NELEM              ! number of elements
INTEGER,           INTENT(IN)   :: SEGMENT(NELEM)     ! segment number
INTEGER,           INTENT(IN)   :: IBEFOR(NELEM)      ! bits before value (in segment)
INTEGER,           INTENT(IN)   :: IREPL(*)           ! instance (or 1 if no replication)
LOGICAL,           INTENT(IN)   :: QCREQ
INTEGER,           INTENT(OUT)  :: DISPL(NELEM)       ! bits before value (in message)
INTEGER,           INTENT(IN)   :: INPUT_NSEG         ! number of segments
INTEGER,           INTENT(IN)   :: INPUT_SEGNUM(*)    ! segment number
INTEGER,           INTENT(IN)   :: INPUT_NWIDTH(*)    ! width of replication count (or 0)
INTEGER,           INTENT(IN)   :: INPUT_NREPLEN(*)   ! bits in segment (to replicate)
LOGICAL,           INTENT(IN)   :: LFLAG


! Local Parameters

INTEGER, PARAMETER              :: MAXLEV = 3      ! dimension of LSEG & NTH up to 3 levels of nesting


! Local Scalars

! Message structure (generic, as input):


! Message structure (specific to this message) with extra columns:

INTEGER              :: NSEG                  ! number of segments
INTEGER              :: SEGNUM(999)           ! segment number
INTEGER              :: NWIDTH(999)           ! width of replication count (or 0)
INTEGER              :: NREPLEN(999)          ! bits in segment (to replicate)
INTEGER              :: NREPL(999)            ! replication count
INTEGER              :: SEGSTAR(999)          ! number of bits before segment

INTEGER              :: TRAILER_START         ! number of bits before trailer
INTEGER              :: I,J                   ! loop variables
INTEGER              :: IBEFOUR               ! number of bits before section 4
INTEGER              :: ELMSEG                ! segment number for this element

INTEGER              :: LSEG(MAXLEV)          ! segment number at each level
INTEGER              :: NTH(MAXLEV)           ! instance of each LSEG(L)

INTEGER              :: NTHS                  ! value of IREPL to set NTH
INTEGER              :: LEVELS                ! number of nesting levels
INTEGER              :: L                     ! nesting level in loop

INTEGER              :: MSGSEG
INTEGER              :: NSUBSEG
INTEGER              :: NEXTRA
INTEGER              :: NTIMES
LOGICAL              :: NESTED
LOGICAL              :: NOMATCH               ! set if n-th instance not found

INTEGER              :: MDI = -9999999        ! missing data indicator

SAVE

!-----------------------------------------------------------------------
! First make an array of displacements of segment starts in the message,
! building these up by adding count times bits replicated plus bits in
! count itself each time round the loop.
!
! VALUE is called to return the replication count for each segment
! if there is a replication - and the count is in the data.
! But the count may be in the descriptor rather than the data
! - if so, MESTRUCT sets NWIDTH to minus the count, so use that.
!
! NREPL is 1 if NWIDTH is 0, but NREPL can be 0 or >1 if NWIDTH = 8.
!
! N.B. The VALUE call below only works if NWIDTH(1)=0; otherwise
!      SEGSTAR(1)-NWIDTH(1)<0.  It's not impossible for a message
!      to start with a replication count, but the input message
!      structure doesn't seem to allow for this.
!-----------------------------------------------------------------------

! Copy the input table so that it can be reused for the next message.

NSEG=INPUT_NSEG
DO I=1,NSEG
  SEGNUM(I)=INPUT_SEGNUM(I)
  NWIDTH(I)=INPUT_NWIDTH(I)
  NREPLEN(I)=INPUT_NREPLEN(I)
END DO

NESTED=SEGNUM(NSEG) /= NSEG
I=1
SEGSTAR(1)=0

DO_SEG: &
DO WHILE (I <= NSEG)
  IF (NWIDTH(I) == 0) THEN
    NREPL(I)=1
  ELSE IF (NWIDTH(I) > 0) THEN
    NREPL(I)=VALUE(MESSAGE,SEGSTAR(I)-NWIDTH(I),NWIDTH(I))
  ELSE
    NREPL(I)=-NWIDTH(I)
  END IF

! If there is nesting in the message & a count >1, then
! count subsegments (segment numbering shows which are subsegments)
! & replicate lines in message structure table,
! incrementing number of segments.

  NSUBSEG=1
IF_NEST: &
  IF (NESTED .AND. NREPL(I) > 1) THEN
    DO WHILE (SEGNUM(I+NSUBSEG) > SEGNUM(I)*10)
      NSUBSEG=NSUBSEG+1
    END DO

! Replicate in the usual way, by moving down the remaining lines
! & then repeating the subsegment lines to fill the gap.
! Set count width =0 (for copy below) to show that replication is done.

    NEXTRA=NSUBSEG*(NREPL(I)-1)

IF_EXTRA: &
    IF (NEXTRA > 0) THEN
      DO J=NSEG,I+NSUBSEG,-1
        SEGNUM(J+NEXTRA)=SEGNUM(J)
        NWIDTH(J+NEXTRA)=NWIDTH(J)
        NREPLEN(J+NEXTRA)=NREPLEN(J)
      END DO

      NWIDTH(I)=0
      DO J=0,NEXTRA-1
        SEGNUM(I+NSUBSEG+J)=SEGNUM(I+J)
        NWIDTH(I+NSUBSEG+J)=NWIDTH(I+J)
        NREPLEN(I+NSUBSEG+J)=NREPLEN(I+J)
      END DO
      NSEG=NSEG+NEXTRA
    END IF IF_EXTRA
  END IF IF_NEST

! Find the start of the next segment.
! Ignore any negative NWIDTH, meaning no count in data.
!  (N.B. this is the next NWIDTH, not the one zeroed & copied above!)
! If NREPL=0, only the width of the count itself is added.

  IF (I < NSEG) THEN
    IF (NESTED) THEN
      SEGSTAR(I+1)=SEGSTAR(I)
      IF (NREPL(I) > 0) SEGSTAR(I+1)=SEGSTAR(I+1)+NREPLEN(I)
    ELSE
      SEGSTAR(I+1)=SEGSTAR(I)+NREPLEN(I)*NREPL(I)
    END IF
    IF (NWIDTH(I+1) > 0) SEGSTAR(I+1)=SEGSTAR(I+1)+NWIDTH(I+1)
    IF (LFLAG) PRINT *,'MSGDSPN:',I+1,'th segment',  &
                  SEGNUM(I+1),SEGSTAR(I+1),'bits before it'
  END IF
  I=I+1
END DO DO_SEG

!-----------------------------------------------------------------------
! Values with the segment set to 99 are in the trailer following the
! message, so we need the length of the data section to add to those
! of the other sections (assumed to be minimum) of the BUFR message.
!  Assume 4 bytes for section 0 (Edition 0, so only 'BUFR' itself),
! 18 for section 1, no section 2, 10 for section 3 (only one sequence
! descriptor.  So the data section starts after 32 bytes.  Use VALUE
! to get the data section length & add 4 for the final '7777' to get
! to the start of the trailer.
!      N.B. Some messages ARE stored in edition 2 (e.g. ERS...), but
! without trailers - and they're fully decoded, so not handled here.
!-----------------------------------------------------------------------

IBEFOUR=256
TRAILER_START=8*(4+18+10+VALUE(MESSAGE,IBEFOUR,24)+4)

! Now work out the value displacements, first the special indicators.

DO_ELEM: &
DO I=1,NELEM
  ELMSEG=SEGMENT(I)

! ELMSEG=-999 indicates missing data if the element was not found or
! there is no QC flag (e.g. for report text): return missing data.

IF_ELMSEG: &
  IF (ELMSEG == -999) THEN
    DISPL(I)=MDI

! If ELMSEG<0 (but not -999, already tested),then the data is either
! report text (-99) or from BUFR section 1 (in an array in the main
! retrieval program).  Just pass on the negative indicator.

  ELSE IF (ELMSEG < 0) THEN
    DISPL(I)=ELMSEG

! If data is in trailer (-97 if nesting, 99 otherwise) use trailer
! start set up above.  (Nesting can lead to segment numbers >99!)

  ELSE IF (ELMSEG == -97 .OR. (.NOT.NESTED.AND.ELMSEG == 99)) THEN
    DISPL(I)=TRAILER_START+IBEFOR(I)

! Set zero-replicated element to missing.

  ELSE IF (NREPL(ELMSEG) == 0) THEN
    DISPL(I)=MDI

!-----------------------------------------------------------------------
! Otherwise get the value from the data section.
! In the simplest case this code will take a segment start & add a
! displacement within the segment.  But if there is any nesting in
! the message, the segment concerned is not simply the ELMSEG-th:
! the NTH segment with the given number must be searched for.  And
! if there is replication, but no nesting, a multiple of the number
! of bits replicated must be added.
!-----------------------------------------------------------------------

  ELSE
    IF (QCREQ) THEN
      NTHS=IREPL((I+1)/2)
    ELSE
      NTHS=IREPL(I)
    END IF

! Find number of levels of nesting & set number & times for each.

IF_NESTED: &
    IF (NESTED) THEN   ! if there's any nesting
      LEVELS=1
      DO WHILE (NTHS/1000**LEVELS > 0)
        LEVELS=LEVELS+1
      END DO

      LSEG(LEVELS)=ELMSEG
      DO L=LEVELS,1,-1
        NTH(L)=MOD(NTHS,1000)
        NTHS=NTHS/1000
        IF (L < LEVELS) LSEG(L)=LSEG(L+1)/10
      END DO

! If there is nesting, loop round the segment numbers in the message
! structure table (with replications expanded out) looking for the
! NTH match (with the segment number in the element table) at each
! level.  So count instances of LSEG(1), then, when NTH(1)-th found,
! instances of LSEG(2) after that point - but only while they are
! subsegments of the same replicated segment, i.e. stop looking if
! another LSEG(1) is reached, or LSEG(1)+1 implying the end of that
! replication...

      NOMATCH=.FALSE.
      MSGSEG=0
      DO L=1,LEVELS
        NTIMES=0
        DO WHILE (MSGSEG < NSEG .AND. NTIMES < NTH(L)  &
                        .AND. .NOT.NOMATCH)
          MSGSEG=MSGSEG+1
          IF (L > 1) THEN
            IF (SEGNUM(MSGSEG) < LSEG(L-1)*10) NOMATCH=.TRUE.
          END IF
          IF (SEGNUM(MSGSEG) == LSEG(L)) NTIMES=NTIMES+1
        END DO
      END DO

! If there was no NTH match at some level, return missing data.
! (NTIMES=0 if the count fell short at a level before the last.)

      IF (NOMATCH .OR. NTIMES < NTH(LEVELS)) THEN
        DISPL(I)=MDI
      ELSE
        DISPL(I)=SEGSTAR(MSGSEG)+IBEFOR(I)
      END IF
    ELSE

! If there is no nesting, we still have to check that there is
! an NTHS replication in this message - if not, return missing.

      IF (NTHS > NREPL(ELMSEG)) THEN
        DISPL(I)=MDI
      ELSE
       DISPL(I)=SEGSTAR(ELMSEG)+IBEFOR(I)+(NTHS-1)*NREPLEN(ELMSEG)
      END IF
    END IF IF_NESTED
  END IF IF_ELMSEG
  IF (LFLAG) PRINT *,'MSGDSPN:',I,'th value after',DISPL(I),'bits'
END DO DO_ELEM
RETURN
END SUBROUTINE MSGDSPN
