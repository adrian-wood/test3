      SUBROUTINE MSGDSPN(MESSAGE,SEGMENT,IBEFOR,IREPL,QCREQ,
     &                   DISPL,NELEM,INPUT_NSEG,INPUT_SEGNUM,
     &                   INPUT_NWIDTH,INPUT_NREPLEN,LFLAG)

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
! ARGUMENTS     : MESSAGE  (ip) : BUFR message
!               : SEGMENT  (ip) : table column segment number
!               : IBEFOR   (ip) : table column bits before value in
!                               : index
!               : IREPL    (ip) :                                  !2.2
!               : QCREQ    (ip) :                                  !2.2
!               : DISPL    (op) : table column bits before in message.
!               : NELEM    (ip) : number of lines in table
!               : NSEG     (ip) : number of segments
!               : SEGNUM   (ip) :                                  !2.2
!               : NWIDTH   (ip) : width of count field for each seg.
!               : NREPLEN  (ip) : length of each segment.
!               : LFLAG    (ip) : true for diagnostics
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:27$
! $Source: /home/us0400/mdb/op/lib/source/RCS/msgdspn.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:27    Sheila Needham  
! $
! Revision 1.1  2003/05/02 14:56:39  usmdb
! Initial revision
!
!
! made from MSGDSP, operational May 2003
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! Element table:
!  (IREPL is dimensioned differently because IREPL(I) only corresponds
!   to DISPL(I) if QCREQ is false; otherwise IREPL(I/2) corresponds...

      INTEGER   NELEM               ! number of elements
      INTEGER   SEGMENT(NELEM)      ! segment number
      INTEGER   IBEFOR(NELEM)       ! bits before value (in segment)
      INTEGER   IREPL(*)            ! instance (or 1 if no replication)
      INTEGER   DISPL(NELEM)        ! bits before value (in message)

! Message structure (generic, as input):

      INTEGER   INPUT_NSEG          ! number of segments
      INTEGER   INPUT_SEGNUM(*)     ! segment number
      INTEGER   INPUT_NWIDTH(*)     ! width of replication count (or 0)
      INTEGER   INPUT_NREPLEN(*)    ! bits in segment (to replicate)

! Message structure (specific to this message) with extra columns:

      INTEGER   NSEG                ! number of segments
      INTEGER   SEGNUM(999)         ! segment number
      INTEGER   NWIDTH(999)         ! width of replication count (or 0)
      INTEGER   NREPLEN(999)        ! bits in segment (to replicate)
      INTEGER   NREPL(999)          ! replication count
      INTEGER   SEGSTAR(999)        ! number of bits before segment

      INTEGER   TRAILER_START       ! number of bits before trailer
      INTEGER   VALUE               ! function to get value from message
      INTEGER   I,J                 ! loop variables
      INTEGER   IBEFOUR             ! number of bits before section 4
      INTEGER   ELMSEG              ! segment number for this element
      INTEGER   MDI                 ! missing data indicator

      INTEGER   MAXLEV              ! dimension of LSEG & NTH
      PARAMETER (MAXLEV=3)          ! up to 3 levels of nesting
      INTEGER   LSEG(MAXLEV)        ! segment number at each level
      INTEGER   NTH(MAXLEV)         ! instance of each LSEG(L)

      INTEGER   NTHS                ! value of IREPL to set NTH
      INTEGER   LEVELS              ! number of nesting levels
      INTEGER   L                   ! nesting level in loop

      INTEGER   MSGSEG
      INTEGER   NSUBSEG
      INTEGER   NEXTRA
      INTEGER   NTIMES

      CHARACTER*(*) MESSAGE
      CHARACTER HEAD*132
      LOGICAL   LFLAG
      LOGICAL   QCREQ                                              !2.2
      LOGICAL   NESTED                                             !2.2
      LOGICAL   NOMATCH             ! set if n-th instance not found

      DATA    MDI/-9999999/

      SAVE

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/msgdspn.f,v $
     &'//'$ $Revision: 1$ $Date: 30/01/2006 20:23:27$ '

!-----------------------------------------------------------------------
! First make an array of displacements of segment starts in the message,
! building these up by adding count times bits replicated plus bits in
! count itself each time round the loop.
!
! VALUE is called to return the replication count for each segment
! if there is a replication - and the count is in the data.        !2.1
! But the count may be in the descriptor rather than the data      !2.1
! - if so, MESTRUCT sets NWIDTH to minus the count, so use that.   !2.1
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
      ENDDO

      NESTED=SEGNUM(NSEG).NE.NSEG
      I=1
      SEGSTAR(1)=0
      DO WHILE (I.LE.NSEG)
        IF (NWIDTH(I).EQ.0) THEN
          NREPL(I)=1
        ELSE IF (NWIDTH(I).GT.0) THEN                              !2.1
          NREPL(I)=VALUE(MESSAGE,SEGSTAR(I)-NWIDTH(I),NWIDTH(I))
        ELSE                                                       !2.1
          NREPL(I)=-NWIDTH(I)                                      !2.1
        ENDIF

! If there is nesting in the message & a count >1, then
! count subsegments (segment numbering shows which are subsegments)
! & replicate lines in message structure table,
! incrementing number of segments.

        NSUBSEG=1
        IF (NESTED .AND. NREPL(I).GT.1) THEN
          DO WHILE (SEGNUM(I+NSUBSEG).GT.SEGNUM(I)*10)
            NSUBSEG=NSUBSEG+1
          ENDDO

! Replicate in the usual way, by moving down the remaining lines
! & then repeating the subsegment lines to fill the gap.
! Set count width =0 (for copy below) to show that replication is done.

          NEXTRA=NSUBSEG*(NREPL(I)-1)
          IF (NEXTRA.GT.0) THEN
            DO J=NSEG,I+NSUBSEG,-1
              SEGNUM(J+NEXTRA)=SEGNUM(J)
              NWIDTH(J+NEXTRA)=NWIDTH(J)
              NREPLEN(J+NEXTRA)=NREPLEN(J)
            ENDDO

            NWIDTH(I)=0
            DO J=0,NEXTRA-1
              SEGNUM(I+NSUBSEG+J)=SEGNUM(I+J)
              NWIDTH(I+NSUBSEG+J)=NWIDTH(I+J)
              NREPLEN(I+NSUBSEG+J)=NREPLEN(I+J)
            ENDDO
            NSEG=NSEG+NEXTRA
          ENDIF
        ENDIF

! Find the start of the next segment.
! Ignore any negative NWIDTH, meaning no count in data.
!  (N.B. this is the next NWIDTH, not the one zeroed & copied above!)
! If NREPL=0, only the width of the count itself is added.

        IF (I.LT.NSEG) THEN
          IF (NESTED) THEN
            SEGSTAR(I+1)=SEGSTAR(I)
            IF (NREPL(I).GT.0) SEGSTAR(I+1)=SEGSTAR(I+1)+NREPLEN(I)
          ELSE
            SEGSTAR(I+1)=SEGSTAR(I)+NREPLEN(I)*NREPL(I)
          ENDIF
          IF (NWIDTH(I+1).GT.0) SEGSTAR(I+1)=SEGSTAR(I+1)+NWIDTH(I+1)
          IF (LFLAG) PRINT *,'MSGDSPN:',I+1,'th segment',
     &            SEGNUM(I+1),SEGSTAR(I+1),'bits before it'
        ENDIF
        I=I+1
      ENDDO

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

      DO I=1,NELEM
        ELMSEG=SEGMENT(I)

! ELMSEG=-999 indicates missing data if the element was not found or
! there is no QC flag (e.g. for report text): return missing data.

        IF (ELMSEG.EQ.-999) THEN
          DISPL(I)=MDI

! If ELMSEG<0 (but not -999, already tested),then the data is either
! report text (-99) or from BUFR section 1 (in an array in the main
! retrieval program).  Just pass on the negative indicator.

        ELSE IF (ELMSEG.LT.0) THEN
          DISPL(I)=ELMSEG

! If data is in trailer (-97 if nesting, 99 otherwise) use trailer
! start set up above.  (Nesting can lead to segment numbers >99!)

        ELSE IF (ELMSEG.EQ.-97 .OR. (.NOT.NESTED.AND.ELMSEG.EQ.99)) THEN
          DISPL(I)=TRAILER_START+IBEFOR(I)

! Set zero-replicated element to missing.

        ELSE IF (NREPL(ELMSEG).EQ.0) THEN
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
          ENDIF

! Find number of levels of nesting & set number & times for each.

          IF (NESTED) THEN   ! if there's any nesting
            LEVELS=1
            DO WHILE (NTHS/1000**LEVELS.GT.0)
              LEVELS=LEVELS+1
            ENDDO

            LSEG(LEVELS)=ELMSEG
            DO L=LEVELS,1,-1
              NTH(L)=MOD(NTHS,1000)
              NTHS=NTHS/1000
              IF (L.LT.LEVELS) LSEG(L)=LSEG(L+1)/10
            ENDDO

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
              DO WHILE (MSGSEG.LT.NSEG .AND. NTIMES.LT.NTH(L)
     &                  .AND. .NOT.NOMATCH)
                MSGSEG=MSGSEG+1
                IF (L.GT.1) THEN
                  IF (SEGNUM(MSGSEG).LT.LSEG(L-1)*10) NOMATCH=.TRUE.
                ENDIF
                IF (SEGNUM(MSGSEG).EQ.LSEG(L)) NTIMES=NTIMES+1
              ENDDO
            ENDDO

! If there was no NTH match at some level, return missing data.
! (NTIMES=0 if the count fell short at a level before the last.)

            IF (NOMATCH .OR. NTIMES.LT.NTH(LEVELS)) THEN
              DISPL(I)=MDI
            ELSE
              DISPL(I)=SEGSTAR(MSGSEG)+IBEFOR(I)
            ENDIF
          ELSE

! If there is no nesting, we still have to check that there is
! an NTHS replication in this message - if not, return missing.

            IF (NTHS.GT.NREPL(ELMSEG)) THEN
              DISPL(I)=MDI
            ELSE
             DISPL(I)=SEGSTAR(ELMSEG)+IBEFOR(I)+(NTHS-1)*NREPLEN(ELMSEG)
            ENDIF
          ENDIF
        ENDIF
        IF (LFLAG) PRINT *,'MSGDSPN:',I,'th value after',DISPL(I),'bits'
      ENDDO
      RETURN
      END
