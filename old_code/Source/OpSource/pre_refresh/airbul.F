      SUBROUTINE AIRBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,NFTAIR,
     &                  NFTBCN,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRBUL
!
! PURPOSE       : To split a bulletin of Airep reports into single
!                 Airep messages before being passed to the decode
!                 routines.
!
! DESCRIPTION   : Using various counters and flags the end of report
!                 delimeter is checked for. The length, starting and
!                 ending position are retained. This process repeats
!                 throughout the bulletin - passing the single report
!                 to the decode routines. Once the end of the bulletin
!                 is reached control passes back to synopt to read-in
!                 the next bulletin.
!
! CALLED BY     : MDBSTOR                                           !2.0
!
! CALLS TO      : AIRLAG, BULLED, AIRARP, DREG, IVALUE             !2.5
!
! PARAMETERS    : (1) pointer to space after last group found by SYNHED
!                      (If group is 'AIREP', ddhh follows)         !2.5
!                 (2) pointer to end of bulletin
!                 (3) TTAAii from bulletin heading
!                 (4) CCCC from bulletin heading
!                 (5) YYGGgg from bulletin heading
!                 (6) FT number for storage
!                 (7) FT number for beacon index
!                 (8) bulletin (still with CR & LF)                !2.5
!
! REVISION INFO :
!
! $Revision: 2$ $Author: Brian Barwell$
! $Date: 25/05/2006 16:22:37$
! $Folder: pre_refresh$ $Workfile: airbul.F$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         25/05/2006 16:22:37    Brian Barwell
!       Additional test to prevent crashes in INDEX function.
!  1    Met_DB_Project 1.0         30/01/2006 20:20:35    Sheila Needham  
! $
! Revision 2.5  2003/01/06 16:05:54  usmdb
! 20 Jan 2003    C Long
! 2.5  Skip day/hour group if bulletin has second line starting AIREP.
!
! Revision 2.4  2002/06/10  15:07:24  15:07:24  usmdb
! 17 June 2002    C Long
! 2.4  The tail number part of change 2.2 didn't work because the
!      bulletins concerned have no equal signs to delimit reports.
!      Assume 2 lines per report unless equal signs found.
! 2.4a Adding REPLEN to get to the next report can only work if
!      BULLED doesn't change REPLEN, i.e. if the report has only
!      one line (luckily true for most AIREPs).  Use L instead.
!
! Revision 2.3  2002/03/13  09:59:59  09:59:59  usmdb (Generic MetDB account)
! 18 March 2002
! 2.3  Check length against max before copying to REPORT
!
! Revision 2.2  2002/03/07  15:55:24  15:55:24  usmdb (Generic MetDB account)
! 18 March 2002     C Long
! 2.2 Call BULLED for each report rather than whole bulletin,
!     after looking for second line which may start with tail number.
!     Restructure program to find equal signs with INDEX,
!     using input POINT as starting point rather than always 36.
! 2.2a Pass bulletin time span (from AIRLAG) to AIRARP/ENC/IND.
!
! Revision 2.1  2001/10/17  13:24:13  13:24:13  usmdb (Generic MetDB account)
! For non-IBM mainframe platforms, initialise POINT so it is past
! the end of the bulletin header as for IBM mainframe code - S.Cox
!
! Revision 2.0  2001/05/31 13:27:18  usmdb
! Removed unused variables and out of date comments. Removed
! unused argument CORF to AIRARP. Added copyright and
! modified header - S.Cox
!
! Revision 1.4  98/09/16  16:11:02  16:11:02  usmdb (Generic MetDB accou
! 21/09/1998 Additional checks to stop 'out-of-bounds' conditions.
! v(G)=8 ev(G)=1 Jon Lewthwaite
!
! Revision 1.3  97/09/22  13:21:34  13:21:34  uspm (Pat McCormack)
! ensure check for report does not begin on a blank character
!
! Revision 1.2  1997/07/31 09:05:39  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/03 13:50:08  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER BULEND        !pointer to end of bulletin
      INTEGER I             !used in loop to find second line      !2.2
      INTEGER IVALUE        !function to convert figures to number !2.5
      INTEGER IN            !start of tail number                  !2.4
      INTEGER L             !report length (for BULLED call)       !2.4
      INTEGER NL            !line number (1 or 2)                  !2.4
      INTEGER NEWLINE(2)    !start of 2nd line or 3rd (if any)     !2.4
      INTEGER NFTAIR        !FT no for airep storage
      INTEGER NFTBCN        !FT no for beacons dataset
      INTEGER POINT         !pointer to end of bulletin heading
      INTEGER REPLEN        !length of airep report
      INTEGER RSTART        !pointer to start of report
      INTEGER RC            !return code from AIRLAG               !2.2a
      INTEGER SPAN          !time span of reports in bulletin      !2.2a
      INTEGER NOW(8)        !to pass time to DREG                  !2.2a
      INTEGER TAILEND       !length of tail number                 !2.4

      LOGICAL FIRST         !Flag for first call to this routine      !2

      CHARACTER*4096 BULL   !bulletin report
      CHARACTER*120  REPORT !airep report from bulletin
      CHARACTER*4    CCCC   !collecting centre
      CHARACTER*6    YYGGGG !date/time of bulletin
      CHARACTER*6    TTAAII !bulletin id header
      CHARACTER*8    TAILNO !tail number (if this is on 2nd line)  !2.2
      CHARACTER*80   HEAD   !revision information                     !2

      DATA FIRST /.TRUE./                                             !2

      SAVE
!                                                Revision information
      IF (FIRST) THEN                                                 !2
        HEAD = '$Workfile: airbul.F$ ' //
     &         '$Revision: 2$ $Date: 25/05/2006 16:22:37$'
        FIRST = .FALSE.                                               !2
      END IF                                                          !2

! POINT may point to the first report in the bulletin (or rather   !2.5
! the space before it).  But some bulletins (especially from KAWN) !2.5
! have a 2nd line "AIREP ddhh".  In that case the word AIREP has   !2.5
! already been skipped by SYNHED, so skip a 4-figure group too     !2.5
! if it's followed by an end-of-line character (less than space).  !2.5

      IF (BULL(POINT+5:POINT+5).LT.' ' .AND.                       !2.5
     &    IVALUE(BULL(POINT+1:POINT+4)).GT.0) POINT=POINT+5        !2.5

! Find time span (in hours) of reports in bulletin.                !2.2a
! This will be stored in trailer & retrievable as BLTN_TIME_SPAN.  !2.2a

      CALL AIRLAG(BULL(POINT:BULEND),YYGGGG,SPAN,RC)               !2.3

! Dreg bulletin if span is >22 hours (but go on to store obs).     !2.2a
! (22 is the threshold mentioned in the original task 494, with a  !2.2a
! note that it might be changed in the light of experience to 18.) !2.2a
! (Last DREG argument is 1-minute DSN, not worth passing?)         !2.2a

      IF (SPAN.GT.22) THEN                                         !2.2a
        CALL DATIM(NOW)                                            !2.2a
#if defined (MVS)
        CALL DREG(22,' Obs span >22 hours',BULL,'AIRBUL',          !2.2a
     &            'AIREP',BULL(15:32),NOW,' ')                     !2.3
#endif
        PRINT *,'AIRBUL: obs in bulletin spanned >22 hours.'       !2.2a
        PRINT *,' Will be stored, but see dregs. ',BULL(1:BULEND)  !2.3
      ENDIF                                                        !2.2a

! Split the bulletin into reports ending with equal signs.

      RSTART=POINT           ! past bulletin heading & any MiMiMjMj
      REPLEN=BULEND-RSTART
      DO WHILE (RSTART.LT.BULEND .AND. REPLEN.GT.0)

! Skip any further CR, LF or space before the start of the report,
! returning if that takes us to the end of the bulletin.

        DO WHILE (BULL(RSTART:RSTART).LE.' ')
          RSTART=RSTART+1    ! skip any futher CR, LF or space
        ENDDO
        IF (RSTART.GE.BULEND) RETURN

! Look for '=' as delimiter.  Look at rest of bulletin if none.    !2.4

        REPLEN=INDEX(BULL(RSTART:BULEND),'=')
        IF (REPLEN.EQ.0) REPLEN=BULEND-RSTART                      !2.4
        TAILNO=' '                                                 !2.4

! If this is a bulletin with no delimiters & tail number on a      !2.4
! second line, look for the ends of the next two lines             !2.4
! (keeping tail number & maybe resetting REPLEN).                  !2.4
! Assume each report has 2 lines & only 2 lines; tail number is    !2.4
! first (but maybe not only) group on second line.                 !2.4

        IF (TTAAII.EQ.'UANT99' .AND.                               !2.4
     &          (CCCC.EQ.'KDDL' .OR. CCCC.EQ.'CWAO')) THEN         !2.4
          I=1                                                      !2.4
          NL=1                                                     !2.4
          NEWLINE(1)=0         ! start of second line              !2.4
          NEWLINE(2)=0         ! start of third line (next report) !2.4
          DO WHILE (I.LT.REPLEN .AND. NEWLINE(2).EQ.0)             !2.4
            IF (BULL(RSTART+I:RSTART+I).LT.' ') THEN               !2.4

! If end of line found, skip any further space or end of line.     !2.4

              DO WHILE (BULL(RSTART+I+1:RSTART+I+1).LE.' ')        !2.4
                I=I+1                                              !2.4
              ENDDO                                                !2.4
              NEWLINE(NL)=I+1  ! keep start of new line            !2.4
              NL=NL+1          ! next line                         !2.4
            ENDIF                                                  !2.4
            I=I+1                                                  !2.4
          ENDDO                                                    !2.4

! If a second line was found, take the first (or only) group as    !2.4
! the tail number.  If no second line, carry on with no tail no.   !2.4
! ("NEWLINE(1).LE.REPLEN" was added to the test below after job       !2
! crashes caused by bulletins with "DUPE" as a third line.)           !2

          IF (NEWLINE(2).GT.0) REPLEN=NEWLINE(2)-1                 !2.4
          IF (NEWLINE(1).GT.0 .AND. NEWLINE(1).LE.REPLEN) THEN        !2
            IN=RSTART+NEWLINE(1)                                   !2.4
            TAILEND=INDEX(BULL(IN:RSTART+REPLEN),' ')              !2.4
            IF (TAILEND.GT.0) THEN                                 !2.4
              TAILNO=BULL(IN:IN+TAILEND)                           !2.4
            ELSE                                                   !2.4
              TAILNO=BULL(IN:RSTART+REPLEN-1)                      !2.4
            ENDIF                                                  !2.4
          ENDIF                                                    !2.4
        ENDIF                                                      !2.4

! Copy the report to another string for editing (unless it's too long)
! (Print bulletin heading if long ob - may be no '=' in bulletin)  !2.4

        IF (REPLEN.GT.LEN(REPORT)) THEN                            !2.4
          WRITE(6,*)'AIRBUL: report rejected - too long',REPLEN,   !2.4
     &       '         ',TTAAII,' ',CCCC,' ',YYGGGG                !2.4
          PRINT *,BULL(RSTART:RSTART+REPLEN-1)                     !2.3
        ELSE IF (REPLEN.GT.0) THEN                                 !2.3
          REPORT=BULL(RSTART:RSTART+REPLEN-1)

! Now that any tail number has been kept, edit this report to      !2.2
! set new line characters to spaces & remove repeated spaces.      !2.2
! (BULLED will change length if REPORT has more than one line;     !2.4a
!  use L so that REPLEN can still be added to RSTART at end.       !2.4a

          L=REPLEN                                                 !2.4a
          CALL BULLED(1,L,REPORT)                                  !2.4a

! Reject any report which is really an AMDAR.                      !2.3
! (Can't easily do this until BULLED called: 333 may start line!)  !2.3

          IF (INDEX(REPORT(1:L),' 333 ').EQ.0) THEN                !2.4a
            CALL AIRARP(L,REPORT,TAILNO,YYGGGG,CCCC,               !2.4a
     &                      TTAAII,NFTAIR,NFTBCN,SPAN)             !2.2a
          ENDIF
        ENDIF

! Move on to next report in bulletin

        RSTART=RSTART+REPLEN+1     ! past '=' & next character     !2.2
      ENDDO
      RETURN
      END
