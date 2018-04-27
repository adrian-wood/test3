      SUBROUTINE SYNBUL(POINT,BEND,TTAAII,CCCC,YYGGGG,
     &                  OCOR,CORNUM,MIMJ,NFT,BULL)                  !2.0

!-----------------------------------------------------------------------
!
! PROGRAM       : SYNBUL
!
!                (NOTE: PRE-PROCESSING STATEMENTS HAVE NOW BEEN REMOVED)
!
! PURPOSE       : FIND STARTS OF SYNOP REPORTS IN BULLETIN & GET DATE
!                 TIME FROM HEADING (YYGGIW RATHER THAN YYGGGG)
!
! CALLED BY     : MDBSTOR
!
! CALLS         : CCCODE,NCHTST,OBHOUR,SYNEND,REPEDT,EERROR,SYNOB
!
! PARAMETERS    : (1) POINT    WHERE TO START IN BULLETIN         (I)
!                    (AFTER MIMIMJMJ: III? YYGGIW? IIIII? CALL SIGN?)
!                 (2) BEND     POSN OF LAST CHARACTER IN BULLETIN (I)
!                 (3) TTAAII   TYPE OF BULLETIN                   (I)
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN    (I)
!                 (5) YYGGGG   TIME OF BULLETIN (DDHHMM)          (I)
!                 (6) OCOR     TRUE IF BULLETIN CORRECTED         (I)
!                 (7) CORNUM   NUMBER OF BULLETIN CORRECTION      (I)
!                 (8) MIMJ     MIMIMJMJ  (MAY BE 'OLDS')          (I)
!                 (9) NFT      FT NUMBER FOR SYNOP STORAGE        (I)
!                (10) BULL     BULLETIN                           (I)
!
! REVISION INFO :
!
! $Workfile: synbul.F$ $Folder: pre_refresh$
! $Revision: 4$ $Date: 07/05/2008 16:03:42$
!
! CHANGE RECORD :
!
! $Log:
!  4    Met_DB_Project 1.3         07/05/2008 16:03:42    Brian Barwell
!       Declaration and initialisation of CR and LF deleted as they are not
!       used.
!  3    Met_DB_Project 1.2         22/04/2008 12:03:16    Brian Barwell
!       Modified to handle mobile SYNOPS. Initialisation of CR and LF modified
!        and pre-processing statements deleted.
!  2    Met_DB_Project 1.1         01/05/2007 17:09:21    Brian Barwell
!       Reject reports longer than REPORT string (4096 bytes).
!  1    Met_DB_Project 1.0         30/01/2006 20:24:45    Sheila Needham  
! $
! Revision 2.3  2002/03/07  15:57:36  15:57:36  usmdb (Generic MetDB account)
! 18 March 2002     C Long
! 2.3  Don't just lose ships's ob if no equal sign: take end of bulletin
!      as delimiter (and reject ob - with print - if it is too long)
!
! Revision 2.2  2001/11/06  10:09:32  10:09:32  usmdb (Generic MetDB account)
! Another check added to prevent out of bounds problem on HP,
! plus some modifications to comments - S.Cox
!
! Revision 2.1  2001/09/05  09:00:46  09:00:46  usmdb (Generic MetDB account)
! Check added to prevent out of bounds error on HP - S.Cox
!
! Revision 2.0  2001/07/03 10:44:02  usmdb
! Change preprocessor statement to if (1 ), else... Separated variable
! declaration and initialisation. Removed unused dummy arguments
! AMDNUM and OAMD. Added copyright and modified header - S.Cox
!
! Revision 1.6  99/06/10  14:40:10  14:40:10  usmdb (Generic MetDB account)
! 21/06/99    C Long
! Accept ddhh/ after AAXX (only 5 figures accepted before);
! reject ob immediately, with message, if that group is bad.
!
! Revision 1.5  98/11/12  08:48:40  08:48:40  usmdb (Generic MDB account)
! Make allowence for OLDS bulletins to have report time in format ddhh00.
!
! Revision 1.4  98/01/29  11:29:48  11:29:48  usmdb (Generic MDB account)
! Addition of IBM preprocess directive
!
! Revision 1.3  97/11/05  10:07:40  10:07:40  uspm (Pat McCormack)
! ACCEPT REPORTS WITH 15 CHARACTERS FOR PASSING TO SYNEXP - J.Norton  !C
!
! Revision 1.2  1997/07/31 11:37:07  uspm
! First revision for 1
!
! Revision 1.1  1997/07/04 13:41:07  uspm
! Initial revision
!
! SEP 96 - ACCEPT YYGG WITH NO IW AFTER OLDS                          !A
!          DO NOT LOOK FOR EQUAL SIGN BEYOND NNNN!                    !B
!
! DEC 94 - CALL EERROR TO COPE WITH E E E
!
! NOV 94 - NO TIME ON 2ND LINE IF SHIP, JUST USE = TO DELIMIT OBS
!
! JUN 94 - FIND END OF REPORT IF EQUAL SIGN IS MISSING
!
! MAY 94 - REJECT BULLETIN IF DATES ON 1ST & 2ND LINE TOO FAR APART
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER BULL*(*)
      CHARACTER*4096 REPORT
      CHARACTER YYGGGG*6,TTAAII*6,CCCC*4,MIMJ*(*), YYGGIW*5
      CHARACTER*2 CORNUM                ! FOR WHOLE BULLETIN        !2.0
      CHARACTER*80 HEAD                 ! revision information       !2

      INTEGER IDATIM(5), DAY,HOUR,WBLK
      INTEGER IVALUE  ! FUNCTION TO TRANSLATE FROM CHAR TO INT
      INTEGER I, ICCCC, INUM, NFT, MISSIN, IRC
      INTEGER BEND,POINT,RSTART,REND,BLKSIZ,REPLEN                  !2.0
      INTEGER NTOL   ! TOLERANCE FOR (BULLETIN-OB.) TIME DIFFERENCE  !3
      LOGICAL OCOR,FIRST,LINEND                                     !2.0
      LOGICAL OLFCR,ONUM,OSPACE
      LOGICAL HEADSET                                               !2.2

      CHARACTER*1 SPACE                                              !4
      DATA SPACE/' '/,MISSIN/-9999999/
      DATA BLKSIZ/27998/                                            !2.0
      DATA HEADSET/.FALSE./                                         !2.2

!-----------------------------------------------------------------------
! REVISION INFORMATION AND INITIALISATIONS
!-----------------------------------------------------------------------

      IF (.NOT.HEADSET) THEN                                        !2.2
        HEAD = '$Workfile: synbul.F$ ' //
     &         '$Revision: 4$ $Date: 07/05/2008 16:03:42$'
        HEADSET=.TRUE.                                              !2.2
      ENDIF                                                         !2.2

      FIRST=.TRUE.
      IF (CCCC.NE.'    ') CALL CCCODE(287,ICCCC,CCCC)

!-----------------------------------------------------------------------
! MOVE PAST SPACES, CARRIAGE RETURNS AND LINEFEEDS (AND ANY OTHER
! TELECOMMUNICATIONS CHARACTERS ) BEFORE NEXT CHARACTER OF REPORT,
! SETTING "END OF LINE" IF FOUND.
!-----------------------------------------------------------------------

      LINEND=.FALSE.
5     CONTINUE
      IF (BULL(POINT:POINT).LE.SPACE)THEN
        IF (BULL(POINT:POINT).NE.SPACE) LINEND=.TRUE.
        POINT=POINT+1
        GOTO 5
      ENDIF

!-----------------------------------------------------------------------
! GET YYGGIW (5-FIGURE GROUP) FROM BULLETIN HEADING IF MIMJ IS GIVEN
! AND NOT FOLLOWED BY END OF LINE - AND IT IS NOT A SHIP OR MOBILE   !3
! SYNOP BULLETIN!                                                    !3
! IF IT IS OLD DATA (MIMJ='OLDS') ALLOW ONLY 4 FIGURES WITH NO IW,    !A
! AND STOP IMMEDIATELY IF THERE IS NO DATE/TIME AFTER 'OLDS'          !A
!-----------------------------------------------------------------------

      YYGGIW=' '
      IF (MIMJ.NE.' ' .AND. MIMJ.NE.'BBXX' .AND. MIMJ.NE.'OOXX'      !3
     &                .AND. .NOT.LINEND) THEN                        !3
        CALL NCHTST(POINT,7,ONUM,INUM,OSPACE,OLFCR,BULL)            !1.5
        IF (OSPACE.OR.OLFCR) THEN                                     !A
          IF (INUM.EQ.6) THEN                                         !A
            YYGGIW=BULL(POINT:POINT+4)
            POINT=POINT+6
          ELSE IF (INUM.EQ.5 .AND. MIMJ.EQ.'OLDS') THEN               !A
            YYGGIW=BULL(POINT:POINT+3)                                !A
            POINT=POINT+5                                             !A
          ELSE IF (INUM.EQ.7 .AND. MIMJ.EQ.'OLDS') THEN             !1.5
            YYGGIW=BULL(POINT:POINT+3)                              !1.5
            POINT=POINT+7                                           !1.5
        PRINT *,'SYNBUL: ddhh00 format IN OLDS BULLETIN>>',yyggiw,
     &           '<<',' point= ',point
          ENDIF

!-----------------------------------------------------------------------
! Accept 4 figures (ddhh) & slash (SYNEXP accepts a missing iw).    !1.6
! Give up now if bad ddhh & not end of line.                        !1.6
!-----------------------------------------------------------------------

        ELSE IF (INUM.EQ.5.AND.BULL(POINT+4:POINT+4).EQ.'/') THEN   !1.6
          YYGGIW=BULL(POINT:POINT+4)                                !1.6
          POINT=POINT+6                                             !1.6
        ENDIF

        IF (YYGGIW.EQ.' ') THEN                                     !1.6
          PRINT *,'SYNBUL: bad ddhh after AAXX:',                   !2.2
     &    BULL(1:MIN(60,BEND))                                      !2.2
          RETURN                                                    !1.6
        ENDIF                                                       !1.6
      ENDIF

!-----------------------------------------------------------------------
! GET DATE/TIME FROM YYGGIW RATHER THAN YYGGGG, UNLESS THEY ARE TOO
! FAR APART.  (FOR SHIPS AND MOBILE SYNOPS, YYGGIW IS WITH EACH      !3
! REPORT, SO LATER...)                                               !3
!-----------------------------------------------------------------------

      IDATIM(3)=IVALUE(YYGGGG(1:2))     ! DAY FROM BULLETIN HEADING
      IDATIM(4)=IVALUE(YYGGGG(3:4))     ! HOUR FROM HEADING (1ST LINE)
      IF (IDATIM(3).EQ.MISSIN) RETURN
      IF (IDATIM(4).EQ.MISSIN) RETURN

      IF(YYGGIW.NE.' ')THEN
        DAY=IVALUE(YYGGIW(1:2))         ! DAY FROM SECOND LINE
        HOUR=IVALUE(YYGGIW(3:4))        ! HOUR FROM SECOND LINE
      ELSE
        DAY=IVALUE(YYGGGG(1:2))         ! DAY FROM SECOND LINE
        HOUR=IVALUE(YYGGGG(3:4))        ! HOUR FROM SECOND LINE
      ENDIF

!-----------------------------------------------------------------------
! OBHOUR PUTS YEAR AND MONTH IN IDATIM AS WELL AS CHECKING BULLETIN
! AND SECOND LINE OF HEADING. USE INCREASED TOLERANCE FOR MOBSYN AS  !3
! DELAYED OBSERVATIONS ARE OFTEN RECEIVED.                           !3
!-----------------------------------------------------------------------

      NTOL = 3                       ! 3 hours for most data but ..  !3
      IF (MIMJ.EQ.'OOXX') NTOL = 24  ! 24 hours for mobile SYNOPs    !3
      CALL OBHOUR (IDATIM, DAY, HOUR, MIMJ, NTOL, IRC)               !3
      IF (IRC.NE.0) RETURN           ! Reject if difference too big
      IDATIM(5)=0                    ! 2nd line time with zero mins

!-----------------------------------------------------------------------
! MOVE PAST SPACES, CARRIAGE RETURNS AND LINEFEEDS BEFORE NEXT
! CHARACTER OF REPORT
!-----------------------------------------------------------------------

   10 IF (POINT.GE.BEND) RETURN
      IF (BULL(POINT:POINT).LE.SPACE)THEN
        POINT=POINT+1
        GOTO 10
      ENDIF
      RSTART=POINT

!-----------------------------------------------------------------------
! IF 'NIL' NEAR START, MOVE POINT PAST IT AND TRY NEXT REPORT
!-----------------------------------------------------------------------

      I=INDEX(BULL(RSTART:MIN(RSTART+20,BEND)),'NIL')               !2.1
      IF (I.GT.0)THEN
        POINT=RSTART+I+3
        GOTO 10
      ENDIF

!-----------------------------------------------------------------------
! Ships and mobile SYNOPs only:                                      !3
! Delimit reports by equal signs.  If no equal sign, take end of   !2.3
! bulletin as end of report unless report would be too short (so   !2.3
! nothing worthwhile left) or too long (equal sign(s) missing?).   !2.3
!-----------------------------------------------------------------------

      REPLEN=0                  ! TO SKIP REPORT IF REPLEN NOT SET
      IF (MIMJ.EQ.'BBXX' .OR. MIMJ.EQ.'OOXX') THEN                   !3
        REPLEN=INDEX(BULL(RSTART:BEND),'=')-1                      !2.3
        IF (REPLEN.GT.0) THEN                 ! equal sign found   !2.3
          REND=RSTART+REPLEN-1                                     !2.3
        ELSE IF (BEND-RSTART.GT.300) THEN     ! no '=': too long   !2.3
          PRINT *,'SYNBUL:',BEND-RSTART,'-byte ship ob too long! ',!2.3
     & 'No equal signs?  Bulletin is: ',TTAAII,' ',CCCC,' ',YYGGGG !2.3
          PRINT *,'Report starts: ',BULL(RSTART:RSTART+100)        !2.3
          RETURN                                                   !2.3
        ELSE IF (BEND-RSTART.LT.30) THEN      ! no '=': too short  !2.3
          RETURN                                                   !2.3
        ELSE                                  ! rest of bulletin   !2.3
          REND=BEND                                                !2.3
          REPLEN=REND-RSTART+1                                     !2.3
        ENDIF                                                      !2.3

!-----------------------------------------------------------------------
! FIRST GROUP OF LAND REPORT COULD BE 3-FIG OR 5-FIG STATION NUMBER.
! IF IT IS 3 FIGS AND TTAAII SAYS UK DATA, INSERT 03.  (DO THIS HERE
! SO THAT BLOCK NUMBER CAN BE USED BY SYNEND TO DELIMIT REPORTS.)
!-----------------------------------------------------------------------

      ELSE
        CALL NCHTST(POINT,6,ONUM,INUM,OSPACE,OLFCR,BULL)
        IF (INUM.EQ.4 .AND. (OSPACE.OR.OLFCR))THEN
          IF (TTAAII(3:4).EQ.'UK') THEN
            RSTART=RSTART-2
            BULL(RSTART:RSTART+1)='03'
            INUM=6
          ENDIF
        ENDIF

!-----------------------------------------------------------------------
! FIND END OF REPORT, KEEPING BLOCK NUMBER OF FIRST STATION IN BULLETIN
! TO CHECK FOR NEW REPORT WHEN '=' MISSING, BEFORE EDITING OUT CRLF.
! NOTE THAT 'REPORT' STRING IS 4096 BYTES LONG SO CHECK THAT ACTUAL  !2
! REPORT IS NOT LONGER. (ADDED AFTER MDBSYNOP CRASH, 30/04/07.)      !2
!-----------------------------------------------------------------------

        IF (INUM.EQ.6 .AND. (OSPACE.OR.OLFCR))THEN
          IF (FIRST) WBLK=IVALUE(BULL(RSTART:RSTART+1))
          CALL SYNEND(RSTART,BEND,REND,WBLK,BULL)
          REPLEN=REND-RSTART+1
        ENDIF
      ENDIF

      IF (REPLEN.GE.15) THEN                                          !C
        CALL EERROR(BULL(RSTART:REND),IRC)
        IF (REPLEN.GT.4096) THEN                                     !2
          WRITE (6,'(T5,A,I7,2A,1X,A,1X,A)') 'SYNBUL:', REPLEN,      !2
     &             '-byte land ob too long! - ', TTAAII,CCCC,YYGGGG  !2
          RETURN                                                     !2
        END IF                                                       !2
        CALL REPEDT(BULL(RSTART:REND),REPORT,REPLEN)

!-----------------------------------------------------------------------
! EXPAND, ENCODE & STORE, THEN GO ON TO NEXT REPORT
!-----------------------------------------------------------------------

        IF (MIMJ.EQ.' ') MIMJ='AAXX'
        CALL SYNOB(REPORT(1:REPLEN),REPLEN,OCOR,CORNUM,
     &    YYGGIW,IDATIM,TTAAII,CCCC,ICCCC,MIMJ,NFT,BLKSIZ)
        FIRST=.FALSE.
        POINT=REND+2
      ELSE                              ! NOT 3 OR 5 FIGS, TRY NEXT OB
        I=INDEX(BULL(POINT:BEND),'=')
        IF (I.EQ.0) RETURN
        POINT=POINT+I
      ENDIF

!-----------------------------------------------------------------------
! IF MORE DATA TO BE PROCESSED, GO BACK TO START
!-----------------------------------------------------------------------

      IF (POINT.LT.BEND) GOTO 10

      RETURN
      END
