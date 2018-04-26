      SUBROUTINE TAFIND(POINT,BEND,OAMD,OCOR,AMDNUM,CORNUM,TTAAII,
     &                  CCCC,YYGGGG,GGGG,ICC,NFT,BLKSIZ,OERR,BULL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : TAFIND
!
! PURPOSE       : TO PASS A TAF OR METAR TO THE APPROPRIATE STORAGE
!                 PROGRAM, COMPLETING ANY 3-LETTER IDENTIFIER AND
!                 STARTING AN INDEX ENTRY (AFTER FURTHER CHECKS
!                 FOR COR, AMD & NIL).
!
! CALLED BY     : METBUL, TAFBUL & SAMBUL
!
! CALLS         : TAFREP, ZPDATE(DATE13,DATE31),DATIM
!
! PARAMETERS    : (1) POINT    PASSED AS POINTER TO START OF REPORT,
!                              RETURNED AS NEXT CHARACTER TO HANDLE.
!                 (2) BEND     NUMBER OF LAST CHARACTER IN BULLETIN
!                 (3) OAMD     LOGICAL SET IF REPORT AMENDED
!                 (4) OCOR     LOGICAL SET IF REPORT CORRECTED
!                 (5) AMDNUM   NUMBER OF AMENDMENT
!                 (6) CORNUM   NUMBER OF CORRECTION
!                 (7) TTAAII   TT TYPE OF BULLETIN (SA IS METAR)
!                 (8) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (9) YYGGGG   BULLETIN DATE/TIME GROUP
!                (10) GGGG     REPORT TIME GROUP (SPACES IF NONE)
!                (11) ICC      NUMBER OF LETTERS IN STATION NAME
!                (12) NFT      FT NUMBER FOR STORAGE OF THIS DATA
!                (13) BLKSIZ   BLOCK SIZE OF DATA SET FOR THIS DATA
!                (14) OERR     LOGICAL SET IF ERROR IN REPORT (IF 3-
!                               LETTER ID & CCCC NOT 'C...' OR 'K...')
!                (15) BULL     REPORT DATA
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:12$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tafind.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:12    Sheila Needham  
! $
! Revision 2.4  2004/10/04 08:38:15  usmdb
! 2.4.  18 October 2004.  Brian Barwell.  Change 55/04.
! Relax check on time of issue to prevent rejection of 00Z ATAFS.
!
! Revision 2.3  2003/03/06  09:16:03  09:16:03  usmdb (MetDB account c/o John C
!   Ward)
! Prevent out of bounds when outputting BULL - S.Cox
!
! Revision 2.2  2002/01/16  09:47:37  09:47:37  usmdb (MetDB account c/o usjh)
! 19 Nov 2001    C.Long
! 2.2  Remove check for 'AMD' in any context (including 'no AMD'!)
!      Change tolerance to store METARs received up to 20 minutes
!      before the nominal time.
!
! Revision 2.1  2001/09/05  09:01:47  09:01:47  usmdb (Generic MetDB account)
! Added out of bounds safety check - S.Cox
! Print message when 3-letter ident is completed from CCCC - C.Long
!
! Revision 2.0  2001/07/03  10:44:12  10:44:12  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.6  2000/11/07  12:12:05  12:12:05  usmdb (Generic MetDB account)
! 20NOV00 R Hirst
! If subtype SPECI, set ENTRY(17) to CHAR(3)
!
! Revision 1.5  99/02/11  12:03:23  12:03:23  usmdb (Generic MDB account)
! 15 Feb 99   C Long
! Improve January change (time handling & prints)
!
! Revision 1.4  99/01/18  11:13:55  11:13:55  usmdb (Generic MDB account
! 15 Feb 99       C Long
! Correct error returns in 1.3 change (to avoid loop!)
!
! Revision 1.3  99/01/14  13:58:24  13:58:24  usmdb (Generic MDB account
! 18 Jan 1999     C Long
! Reject any METAR more than 10 minutes in the future. Only set METAR
! day back to yesterday if that keeps well away from 24 hours ago.
!
! Revision 1.2  97/07/31  11:41:39  11:41:39  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 14:05:31  uspm
! Initial revision
!
! 03/06/96 (A.M) REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY A NEW
!                PARAMETER 'BULL'.
!
!  JAN 96 : ADD CHECK ON VALIDITY OF DAY AND MINUTE OF YYGGGG         !A
!
! JUNE 94 : ADD SIXTH ARGUMENT (IDENT) TO TAFREP CALL
!
! JULY 93 : CHECK FOR BAD HOUR IF TIME GROUP IN REPORT
!
!  MAY 93 : USE 2 FLAGS IN INDEX ENTRY TO INDICATE SHORT & LONG TAFS
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

      CHARACTER*(*) BULL
*
      INTEGER POINT,BEND, BLKSIZ
      LOGICAL OAMD,OCOR, OERR
      CHARACTER*2 AMDNUM,CORNUM
      CHARACTER*6 TTAAII,YYGGGG
      CHARACTER*4 CCCC,GGGG
*
      INTEGER RSTART,REND
      INTEGER DATIME(5),NOW(9)
      CHARACTER*23 ENTRY
      CHARACTER*132 HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tafind.f,v $
     &'//'$ $Date: 30/01/2006 20:25:12$ $Revision: 1$'

*
* SET MISSING POSITION IN INDEX ENTRY: IT WILL BE LOOKED UP IN TAFREP.
*
      ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)
      OERR=.FALSE.
***********************************************************************
*                                                                     *
* FIND END OF REPORT (NEXT EQUAL SIGN).  IF NO EQUAL SIGN, RETURN     *
*                                                                     *
***********************************************************************
      RSTART=POINT
      REND=RSTART+INDEX(BULL(RSTART:BEND),'=')-2
      IF (REND.LE.RSTART .OR. REND.GT.BEND) THEN
        POINT=BEND
        OERR=.TRUE.
        RETURN
      ENDIF
***********************************************************************
*                                                                     *
* RETURN IF 'NIL'.  IF 'COR' FOUND ANYWHERE, SET COUNT.               *
* TAFBUL checks for COR in various places, so this check shouldn't !2.2
* be needed, though perhaps it does no harm.  A similar check for  !2.2
* AMD was removed, as it set the flag regardless of context, e.g.  !2.2
* even from 'no AMD '!                                             !2.2
*                                                                     *
***********************************************************************
      IF (INDEX(BULL(RSTART+4:RSTART+16),'NIL').GT.0) GO TO 999
*
      NAMD=0
      IF (AMDNUM.GT.'01') THEN
        NAMD=ICHAR(AMDNUM(2:2))-ICHAR('0')
      ELSE IF (OAMD) THEN                                          !2.2
        NAMD=1
      ENDIF
*
      NCOR=0
      IF (.NOT.OCOR) ICOR=INDEX(BULL(RSTART+4:REND),' COR ')
      IF (CORNUM.GT.'01') THEN
        NCOR=ICHAR(CORNUM(2:2))-ICHAR('0')
      ELSE IF (OCOR .OR. ICOR.GT.0) THEN
        NCOR=1
      ENDIF
***********************************************************************
*                                                                     *
* IF THE COLLECTING CENTRE IS C... OR K... (CANADA OR USA),           *
* COMPLETE A 3-LETTER IDENTIFIER WITH C OR K RESPECTIVELY.            *
* IF THE COLLECTING CENTRE IS CROUGHTON (EGWR), ASSUME A 3-LETTER     *
* IDENTIFIER STARTING WITH Y IS CANADIAN, SET ANY OTHER TO USA.       *
* (THIS IS RISKY: CU, CW, CY & CZ ARE CANADIAN REGIONS, KW, KY & KZ   *
*  ARE US REGIONS; BUT CY HAS LOTS OF AIRFIELDS, KY VERY FEW...)      *
*                                                                     *
***********************************************************************
      IF (ICC.EQ.3) THEN
        IF (CCCC(1:1).EQ.'C' .OR. CCCC(1:1).EQ.'K') THEN           !2.1
          RSTART=RSTART-1
          BULL(RSTART:RSTART)=CCCC(1:1)
        ELSE IF (BULL(RSTART:RSTART+1).EQ.CCCC(2:3)) THEN          !2.1
          RSTART=RSTART-1                                          !2.1
          BULL(RSTART:RSTART)=CCCC(1:1)                            !2.1
          PRINT *,'Only 3-letter ident:',BULL(RSTART+1:RSTART+3)   !2.1
          PRINT *,'Assume it should be:',BULL(RSTART:RSTART+3),    !2.1
     &            '(with middle letters as in CCCC)'               !2.1
          PRINT *,BULL(RSTART:MIN(RSTART+80,BEND))                 !2.1
        ELSE IF (CCCC.EQ.'EGWR') THEN
          RSTART=RSTART-1
          IF (BULL(RSTART+1:RSTART+1).EQ.'Y') THEN
            BULL(RSTART:RSTART)='C'
          ELSE
            BULL(RSTART:RSTART)='K'
          ENDIF
        ELSE
          PRINT *,BULL(RSTART:RSTART+2),' - BUT CCCC IS ',CCCC
          OERR=.TRUE.
          GO TO 999
        ENDIF
      ENDIF
***********************************************************************
*                                                                     *
* PASS REPORT AND ASSOCIATED INFORMATION ON FOR STORING IN DATABASE,  *
* SETTING DATE/TIME FROM YYGGGG, GGGG & CURRENT YEAR/MONTH/DAY AND    *
* MAKING CHARACTER STRING OF TTAA11, AMD/COR & CCCC TO GO IN INDEX.   *
*                                                                     *
* TAKE CURRENT YEAR & MONTH, DAY FROM BULLETIN & TIME FROM REPORT     *
* UNLESS (1) 0Z DATA FOR 1ST BEFORE 0Z (MAY BE NEXT MONTH)            *
*        (2) BULLETIN DAY > CURRENT DAY (0Z OB OR LAST MONTH'S DATA)  *
*        (3) REPORT HOUR > BULLETIN HOUR (REPORT FOR DAY BEFORE)      *
* (ASSUME EARLY 0Z DATA IF DATA TIME <0010 & CURRENT TIME >2330)      *
*                                                                     *
***********************************************************************
      CALL DATIM(NOW)                             ! CURRENT DATE
      DATIME(1)=NOW(8)                            ! YEAR
      DATIME(2)=NOW(7)                            ! MONTH
*
* CHECK YY FALLS WITHIN A VALID DAY RANGE
*
      READ (YYGGGG(1:2),'(I2)') DATIME(3)         ! DAY
      IF (DATIME(3).GT.31 .OR. DATIME(3).LT.1) THEN               !1.5
        WRITE (6,*) 'BAD TIME GROUP (YY): ',YYGGGG(1:2),          !A
     &  BULL(RSTART:MIN(RSTART+50,BEND))                          !2.3
        GO TO 999                                                 !A
      ENDIF                                                       !A
*
* READ TIME (HOURS AND MINUTES) IF NOT ALREADY SET
*
      IF (GGGG.EQ.'    ') GGGG=YYGGGG(3:6)
*
* CHECK GGGG FALLS WITHIN VALID TIME RANGE
*
      READ (GGGG(1:2),'(I2)') DATIME(4)           ! HOUR
      READ (GGGG(3:4),'(I2)') DATIME(5)           ! MINUTE
      IF (DATIME(4) .GE. 24 .OR. DATIME(5) .GE. 60) THEN             !A
        PRINT *,'BAD TIME GROUP: ',GGGG,'   ',
     &           BULL(RSTART:MIN(RSTART+50,BEND))                  !2.3
        GO TO 999
      ENDIF
*
* If bulletin is for 0Z on 1st & current date is end of month,     !1.5
* then assume it's next month's data.                              !1.5
*
      IF (YYGGGG(1:4).EQ.'0100' .AND. NOW(6).GE.28) THEN
        CALL DATE31(NOW(6),NOW(7),NOW(8),NCENDY)  ! CURRENT CENTURY-DAY
        CALL DATE13(NCENDY+1,NDAY,NMONTH,NYEAR)   ! TOMORROW'S DATE
        IF (NDAY.EQ.1) THEN                       ! IF IT'S THE FIRST,
          DATIME(2)=NMONTH                        ! NEXT MONTH'S DATA
          DATIME(1)=NYEAR
        ENDIF
!
! If day of data is greater than current day (and it's not a       !2.4
! midnight bulletin received just before 00Z), change the month    !2.4
! to last month to avoid giving TAFREP a date in the future!       !2.4
! 'Just before 00Z' means after 22Z the previous day. It used to   !2.4
! be 23:30Z but was changed to avoid rejection of 00Z ATAF data.   !2.4
!
      ELSE IF (DATIME(3).GT.NOW(6)) THEN
        IF (.NOT.(DATIME(3).EQ.NOW(6)+1.AND.YYGGGG(3:6).LE.'0010'  !1.5
     &             .AND. NOW(5).GE.22)) THEN                       !2.4
          DATIME(2)=DATIME(2)-1                ! LAST MONTH'S DATA
          IF (DATIME(2).EQ.0) THEN
            DATIME(2)=12                       ! DECEMBER
            DATIME(1)=DATIME(1)-1              ! LAST YEAR
          ENDIF
        ENDIF

! The check below is for METARs only (GGGG=YYGGGG(3:6) for TAFs).
! If the report hour is greater than the bulletin hour, it might be
! yesterday's data (or it might be a mistake!): say yesterday if 12
! hours or less between report & bulletin, where 12 is arbitrary
! (most METARs are received within a few hours, METARs from 24
! hours ago are very unlikely - beware of bulletin time a few
! minutes before report time!)

      ELSE IF (GGGG(1:2).GT.YYGGGG(3:4)) THEN
        READ (YYGGGG(3:4),'(I2)') BULHOUR                           !1.3
        IF (24-DATIME(4)+BULHOUR.LT.12) THEN                        !1.3
          CALL DATE31(DATIME(3),NOW(7),NOW(8),NCENDY)
          CALL DATE13(NCENDY-1,DATIME(3),DATIME(2),DATIME(1))
        ELSE                                                        !1.3
          PRINT *,'TAFIND: METAR time wrong?:',GGGG,' ',YYGGGG,' ', !1.5
     &            BULL(RSTART:MIN(RSTART+80,BEND))                  !2.1
          OERR=.TRUE.                                               !1.4
          GO TO 999                                                 !1.4
        ENDIF                                                       !1.3
      ENDIF

! Finally reject any METAR more than twenty minutes in the future.  !2.1
! (only if it's data for today!)                                    !1.5
! (Assume SPECIs don't come in before the nominal time.)            !2.1

      IF (TTAAII(1:2).EQ.'SA' .AND. DATIME(3).EQ.NOW(6) .AND.       !1.5
     &    DATIME(4)*60+DATIME(5)-(NOW(5)*60+NOW(4)).GT.20) THEN     !2.1
        PRINT *,'TAFIND: METAR in future ',GGGG,' ',YYGGGG,' ',     !1.5
     &            BULL(RSTART:MIN(RSTART+80,BEND))                  !2.1
        OERR=.TRUE.                                                 !1.4
        GO TO 999                                                   !1.4
      ENDIF

      IF (TTAAII(1:2).EQ.'SA') THEN
        ENTRY(17:17)=CHAR(0)                         ! METAR
      ELSE IF (TTAAII(1:2).EQ.'FT') THEN
        ENTRY(17:17)=CHAR(1)                         ! LONG TAF
      ELSE IF (TTAAII(1:2).EQ.'FC') THEN
        ENTRY(17:17)=CHAR(2)                         ! SHORT TAF
      ELSE IF (TTAAII(1:2).EQ.'SP') THEN                            !1.6
        ENTRY(17:17)=CHAR(3)                         ! SPECI        !1.6
      ENDIF
*
      ENTRY(3:11)=TTAAII(3:6)//CHAR(NAMD*16+NCOR)//CCCC
      CALL TAFREP(DATIME,ENTRY,BULL(RSTART:REND),NFT,BLKSIZ,
     &                         BULL(RSTART:RSTART+4))
*
  999 POINT=REND+1
      RETURN
      END
