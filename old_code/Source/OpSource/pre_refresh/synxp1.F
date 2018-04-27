      SUBROUTINE SYNXP1(REPORT,POINT,NGRPS,SYNTAX,IWMOB,
     &  WXMAN,TTT,TDTDTD,UUU,P0P0P0,PPPP,A3,HHH,ATEND,
     &  PCHANG,RRR,TR,WW,W1,W2,NH,CL,CM,CH,HOUR,MINS,IX,IDATIM)   !2.1!A

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNXP1
!
! PURPOSE       : TO EXPAND SECTION 1 OF A SYNOP REPORT AFTER THE
!                 MANDATORY GROUPS.
!
! DESCRIPTION   : LOOPS OVER THE GROUPS IN THE SECTION.
!                 IDENTIFIES A GROUP BY THE FIRST DIGIT.
!                 RETURNS REAL VALUES IN 'BUFR' UNITS.
!
! CALLED BY     : SYNEXP
!
! CALLS         : IVALUE (FUNCTION)
!
! PARAMETERS    : (1) REPORT CHAR    COMPLETE REPORT              (I)
!                 (2) POINT  INTEGER POINTER TO NEXT GROUP        (I)
!                 (3) NGRPS  INTEGER GROUP COUNT FOR SECTION      (I)
!                 (4) SYNTAX LOGICAL TRUE IF SYNTAX ERROR FOUND   (O)
!                 (5) IWMOB  INTEGER WMO BLOCK (TO DECIDE TR)     (I)
!                 (6) WXMAN  LOGICAL TRUE IF MANUAL WEATHER CODE  (I)
!                 (7) TTT    REAL    DRY BULB TEMPERATURE         (O)
!                 (8) TDTDTD REAL    DEWPOINT                     (O)
!                 (9) UUU    REAL    RELATIVE HUMIDITY            (O)
!                (10) P0P0P0 REAL    STN LEVEL PRESSURE           (O)
!                (11) PPPP   REAL    MEAN SEA LEVEL PRESSURE      (O)
!                (12) A3     REAL    STD PRESSURE LEVEL           (O)
!                (13) HHH    REAL    GEOPOTENTIAL HEIGHT          (O)
!                (14) ATEND  REAL    PRESSURE TENDENCY            (O)
!                (15) PCHANG REAL    3HR PRESSURE CHANGE          (O)
!                (16) RRR    REAL    RAINFALL AMOUNT              (O)
!                (17) TR     REAL    RAINFALL PERIOD              (O)
!                (18) WW     REAL    PRESENT WEATHER              (O)
!                (19) W1     REAL    PAST WEATHER (1)             (O)
!                (20) W2     REAL    PAST WEATHER (2)             (O)
!                (21) NH     REAL    LOW(EST) CLOUD AMOUNT        (O)
!                (22) CL     REAL    LOW CLOUD TYPE               (O)
!                (23) CM     REAL    MEDIUM CLOUD TYPE            (O)
!                (24) CH     REAL    HIGH CLOUD TYPE              (O)
!                (25) HOUR   REAL    ACTUAL HOUR OF OBSERVATION   (O)
!                (26) MINS   REAL    ACTUAL MINUTE OF OBSERVATION (O)
!                (27) IX     INTEGER STATION TYPE                 (I)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:04$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synxp1.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:04    Sheila Needham  
! $
! Revision 2.2  2004/08/12 15:13:10  usmdb
! 16 August 2004. Stan Kellett. CHN53_04. INC123821.
! Ammended GT symbol to be GE ie Greater than or eaqual to
! rather than greater than. This fixed the problem that a rainfall
! group reporting 0.0 mm of rain is stored as missing.
!
! Revision 2.1  2002/12/02  14:31:26  14:31:26  usmdb (MetDB account c/o John C
!      Ward)
! Change day 16DEC02    R Hirst
! Correct the evaluation of rainfall periods for block 42 & 43.
!
! Revision 2.0  2001/07/03  10:44:10  10:44:10  usmdb (Generic MetDB account)
! Changed preprocessor statement to if (1 ), else...  Removed
! unused variables, added copyright and modified header - S.Cox
!
! Revision 1.8  2000/08/09  15:04:00  15:04:00  usmdb (Generic MetDB account)
! 21/8/2000 Update to allow for Blocks 42&43 reporting '/' at end of
! rainfall group to indicate rain since 3Z. Stan Kellett
!
! Revision 1.7  98/05/15  10:22:57  10:22:57  usmdb (Generic MDB account)
! Correct setting of section 1 group 4 700mb height - S.Cox           !D
!
! Revision 1.6  98/01/29  11:39:54  11:39:54  usmdb (Generic MDB account)
! Addition of IBM preprocess directive
!
! Revision 1.5  97/11/18  12:18:10  12:18:10  usjl (Jon Lewthwaite)
! Return time of ob even if it's within ten minutes of the hour.      !C
!
! Revision 1.4  1997/07/31 11:40:03  uspm
! First revision for  1
!
! Revision 1.3  1997/07/04 13:51:09  uspm
! Latst version from  1  - Y2K check
!
! Revision 1.2  1997/05/15 15:16:37  uspm
! Add revision information and preprocessor checks about INCLUDE
! statements
!
! MAY 97 - ONLY RETURN PRECISE TIME OF OB IF IT'S NOT WITHIN
!          TEN MINUTES OF THE HOUR                                    !B
!
! FEB 97 - REINTRODUCE NOV 96 CHANGE                                  !A
!
! FEB 97 - REVERSE NOV 96 CHANGE                                      !A
!
! NOV 96 - USE IX VALUE TO SET WW MISING DATA VALUES                  !A
!
! FEB 95 - CONVERT RAINFALL TO KG/M**2 (I.E. MM), NOT METRES!
!
! JAN 95 - PASS WMO BLOCK TO DECIDE RAINFALL PERIOD
!
! DEC 94 - DON''T ACCEPT RELATIVE HUMIDITY >100!
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

      INTEGER POINT,NGRPS,IWMOB
      INTEGER J,I,I1,IVALUE,LASTID,IX                               !2.0
      INTEGER IDATIM(5)                                             !2.1
      CHARACTER REPORT*(*)
      LOGICAL THREEGP,WXMAN,SYNTAX,WWMISS                           !2.0
      CHARACTER*132 CHEAD

#if defined (MVS)
      INCLUDE (SYNELM)
      INCLUDE (SYNCOM)    ! CONSTANTS
#else
      INCLUDE 'SYNELM'                                                          
      INCLUDE 'SYNCOM'    ! CONSTANTS
#endif

      CHEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synxp1.F,v $
     &'//'$ $Date: 30/01/2006 20:25:04$ $Author: Sheila Needham$ '//
     &'$Revision: 1$'

***********************************************************************
*
* CHECK SEQUENCE OF GROUPS: SET SYNTAX FLAG & STOP IF NOT ASCENDING
*
***********************************************************************
      THREEGP=.FALSE.
      LASTID=0
      WWMISS = .TRUE.                                              !A
*
      DO 999 J=1,NGRPS
      I=IVALUE(REPORT(POINT:POINT))
      IF(I.LE.LASTID)THEN
        SYNTAX=.TRUE.
        RETURN
      ENDIF
      LASTID=I
***********************************************************************
*
* DRY BULB TEMPERATURE (FROM DEGREES AND TENTHS TO K)
*
***********************************************************************
      IF (I.EQ.1) THEN
        I=IVALUE(REPORT(POINT+2:POINT+4))
        IF (I.NE.MISSIN) THEN
          IF (REPORT(POINT+1:POINT+1).EQ.'1') THEN
            TTT=-I*0.1+TCONV
          ELSE IF (REPORT(POINT+1:POINT+1).EQ.'0') THEN
            TTT=I*0.1+TCONV
          ENDIF
        ENDIF
***********************************************************************
*
* DEW POINT TEMPERATURE (FROM DEGREES AND TENTHS TO K) OR
* RELATIVE HUMIDITY (IN PERCENT)
*
***********************************************************************
      ELSE IF (I.EQ.2) THEN
        I=IVALUE(REPORT(POINT+2:POINT+4))
        IF (REPORT(POINT+1:POINT+1).EQ.'9') THEN
          IF (I.LE.100) UUU=I
        ELSE
          IF (I.NE.MISSIN) THEN
            IF (REPORT(POINT+1:POINT+1).EQ.'1') THEN
              TDTDTD=-I*0.1+TCONV
            ELSE IF (REPORT(POINT+1:POINT+1).EQ.'0') THEN
              TDTDTD=I*0.1+TCONV
            ENDIF
          ENDIF
        ENDIF
***********************************************************************
*
* STATION LEVEL PRESSURE (CONVERT FROM MB AND TENTHS TO PA).
* SYNTAX WILL BE FLAGGED IF 4-GROUP WITH HEIGHT BUT NO 3-GROUP
*
***********************************************************************
      ELSE IF (I.EQ.3) THEN
        I=IVALUE(REPORT(POINT+1:POINT+4))
        IF (I.NE.MISSIN) THEN
          IF (REPORT(POINT+1:POINT+1).EQ.'0') THEN
            P0P0P0=(I+10000.)*10.
          ELSE
            P0P0P0=I*10.
          ENDIF
        ENDIF
        THREEGP=.TRUE.
***********************************************************************
*
* GEOPOTENTIAL LEVEL AND ITS HEIGHT, OR MEAN SEA-LEVEL PRESSURE.
*
***********************************************************************
      ELSE IF (I.EQ.4) THEN
        IF (REPORT(POINT+1:POINT+1).EQ.'0' .OR.
     -      REPORT(POINT+1:POINT+1).EQ.'9') THEN
          I=IVALUE(REPORT(POINT+1:POINT+4))
          IF (I.NE.MISSIN) THEN
            IF (I.LT.9000) THEN
              PPPP=(I+10000.)*10.
            ELSE
              PPPP=I*10.
            ENDIF
          ENDIF
        ELSE
*
* IF SECOND FIGURE IS NOT 0 OR 9, IT''S A3 AS BELOW & LAST 3 FIGURES
* GIVE HEIGHT (WITHOUT THOUSANDS FIGURE) OF THAT STANDARD LEVEL.
*
          I=IVALUE(REPORT(POINT+1:POINT+1))
          HHH=IVALUE(REPORT(POINT+2:POINT+4))
          IF (I.NE.MISSIN .AND. HHH.NE.RMISS) THEN
            IF (I.EQ.1) THEN              ! NO THOUSANDS TO ADD
              A3=100000.
            ELSE IF (I.EQ.2) THEN         ! NO THOUSANDS TO ADD
              A3=92500.
            ELSE IF (I.EQ.8) THEN
              A3=85000.
              HHH=HHH+1000.
            ELSE IF (I.EQ.7) THEN
              A3=70000.
              IF (HHH.LT.400) THEN      ! TAKE RANGE AS 2400-3399M    !D
                HHH=HHH+3000.
              ELSE
                HHH=HHH+2000.
              ENDIF
            ELSE IF (I.EQ.5) THEN
              A3=50000.
              HHH=HHH+5000.
            ENDIF
          ENDIF
          IF (.NOT.THREEGP) SYNTAX=.TRUE.
        ENDIF
***********************************************************************
*
* PRESSURE TENDENCY AND AMOUNT OF CHANGE.
* NO CONVERSION NEEDED BETWEEN SYNOP CODE 0200 AND BUFR CODE 010063.
* CONVERT CHANGE FROM TENTHS OF MB TO PA.
* SET A CHANGE OF MORE THAN 50MB TO MISSING - TOO BIG FOR BUFR RANGE.
*
***********************************************************************
      ELSE IF (I.EQ.5) THEN
        I=IVALUE(REPORT(POINT+1:POINT+1))
        ATEND=I
        I1=IVALUE(REPORT(POINT+2:POINT+4))
        IF (I.NE.MISSIN .AND. I1.NE.MISSIN .AND. I1.LE.500) THEN
          IF (I.GE.5) THEN
            PCHANG=-I1*10.
          ELSE
            PCHANG=I1*10.
          ENDIF
        ENDIF
***********************************************************************
*
* RAINFALL AMOUNT AND PERIOD.
* CONVERT AMOUNT FROM SYNOP CODE TABLE 3590 TO KG/M**2 (NUMERICALLY
* SAME AS MM) & PERIOD FROM SYNOP CODE TABLE 4019 TO HOURS.
*
***********************************************************************
      ELSE IF (I.EQ.6) THEN
        I=IVALUE(REPORT(POINT+1:POINT+3))
        IF(I.GE.0.AND.I.LE.989) THEN           !2.2
          RRR=I
        ELSE IF (I.GT.990 .AND. I.LE.999) THEN
          RRR=(I-990)*0.1
        ELSE IF (I.EQ.990) THEN
          RRR=-1    !  A TRACE
        ENDIF
*
!1.8 For Indian and Sri-Lanka a Tr value of / means that it is a
!1.8 Rainfall period since 0300Z
        IF ((REPORT(POINT+4:POINT+4).EQ.'/').AND.(IWMOB.GE.42)   !1.8
     &    .AND.(IWMOB.LE.43)) THEN                               !1.8
          TR = -(MOD(IDATIM(4)+20,24)+1)                     !2.1!1.8
        ELSE                                                     !1.8
          I=IVALUE(REPORT(POINT+4:POINT+4))                      !1.8
          IF (I.GE.1 .AND. I.LE.4) THEN                          !1.8
            TR=-I*6                                              !1.8
          ELSE IF (I.GE.5 .AND. I.LE.7) THEN                     !1.8
            TR=-(I-4)                                            !1.8
          ELSE IF (I.EQ.8) THEN                                  !1.8
            TR=-9.                                               !1.8
          ELSE IF (I.EQ.9) THEN                                  !1.8
            TR=-15.                                              !1.8
          ELSE                                                   !2.1
            TR=I                                                 !2.1
          ENDIF                                                  !1.8
        ENDIF                                                    !1.8
*
* SOME BLOCKS IN THE TWENTIES & THIRTIES REPORT 12-HOUR RAINFALL WITH
* TR=0 BECAUSE THE 12-HOUR PERIOD DEPENDS ON LOCAL TIME RATHER THAN
* ENDING AT THE REPORT TIME.
*
        IF (I.EQ.0 .AND.IWMOB.GE.20.AND.IWMOB.LE.39) TR=-12.
***********************************************************************
*
* PRESENT AND PAST WEATHER GROUP 7WWW1W1 FROM MANNED STNS
* OR 7WAWAWA1WA2 FROM AUTOMATIC STNS.
* WW SYNOP CODE 4677 = BUFR CODE 20003
* WAWA SYNOP CODE 4680 (0-99)= BUFR CODE 20003 (100-199)
* W1,W2 SYNOP CODE 4561 = BUFR CODE 20004/5
* WA1,WA2 SYNOP CODE 4531 (0-9) = BUFR CODE 20004/5 (10-19)
*
***********************************************************************
      ELSE IF (I.EQ.7) THEN
        WW=IVALUE(REPORT(POINT+1:POINT+2)) ! PRESENT WX
        IF (.NOT.WXMAN .AND. WW.GT.RMISS) WW=WW+100.
        W1=IVALUE(REPORT(POINT+3:POINT+3)) ! PAST WX 1
        IF (.NOT.WXMAN .AND. W1.GT.RMISS) W1=W1+10.
        W2=IVALUE(REPORT(POINT+4:POINT+4)) ! PAST WX 2
        IF (.NOT.WXMAN .AND. W2.GT.RMISS) W2=W2+10.
        WWMISS = .FALSE.                                           !A
***********************************************************************
*
* MAIN CLOUD GROUP
* TOTAL CLOUD SYNOP CODE 2700 TO BUFR TBL 020011
* CL SYNOP CODE 0513 (+30) = BUFR CODE TBL 020012
* CM SYNOP CODE 0515 (+20) = BUFR CODE TBL 020012
* CH SYNOP CODE 0509 (+10) = BUFR CODE TBL 020012
*
***********************************************************************
      ELSE IF (I.EQ.8) THEN
        NH=IVALUE(REPORT(POINT+1:POINT+1))
        I=IVALUE(REPORT(POINT+2:POINT+2))
        IF (I.NE.MISSIN) CL=I+30.
        I=IVALUE(REPORT(POINT+3:POINT+3))
        IF (I.NE.MISSIN) CM=I+20.
        I=IVALUE(REPORT(POINT+4:POINT+4))
        IF (I.NE.MISSIN) CH=I+10.
***********************************************************************
*
* SUPPLEMENTARY TIME GROUP:
* IGNORE IT IF TIME WITHIN 10 MINUTES OF HOUR                       !B
*
* >>> THE CHANGE TO IGNORE A TIME GROUP WITHIN 10 MINUTES OF THE HOUR
* >>>  WAS MADE IN MAY 97 TO COPE WITH BLOCK 72 OBS WHICH MISUSE THE
* >>>   9-GROUP & WERE STORED AS OBS FOR THE PREVIOUS HOUR
*
* >>> The change above is removed so that the processing of the time
* >>> can be done in SYNEXP.
*
***********************************************************************
      ELSE IF(I.EQ.9) THEN
        I=IVALUE(REPORT(POINT+3:POINT+4))
        IF (I.NE.MISSIN .AND. I.LT.60 .AND. I.GE.00) THEN           !C
          MINS=I                                                    !B
          I=IVALUE(REPORT(POINT+1:POINT+2))
          IF (I.NE.MISSIN .AND. I.LE.23) HOUR=I
        ENDIF                                                       !B
      ENDIF
***********************************************************************
*
*   INCREMENT POINTER TO NEXT GROUP
*
***********************************************************************
      POINT=POINT+6
999   CONTINUE
      IF(WWMISS)THEN                                               !A
        IF    (IX.EQ.2.OR.IX.EQ.5)THEN                             !A
           WW = 508                                                !A
        ELSEIF(IX.EQ.3.OR.IX.EQ.6)THEN                             !A
           WW = 509                                                !A
        ENDIF                                                      !A
      ENDIF                                                        !A
      RETURN
      END
