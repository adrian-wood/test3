      SUBROUTINE SYNQC(REXP,WXMAN,IR,IX,N8,WPER,MSG,SYNTAX,SHIP)  !1.8m

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : SYNQC                                               
!                                                                     
! PURPOSE       : INTERNAL CONSISTENCY CHECKS ON SYNOP.               
!                 VALUES ARE IN FIXED SLOTS IN REXP - SEE SYNEXP.     
!                 2-BIT FLAGS, BUT SO FAR ONLY 1 OR MISSING SET       
!                                                                     
! CALLED BY     : SYNEXP                                              
!                                                                     
! PARAMETERS    : REXP    EXPANSION ARRAY                             
!                 WXMAN   TRUE FOR MANUAL STATION                     
!                 IR      RAINFALL GROUP INDICATOR                    
!                 IX      WEATHER GROUP INDICATOR                     
!                 N8      NUMBER OF 8-GROUPS (UP TO 4)                
!                 WPER    WEATHER PERIOD (-6, -3 OR -1)         !1.5  
!                 MSG     .TRUE. if diagnostic test messages    !1.8m 
!                 SYNTAX  TRUE IF THERE IS A SYNTAX ERROR             
!                 SHIP    TRUE IF REPORT IS FROM A SHIP               
!                                                                     
! REXP(2,N) HAS FLAGS IN REXP(1,N) & VALUES IN REXP(2,N)              
! ONLY THE FOLLOWING ELEMENTS ARE CHECKED:                            
!   15        DD           WIND DIRECTION  (990 IF VARIABLE)        !D
!   16        FF           WIND SPEED      (M/S)                    !D
!   17        T            TEMPERATURE     (KELVIN)                 !D
!   18        TD           DEW POINT       (KELVIN)                 !D
!   20        VV           VISIBILITY      (METRES, SEE BELOW)      !D
!   22        WW           PRESENT WEATHER (BUFR TABLE 020003)      !D
!   24        W1           PAST WEATHER    (BUFR TABLE 020004)      !D
!   25        W2           PAST WEATHER                             !D
!   26        N            CLOUD AMOUNT                             !D
!   33        RAINFALL IN SECTION 1                                 !D
!   35        RAINFALL IN SECTION 3                                 !D
!   49        A            PRESSURE TENDENCY                        !D
!   50        PP           PRESSURE TENDENCY (PASCALS)              !D
!  102        CL           LOW CLOUD TYPE  (BUFR TABLE 020012)      !D
!  104        CM           MIDDLE CLOUD TYPE    (TABLE 020012)      !D
!  106        CH           HIGH CLOUD TYPE (BUFR TABLE 020012)      !D
!  108        NH           LOW OR MEDIUM CLOUD AMOUNT               !D
!  109        H            HEIGHT OF LOWEST CLOUD (METRES, SEE BELOW)D
!  111,115... NS           CLOUD AMOUNT    (8-GROUPS)               !D
!  112,116... C            CLOUD TYPE      (8-GROUPS)               !D
!  113,117... HSHS         CLOUD HEIGHT    (8-GROUPS) (METRES)      !D
!                                                                     
! CODE FIGURES FOR CLOUD HEIGHT AND VISIBILITY WHICH CORRESPOND TO    
! RANGES OF VALUES ARE REDUCED TO SINGLE NUMBERS IN THE MDB AS BELOW: 
!  VV:   0M FOR VV=00, 75KM FOR VV=89, 25M FOR VV=90, 50KM FOR VV=99  
!  H:    LOWEST HEIGHT IN RANGE EXCEPT 25M FOR H=0                    
!  HSHS: 0M FOR HSHS=00, 22500M FOR HSHS=89,                          
!        MAX OF RANGE FOR HSHS=90-98, MISSING IF HSHS=99              
!                                                                     
! THE MISSING DATA INDICATOR IS -9999999                              
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:55$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synqc.F,v $
!                                                                     
! CHANGE RECORD :                                                     
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:55    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:07  usmdb
! Separated variable declaration and initialisation. Added copyright
! and modified header - S.Cox
!
! Revision 1.8  2001/05/09  09:38:03  09:38:03  usmdb (Generic MetDB account)
! 21 May 2001 Stan Kellett
! Number of symantic bug fixes identified by users implemented.
! 
! Revision 1.7  2001/03/07  11:34:14  11:34:14  usmdb (Generic MetDB account)
! Change day:   19MAR01         R Hirst
! Stop flagging of non-significant present and past weather
! elements reported by ships.
!
! Revision 1.6  2000/09/06  10:51:28  10:51:28  usmdb (Generic MetDB account)
! 18 Septeber 2000, Revision 1.6 by Stanley Kellett
! Corrected bugfix so that not every instance of Cs Flagged.
!
! Revision 1.5  98/09/08  17:02:19  17:02:19  usmdb (Generic MDB account)
! 09 September 1998 Add numbers to each 'msg' diagnostic message.
! Use weather period in cloud amount test (38) and remove present
! weather 41 from vis < 1000m test (23). John Norton
!
! Revision 1.4  98/02/04  15:57:06  15:57:06  usmdb (Generic MDB account)
! Add output of both heights returned from STAPOS. New value STNHTP
! added as item 6 in REXP array. Subsequent values have been moved
! on 1 place in array. JN                                             !D
!
! Revision 1.3  1997/08/28 10:10:19  usjl
! TEST FOR SKY OBSCURED CHANGED TO STOP FLAGGING WHEN PRESENT
! WEATHER VALUES OCCUR. EXTRA BRACKETS ADDED. JN                      !C
!
! Revision 1.2  1997/07/31 11:39:06  uspm
! First revision for 1
!
! Revision 1.1  1997/07/04 13:46:46  uspm
! Initial revision
!
! FEB 97: TEST FOR MANNED STATION WITH VISIBILTY <1 KM AND            !B 
!         PRESENT WEATHER REVERSED. JN 
!                                                                     
! FEB 97: CHANGES FOR PRESENT WEATHER (IWW) WHEN SET TO 508 OR        !A 
!         509 TO INDICATE MISSING PRESENT WEATHER. JN 
!                                                                     
! OCT 95: TIDIED UP, SOME TESTS REMOVED (ONLY NON-CONSISTENCY ONES  
!         LIKE NO POSITION FOUND & TEMP RANGE), OTHERS COMBINED.    
!                                                                     
! JAN 95: CORRECT SOME MESSAGES, LET TD FOR SHIP BE SLIGHTLY >T     
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

      CHARACTER*132 HEAD

      REAL    REXP(2,*),MIS,NS(4),C(4),HSHS(4)                      !2.0
      INTEGER IR,IX,N8, NMAND(3)                                    !2.0
      INTEGER IRRR,IRRR3, IWW,IW1,IW2, IH,INH,IN, ICL,ICM,ICH
      INTEGER ATEND,I,PCHANG
      INTEGER IMIS                                                  !2.0
      REAL    TTT,TDTD,DD,FF,VVV,WPER
      LOGICAL LF
      LOGICAL WXMAN       ! TRUE IF STATION MANNED
      LOGICAL WXGRP       ! TRUE IF PRESENT OR PAST WEATHER REPORTED
      LOGICAL WWREPT      ! TRUE IF PRESENT WEATHER REPORTED
      LOGICAL SYNTAX      ! TRUE IF THERE IS A SYNTAX ERROR
      LOGICAL SHIP        ! TRUE IF REPORT IS FROM A SHIP
      LOGICAL MSG         ! TRUE IF QC MESSAGES ARE TO BE OUTPUT (TEST)

      DATA MIS/-9999999./                                           !2.0
      DATA NMAND/3,5,8/                                             !2.0
      DATA IMIS/-9999999/                                           !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synqc.F,v $
     &'//'$ $Date: 30/01/2006 20:24:55$ $Revision: 1$'

***********************************************************************
*
*  IR & RAINFALL
*
***********************************************************************
      IRRR=REXP(2,33)                                             !D
      IRRR3=REXP(2,35)                                            !D
*
* IR INDICATED RAINFALL IN SECTIONS 1 AND 3 BUT NONE FOUND
*
      IF (IR.EQ.0 .AND. IRRR.EQ.IMIS .AND. IRRR3.EQ.IMIS) THEN
        SYNTAX=.TRUE.
        IF (MSG) PRINT *,' 1)IR=0 BUT NO RAINFALL IN SECTION 1 OR 3'
      ENDIF
*
* IR INDICATED RAINFALL IN SECTION 1 BUT NONE FOUND
*
      IF (IR.EQ.1 .AND. IRRR.EQ.IMIS) THEN
        SYNTAX=.TRUE.
        IF (MSG) PRINT *,' 2)IR=1 BUT NO RAINFALL IN SECTION 1'
      ENDIF
*
* IR INDICATED RAINFALL IN SECTION 3 BUT NONE FOUND
*
      IF (IR.EQ.2 .AND. IRRR3.EQ.IMIS) THEN
        SYNTAX=.TRUE.
        IF (MSG) PRINT *,' 3)IR=2 BUT NO RAINFALL IN SECTION 3'
      ENDIF
*
* IR INDICATED NO RAINFALL BUT RAINFALL FOUND
*
      IF (IR.EQ.3 .OR. IR.EQ.4) THEN
        IF (IRRR.NE.IMIS .AND. IRRR.NE.0) THEN
          REXP(1,33)=1.                                           !D
          IF (MSG) PRINT *,' 4)IR>2 BUT RAINFALL IN SECTION 1'
        ENDIF
        IF (IRRR3.NE.IMIS) THEN
          REXP(1,35)=1.                                           !D
          IF (MSG) PRINT *,' 5)IR>2 BUT RAINFALL IN SECTION 3'
        ENDIF
      ENDIF
***********************************************************************
*
*  IX & WEATHER
*
***********************************************************************
      IWW=REXP(2,22)                                              !D
      IW1=REXP(2,24)                                              !D
      IW2=REXP(2,25)                                              !D
*
      WXGRP=.FALSE.
      WWREPT=.FALSE.                                                 !A
      IF(IWW.NE.IMIS .AND. IWW.NE.508 .AND. IWW.NE.509)WWREPT=.TRUE. !A
      IF (WWREPT .OR. IW1.NE.IMIS .OR. IW2.NE.IMIS) WXGRP=.TRUE.     !A
*
* IX INDICATED NO WEATHER GROUP, BUT WEATHER GROUP FOUND (SUSPECT ID?)
*
      IF ((IX.EQ.2.OR.IX.EQ.3.OR.IX.EQ.5.OR.IX.EQ.6) .AND. WXGRP) THEN
        REXP(1,22)=1.                                             !D
        REXP(1,24)=1.                                             !D
        REXP(1,25)=1.                                             !D
        IF (MSG) PRINT *,' 6)WEATHER GROUP REPORTED DESPITE IX'
      ENDIF
*
* IX INDICATED WEATHER GROUP, BUT NO WEATHER GROUP FOUND.
*
      IF((IX.EQ.1.OR.IX.EQ.4.OR.IX.EQ.7)
     &    .AND. .NOT.(WXGRP .OR. IWW.EQ.510)) THEN                   !A
        SYNTAX=.TRUE.
        IF (MSG) PRINT *,' 7)NO WEATHER GROUP REPORTED DESPITE IX'
      ENDIF
***********************************************************************
*
*  WEATHER (WW, W1 & W2)
*
***********************************************************************
*
* PRESENT AND PAST WEATHER INSIGNIFICANT BUT WEATHER GROUP
* REPORTED  (ALLOWED FOR SHIPS BUT NOT FOR LAND STATIONS            !1.7
*            SEE MANUAL ON CODES 12.2.6.1 AND 12.2.6.2).            !1.7
*
      IF (.NOT.SHIP) THEN                                           !1.7
        IF (WXMAN) THEN
          IF (IWW.GE.0.AND.IWW.LE.3 .AND. IW1.GE.0.AND.IW1.LE.2
     &        .AND. IW2.GE.0.AND.IW2.LE.2) THEN                     !1.7
            REXP(1,22)=1.                                           !D
            REXP(1,24)=1.                                           !D
            REXP(1,25)=1.                                           !D
          ENDIF
        ELSE
          IF (IWW.GE.100 .AND. IWW.LE.103 .AND.
     &        IW1.EQ.10 .AND. IW2.EQ.10) THEN
            REXP(1,22)=1.                                           !D
            REXP(1,24)=1.                                           !D
            REXP(1,25)=1.                                           !D
          ENDIF
        ENDIF
      ENDIF                                                         !1.7
*
* PAST WEATHER INDICATES FOG IN THE PAST HOUR BUT PRESENT WEATHER
* DOESN''T MENTION FOG AND NO THUNDERSTORM REPORTED.
*
      IF (WXMAN) THEN
        IF ((IW1.EQ.4.OR.IW2.EQ.4) .AND. INT(WPER).EQ.-1 .AND.    !A1.5
     &      ((IWW.GE.0 .AND.IWW.LT.28 .AND. IWW.NE.17) .OR.          !A
     &        .NOT.WWREPT)) THEN                                     !A
          REXP(1,22)=1.                                           !D
          REXP(1,24)=1.                                           !D
          REXP(1,25)=1.                                           !D
          IF (MSG) PRINT *,' 10)WW INCONSISTENT WITH FOG IN PAST HOUR'
        ENDIF
      ELSE
        IF ((IW1.EQ.13.OR.IW2.EQ.13) .AND. INT(WPER).EQ.-1 .AND.  !A1.5
     &      ((IWW.GE.100 .AND.IWW.LT.130 .AND. IWW.NE.126) .OR.      !A
     &        .NOT.WWREPT)) THEN                                     !A
          REXP(1,22)=1.                                           !D
          REXP(1,24)=1.                                           !D
          REXP(1,25)=1.                                           !D
          IF (MSG) PRINT *,' 11)WW INCONSISTENT WITH FOG IN PAST HOUR'
        ENDIF
      ENDIF
*
* SECOND PAST WEATHER CODE IS GREATER THAN THE FIRST.
*
      IF (IW2.GT.IW1) THEN
        REXP(1,24)=1.                                             !D
        REXP(1,25)=1.                                             !D
      IF (MSG) PRINT *,' 12)SECOND PAST WEATHER CODE GREATER THAN FIRST'
      ENDIF
***********************************************************************
*
*  WIND
*
***********************************************************************
      DD=REXP(2,15)               ! MAY BE 990, "VARIABLE"        !D
      FF=REXP(2,16)                                               !D
*
* WIND DIRECTION INDICATES CALM BUT WIND SPEED REPORTED.
*
      IF (DD.EQ.0 .AND. FF.GT.0.) THEN
        REXP(1,15)=1.                                             !D
        REXP(1,16)=1.                                             !D
        IF (MSG) PRINT *,' 13)WIND DIRECTION CALM BUT SPEED REPORTED'
      ENDIF
*
* WIND SPEED INDICATES CALM BUT WIND DIRECTION REPORTED.
*
      IF (FF.EQ.0 .AND. DD.GT.0.) THEN
        REXP(1,15)=1.                                             !D
        REXP(1,16)=1.                                             !D
        IF (MSG) PRINT *,' 14)WIND SPEED CALM BUT DIRECTION REPORTED'
      ENDIF
*
* WIND DIRECTION RANGE CHECK
* (DD=990, "VARIABLE", WILL BE ENCODED AS DD=0)
*
      IF (DD.GT.360. .AND. DD.NE.990) THEN
        REXP(1,15)=1.                                             !D
        IF (MSG) PRINT *,' 15)WIND DIRECTION IMPOSSIBLE'
      ENDIF
*
* VARIABLE WIND DIRECTION & INCONSISTENT SPEED (0 OR MORE THAN 5M/S)
*
*     IF (DD.EQ.990 .AND. (FF.EQ.0 .OR. FF.GT.5)) THEN
*       REXP(1,15)=1.                                             !D
*       REXP(1,16)=1.                                             !D
*       IF (MSG) PRINT *,' 16)VARIABLE WIND BUT SPEED ZERO OR TOO HIGH'
*     ENDIF
***********************************************************************
*
*  PRESSURE TENDENCY (APP)
*
***********************************************************************
      ATEND=REXP(2,49)                                            !D
      PCHANG=REXP(2,50)                                           !D
*
* PRESSURE CHARACTERISTIC INDICATES STEADY, BUT PRESSURE CHANGE EXISTS
*
      IF (ATEND.EQ.4 .AND. PCHANG.NE.0) THEN
        REXP(1,49)=1.                                             !D
        REXP(1,50)=1.                                             !D
        IF (MSG) PRINT *,' 17)PRESSURE STEADY BUT NONZERO CHANGE'
      ENDIF
*
* PRESSURE CHARACTERISTIC INDICATES CHANGE, BUT NO PRESSURE CHANGE.
*
!1.8a 9 changed to 8 as OPR says 8 with no pressure changes is valid
!1.8a whilst 9 was a mistake.
      IF (PCHANG.EQ.0 .AND. ATEND.NE.0.AND.ATEND.NE.4.AND.ATEND.NE.5
     &   .AND. ATEND.NE.8 .AND. ATEND.NE.IMIS) THEN   !1.8a
        REXP(1,49)=1.                                             !D
        REXP(1,50)=1.                                             !D
        IF (MSG) PRINT *,' 18)NO PRESSURE CHANGE DESPITE CHARACTERISTIC'
      ENDIF
***********************************************************************
*
*  TEMPERATURE RANGE, TEMPERATURES AND WEATHER
*
***********************************************************************
      TTT=REXP(2,17)                                              !D
      TDTD=REXP(2,18)                                             !D
*
* DEW POINT HIGHER THAN TEMPERATURE AT LAND STATION (BLOCK NUMBER SET)
* OR MORE THAN 1 DEGREE HIGHER ON SHIP (WMO BLOCK MISSING)
*
      IF (TTT.NE.MIS .AND. TDTD.NE.MIS .AND.
     &   (.NOT.SHIP .AND. TDTD.GT.TTT) .OR.
     &   (SHIP .AND. TDTD.GT.TTT+1.)) THEN
        REXP(1,17)=1.                                             !D
        REXP(1,18)=1.                                             !D
      IF (MSG) PRINT *,' 19)DEW POINT TOO HIGH FOR TEMPERATURE',TDTD,TTT
      ENDIF
*
* FREEZING RAIN, SNOW, RIME ETC BUT TTT 10C OR MORE
*
      IF (WXMAN) THEN
        IF (TTT.GT.283.2 .AND.
     &   ((IWW.GE.66.AND.IWW.LE.79).OR.(IWW.GE.83.AND.IWW.LE.88).OR.
     &    IWW.EQ.24.OR.IWW.EQ.48.OR.IWW.EQ.49.OR.IWW.EQ.56)) THEN
          REXP(1,17)=1.                                           !D
          REXP(1,22)=1.                                           !D
          IF (MSG) PRINT *,' 20)TEMPERATURE TOO HIGH FOR SNOW, RIME ETC'
        ENDIF
      ELSE
        IF (TTT.GT.283.2 .AND.
     &   ((IWW.GE.164.AND.IWW.LE.166).OR.(IWW.GE.185.AND.IWW.LE.187).OR.
     &    IWW.EQ.125 .OR. IWW.EQ.135 .OR.
     &    IWW.EQ.154 .OR. IWW.EQ.155 .OR. IWW.EQ.156)) THEN
          REXP(1,17)=1.                                           !D
          REXP(1,22)=1.                                           !D
          IF (MSG) PRINT *,' 21)TEMPERATURE TOO HIGH FOR SNOW, RIME ETC'
        ENDIF
      ENDIF
*
* DEW POINT DEPRESSION TOO GREAT FOR FOG
*
!1.8b T-Td put back, ww 40-49 changed to 41-49
      IF (TTT-TDTD.GT.5. .AND. IWW.GE.41 .AND. IWW.LE.49) THEN    !1.8b
        REXP(1,17)=1.                                             !1.8b
        REXP(1,18)=1.                                             !1.8b
        REXP(1,22)=1.                                             !1.8b
        IF (MSG) PRINT *,' 22)DEW POINT DEPRESSION TOO GREAT FOR FOG'!1.8b
      ENDIF                                                       !1.8b
***********************************************************************
*
*  VISIBILITY & WEATHER
*
***********************************************************************
      VVV=REXP(2,20)                                              !D
*
* VISIBILITY < 1 KM, BUT PRESENT WEATHER INCONSISTENT.
*
      IF (VVV.LT.1000.) THEN
        IF (WXMAN) THEN
!1.8c improve readability of if statement.
          IF(IWW.LT.4 .OR. (IWW.GE.5 .AND. IWW.LE.28)        !1.8c
     &       .OR. IWW.EQ.40 ) THEN                           !1.8c
            REXP(1,22)=1.                                         !D
            REXP(1,20)=1.                                         !D
            IF (MSG) PRINT *,' 23)WEATHER WRONG FOR VISIBILITY <1 KM'
          ENDIF
        ELSE
!1.8c Allow for reserved ranges being used (IWW 106-109)
          IF((IWW.GE.100.AND.IWW.LE.104).OR.
     &       (IWW.GE.106.AND.IWW.LE.119).OR.                      !1.8c
     &       (IWW.EQ.128))THEN                                    !1.8c
            REXP(1,22)=1.                                         !D
            REXP(1,20)=1.                                         !D
            IF (MSG) PRINT *,' 24)WEATHER WRONG FOR VISIBILITY <1 KM'
          ENDIF
        ENDIF
      ENDIF
*
* VISIBILITY >= 1 KM, BUT FOG REPORTED.
*
      IF (VVV.GE.1000.) THEN
        IF (WXMAN) THEN
          IF (IWW.GE.41 .AND. IWW.LE.49) THEN
            REXP(1,22)=1.                                         !D
            REXP(1,20)=1.                                         !D
            IF (MSG) PRINT *,' 25)VISIBILITY >1 KM BUT FOG REPORTED'
          ENDIF
        ELSE
!1.8d flag if ww 130-135 (Fog <1Km )
          IF (IWW.GE.130 .AND. IWW.LE.135 ) THEN                  !1.8d
            REXP(1,22)=1.                                         !D
            REXP(1,20)=1.                                         !D
            IF (MSG) PRINT *,' 26)VISIBILITY >1 KM BUT FOG REPORTED'
          ENDIF
        ENDIF
      ENDIF
*
* VISIBILITY >= 2 KM, BUT HEAVY SNOW REPORTED.
*
      IF (WXMAN) THEN
        IF (VVV.GE.2000. .AND. (IWW.EQ.74.OR.IWW.EQ.75)) THEN
          REXP(1,20)=1.                                           !D
          REXP(1,22)=1.                                           !D
          IF (MSG) PRINT *,' 27)VISIBILITY >2 KM BUT HEAVY SNOW'
        ENDIF
      ELSE
        IF (VVV.GE.2000. .AND. IWW.EQ.173) THEN
          REXP(1,20)=1.                                           !D
          REXP(1,22)=1.                                           !D
          IF (MSG) PRINT *,' 28)VISIBILITY >2 KM BUT HEAVY SNOW'
        ENDIF
      ENDIF
*
* VVV>1KM BUT WEATHER IS HAZE WITH VISIBILITY <1KM  (N.B. THIS CHECK
* IS FOR AUTOMATIC STATIONS ONLY: NO VISIBILITY IN MANUAL HAZE CODE)
*
      IF (.NOT.WXMAN) THEN
        IF (VVV.GE.1000. .AND. IWW.EQ.105) THEN
          REXP(1,20)=1.                                           !D
          REXP(1,22)=1.                                           !D
          IF (MSG) PRINT *,' 29)VISIBILITY >1 KM BUT WW=105'
        ENDIF
      ENDIF
***********************************************************************
*
*  TOTAL CLOUD AMOUNT (N) & HEIGHT OF LOWEST CLOUD (H)
*
***********************************************************************
      IH=REXP(2,109)                                              !D
      IN=REXP(2,26)                                               !D
*
* SKY NOT OBSCURED BUT NO LOW/MEDIUM CLOUD BASE REPORTED.
*
      IF (IN.NE.9 .AND. IN.NE.IMIS .AND. IH.EQ.IMIS) THEN
        REXP(1,109)=1.                                            !D
        REXP(1,26)=1.                                             !D
        IF (MSG) PRINT *,' 30)SKY NOT OBSCURED BUT NO CLOUD BASE'
      ENDIF
*
* NO CLOUD AMOUNT BUT CLOUD BASE REPORTED.
*
!1.8e Decision made that if a 0 is reported for lowest cloud
!1.8e yet the lowest cloud group is reported as '/' (missing) then
!1.8e this should be flagged. change made to flag if IH missing.
      IF (IN.EQ.0 .AND. IH.LT.2500) THEN                          !1.8e
        REXP(1,26)=1.                                             !D
        REXP(1,109)=1.                                            !D
        IF (MSG) PRINT *,' 31)CLOUD BASE BUT NO CLOUD AMOUNT'
      ENDIF
***********************************************************************
*
*  TOTAL CLOUD AMOUNT (N), VISIBILITY & WEATHER
*
***********************************************************************
*
* TOTAL CLOUD AMOUNT OBSCURED BUT VISIBILITY 2KM OR MORE
*
      IF (IN.EQ.9 .AND. VVV.GE.2000.) THEN
        REXP(1,26)=1.                                             !D
        REXP(1,20)=1.                                             !D
        IF (MSG) PRINT *,' 32)CLOUD OBSCURED BUT VISIBILITY >2 KM'
      ENDIF
!
!1.8f check 33 deleted: ww=0 can meat sky not observed, not cloud
!1.8f cloud missing
*
* TOTAL CLOUD AMOUNT OBSCURED BUT SKY VISIBLE.
*
      IF (WXMAN) THEN
        IF (IN.EQ.9 .AND. ((IWW.GE.0.AND.IWW.LE.29).OR.IWW.EQ.40.OR.
     &     IWW.EQ.42.OR.IWW.EQ.44.OR.IWW.EQ.46.OR.IWW.EQ.48))THEN   !c
          REXP(1,22)=1.                                           !D
          REXP(1,26)=1.                                           !D
          IF (MSG) PRINT *,' 34)CLOUD OBSCURED BUT SKY VISIBLE'
        ENDIF
      ELSE
!1.8g flag only Iww between 100 & 103 as these imply sky visible
        IF (IN.EQ.9 .AND. IWW.GE.100.AND.IWW.LE.103) THEN         !1.8g
          REXP(1,22)=1.                                           !D
          REXP(1,26)=1.                                           !D
          IF (MSG) PRINT *,' 35)CLOUD OBSCURED BUT SKY VISIBLE'
        ENDIF
      ENDIF
*
* NO TOTAL CLOUD AMOUNT BUT CLOUDS FORMING OR PRECIPITATION REPORTED.
*
      IF(WXMAN)THEN
        IF((IN.EQ.0.OR.IN.EQ.MIS).AND.(IWW.EQ.3.OR.
     &     (IWW.GE.14.AND.IWW.LE.17).OR.
     &     (IWW.GE.50.AND.IWW.LE.75).OR.
     &     (IWW.GE.77.AND.IWW.LE.99)))THEN
          REXP(1,22)=1.                                           !D
          REXP(1,26)=1.                                           !D
          IF (MSG) PRINT *,' 36)NO TOTAL CLOUD AMOUNT DESPITE WEATHER'
        ENDIF
      ELSE
!1.8h decimal point removed
        IF((IN.EQ.0.OR.IN.EQ.MIS).AND.(IWW.EQ.103 .OR.            !1.8h
     &     (IWW.GE.150.AND.IWW.LE.199)))THEN
          REXP(1,22)=1.                                           !D
          REXP(1,26)=1.                                           !D
          IF (MSG) PRINT *,' 37)NO TOTAL CLOUD AMOUNT DESPITE WEATHER'
        ENDIF
      ENDIF
*
* HALF CLOUD COVER OR LESS BUT PAST WEATHER INDICATES MORE THAN
* HALF CLOUD COVER THROUGHOUT PERIOD.
*
      IF (IN.GE.0 .AND. IN.LE.4 .AND. (IW1.EQ.2.OR.IW2.EQ.2))THEN
        REXP(1,26)=1.                                             !D
        REXP(1,24)=1.                                             !D
        REXP(1,25)=1.                                             !D
        IF (MSG) PRINT *,' 38)CLOUD COVER & PAST WEATHER INCONSISTENT'
      ENDIF
*
* MORE THAN HALF CLOUD COVER BUT PAST WEATHER INDICATES HALF CLOUD
* COVER OR LESS THROUGHOUT PERIOD.
*
      IF (IN.GE.5 .AND. IN.LE.8 .AND. (IW1.EQ.0.OR.IW2.EQ.0))THEN
        REXP(1,26)=1.                                             !D
        REXP(1,24)=1.                                             !D
        REXP(1,25)=1.                                             !D
        IF (MSG) PRINT *,' 39)CLOUD COVER & PAST WEATHER INCONSISTENT'
      ENDIF
***********************************************************************
***********************************************************************
*
*  MAIN CLOUD GROUP (8-GROUP IN SECTION 1)
*  WW & MAIN CLOUD GROUP
*
***********************************************************************
***********************************************************************
      INH=REXP(2,108)                                             !D
      ICL=REXP(2,102)                                             !D
      ICM=REXP(2,104)                                             !D
      ICH=REXP(2,106)                                             !D
*
* THUNDERSTORM AT OR IN SIGHT OF THE STATION, CLOUD AMOUNT NONZERO,
* BUT NO SUITABLE CLOUD IN 8-GROUP (ALLOW FOR A BLANKET OF LOW CLOUD).
*
      IF (IN.GE.0.AND.IN.LE.8 .AND. ICL.NE.33.AND.ICL.NE.39 .AND.
     &    ICM.NE.28.AND.ICM.NE.29 .AND. ICH.NE.13 .AND.
     &    ICL.NE.36.AND.ICL.NE.37 .AND. (INH.EQ.7.OR.INH.EQ.8)) THEN
        IF (WXMAN) THEN
          IF (IWW.EQ.17 .OR. (IWW.GE.95.AND.IWW.LE.99)) THEN
            REXP(1,22)=1.        ! FLAG WW & CL,CM,CH             !D
            REXP(1,102)=1.                                        !D
            REXP(1,104)=1.                                        !D
            REXP(1,106)=1.                                        !D
            IF (MSG) PRINT *,' 40)THUNDERSTORM BUT NO SUITABLE CLOUD'
          ENDIF
        ELSE
          IF (IWW.EQ.117 .OR. IWW.EQ.191 .OR.
     &       (IWW.GE.193.AND.IWW.LE.196)) THEN
            REXP(1,22)=1.                                         !D
            REXP(1,102)=1.                                        !D
            REXP(1,104)=1.                                        !D
            REXP(1,106)=1.                                        !D
            IF (MSG) PRINT *,' 41)THUNDERSTORM BUT NO SUITABLE CLOUD'
          ENDIF
        ENDIF
      ENDIF
***********************************************************************
*
*  H & MAIN CLOUD GROUP
*
***********************************************************************
*
* CLOUD BASE IS MISSING BUT LOW/MEDIUM CLOUD AMOUNT IS REPORTED.
*
      IF (IH.EQ.IMIS .AND. INH.NE.9 .AND. INH.NE.IMIS) THEN
        REXP(1,108)=1.                                            !D
        REXP(1,109)=1.                                            !D
        IF (MSG) PRINT *,' 42)LOW/MEDIUM CLOUD BUT NO CLOUD BASE'
      ENDIF
*
* CLOUD BASE REPORTED BUT NO AMOUNT OF LOW/MEDIUM CLOUD IS REPORTED.
*
      IF (INH.EQ.0 .AND. IH.GE.0.AND.IH.LT.2500) THEN
        REXP(1,108)=1.                                            !D
        REXP(1,109)=1.                                            !D
        IF (MSG) PRINT *,' 43)CLOUD BASE BUT NO LOW/MEDIUM CLOUD'
      ENDIF
***********************************************************************
*
*  N & MAIN CLOUD GROUP
*
***********************************************************************
*
* TOTAL CLOUD AMOUNT IS LESS THAN LOW/MEDIUM CLOUD AMOUNT.
*
      IF(IN.LT.INH)THEN
        REXP(1,26)=1.                                             !D
        REXP(1,108)=1.                                            !D
        IF (MSG) PRINT *,' 44)TOTAL CLOUD LESS THAN LOW/MEDIUM CLOUD'
      ENDIF
*
* SKY OBSCURED BUT LOW OR MEDIUM CLOUD PRESENT.
*
!1.8i flag if a value given to INH but IN states sky obscured
!1.8i OPR have said that a INH value of 9 is invalid
      IF (IN.EQ.9 .AND. INH.NE.MIS) THEN                          !1.8i
        REXP(1,26)=1.                                             !D
        REXP(1,108)=1.                                            !D
        IF (MSG) PRINT *,' 45)SKY OBSCURED BUT LOW/MEDIUM CLOUD PRESENT'
      ENDIF
*
* NO MEDIUM/HIGH CLOUD, BUT NOT ALL CLOUD IS IN LOWEST LAYER.
*
      IF (INH.NE.IMIS .AND. IN.NE.INH .AND.
     &    ICM.EQ.IMIS .AND. ICH.EQ.IMIS) THEN
        REXP(1,26)=1.           ! FLAG N & NH                     !D
        REXP(1,108)=1.                                            !D
        IF (MSG) PRINT *,' 46)NOT ALL CLOUD LOWEST, BUT NO MEDIUM/HIGH'
      ENDIF
*
* NO LOW/HIGH CLOUD, BUT NOT ALL CLOUD IS IN MEDIUM LAYER.
*
      IF (INH.NE.IMIS .AND. IN.NE.INH .AND.
     &    ICL.EQ.IMIS .AND. ICH.EQ.IMIS) THEN
        REXP(1,26)=1.           ! FLAG N & NH                     !D
        REXP(1,108)=1.                                            !D
        IF (MSG) PRINT *,' 47)NOT ALL CLOUD MEDIUM, BUT NO LOW/HIGH'
      ENDIF
*
* CIRROSTRATUS COVERS SKY BUT CLOUD AMOUNT NOT 8/8.
*
      IF(IN.LT.8.AND.ICH.EQ.17)THEN
        REXP(1,26)=1.                                             !D
        REXP(1,106)=1.                                            !D
        IF (MSG) PRINT *,' 48)CS COVERS SKY BUT AMOUNT NOT 8/8'
      ENDIF
*
* TOTAL CLOUD AMOUNT REPORTED BUT NO LOW, MEDIUM OR HIGH CLOUD.
*
      IF (ICL.EQ.30 .AND. ICM.EQ.20 .AND. ICH.EQ.10) THEN
        IF (IN.GE.1 .AND. IN.LE.8) THEN
          REXP(1,26)=1.                                           !D
          REXP(1,102)=1.                                          !D
          REXP(1,104)=1.                                          !D
          REXP(1,106)=1.                                          !D
          IF (MSG) PRINT *,' 49)TOTAL CLOUD BUT NO LOW, MEDIUM OR HIGH'
        ENDIF
      ENDIF
*
* NO TOTAL CLOUD AMOUNT BUT LOW, MEDIUM OR HIGH CLOUD REPORTED.
*
      IF (ICL.GT.30 .OR. ICM.GT.20 .OR. ICH.GT.10) THEN
        IF (IN.EQ.0) THEN
          REXP(1,26)=1.                                           !D
          REXP(1,102)=1.                                          !D
          REXP(1,104)=1.                                          !D
          REXP(1,106)=1.                                          !D
          IF (MSG) PRINT *,' 50)NO TOTAL CLOUD BUT LOW, MEDIUM OR HIGH'
        ENDIF
      ENDIF
*
* SKY OBSCURED BUT CLOUD TYPE REPORTED (PERHAPS AS ZERO)
*
      IF (IN.EQ.9 .AND. (ICH.GE.10.OR.ICM.GE.20.OR.ICL.GE.30)) THEN
        REXP(1,26)=1.                                             !D
        REXP(1,102)=1.                                            !D
        REXP(1,104)=1.                                            !D
        REXP(1,106)=1.                                            !D
        IF (MSG) PRINT *,' 51)SKY OBSCURED BUT CLOUD TYPE REPORTED'
      ENDIF
***********************************************************************
*
*  MAIN CLOUD GROUP: INTERNAL CHECKS
*
***********************************************************************
*
* LOW CLOUD MISSING BUT MEDIUM CLOUD REPORTED.
*
      IF (ICM.NE.IMIS .AND. ICL.EQ.IMIS) THEN
        REXP(1,104)=1.    ! ONLY CM FLAGGED (CL MISSING)          !D
        IF (MSG) PRINT *,' 52)MEDIUM CLOUD, BUT LOW CLOUD MISSING'
      ENDIF
*
* LOW AND MEDIUM CLOUD MISSING BUT HIGH CLOUD REPORTED.
*
      IF (ICH.NE.IMIS .AND. ICL.EQ.IMIS .AND. ICM.EQ.IMIS) THEN
        REXP(1,106)=1.    ! ONLY CH FLAGGED (CL & CM MISSING)     !D
        IF (MSG) PRINT *,' 53)HIGH CLOUD, BUT LOW/MEDIUM CLOUD MISSING'
      ENDIF
*
* NO CLOUD AMOUNT REPORTED BUT LOW CLOUD TYPE GIVEN.
*
      IF ((INH.EQ.IMIS .OR. INH.EQ.0) .AND. ICL.GT.30) THEN
        REXP(1,102)=1.                                            !D
        REXP(1,108)=1.                                            !D
        IF (MSG) PRINT *,' 54)LOW CLOUD TYPE, BUT NO CLOUD AMOUNT'
      ENDIF
*
* CLOUD AMOUNT REPORTED BUT NO LOW CLOUD TYPE GIVEN.
*
      IF (INH.LT.9.AND.INH.GT.0 .AND. ICL.EQ.IMIS) THEN
        REXP(1,102)=1.                                            !D
        REXP(1,108)=1.                                            !D
        IF (MSG) PRINT *,' 55)CLOUD AMOUNT, BUT NO LOW CLOUD TYPE'
      ENDIF
*
* MEDIUM CLOUD REPORTED BUT NO CLOUD AMOUNT.
*
      IF (INH.EQ.0 .AND. ICM.GT.20) THEN
        REXP(1,104)=1.                                            !D
        REXP(1,108)=1.                                            !D
        IF (MSG) PRINT *,' 56)MEDIUM CLOUD, BUT NO CLOUD AMOUNT'
      ENDIF
*
* 8/8 LOW/MEDIUM CLOUD AMOUNT BUT CH NOT MISSING.
*
      IF (INH.EQ.8 .AND. ICH.GT.10) THEN  !1.8k ICH=60 not set  in
                                          !1.8k MetDB, so not tested
        REXP(1,106)=1.                                            !D
        REXP(1,108)=1.                                            !D
        IF (MSG) PRINT *,' 57)8/8 LOW/MEDIUM CLOUD, BUT CH NOT MISSING'
      ENDIF
***********************************************************************
***********************************************************************
*
*  CLOUD CHECKS INVOLVING 8-GROUPS IN SECTION 3
* - ALL THE TESTS FROM HERE TO THE END ASSUME 8-GROUPS IN SECTION 3
*
***********************************************************************
***********************************************************************
      IF (N8.GT.0) THEN
*
* CLOUD NOT OBSERVED, TOTAL CLOUD IS 0, BUT 8-GROUPS REPORTED.
*  (N & WW AGREE, SO THE 8-GROUPS ARE ASSUMED TO BE WRONG)
*
        IF (IWW.EQ.0 .AND. IN.EQ.0.) THEN
          DO I=1,N8
           REXP(1,111+(I-1)*4)=1.                                 !D
           REXP(1,112+(I-1)*4)=1.                                 !D
           REXP(1,113+(I-1)*4)=1.                                 !D
           IF (MSG) PRINT *,' 58)8-GROUPS THOUGH TOTAL CLOUD ZERO'
          ENDDO
        ENDIF
*
* FIRST PUT NS, C & HSHS IN ARRAYS.  FLAG ANY ZERO NS.
*
        LF=.FALSE.
        DO I=1,N8
         NS(I)=REXP(2,111+(I-1)*4)                                !D
         C(I)=REXP(2,112+(I-1)*4)                                 !D
         HSHS(I)=REXP(2,113+(I-1)*4)                              !D
         IF (NS(I).EQ.0) REXP(1,111+(I-1)*4)=1.                   !D
        ENDDO
*
* TOTAL CLOUD AMOUNT LESS THAN CLOUD AMOUNT IN ONE 8-GROUP
*
        DO I=1,N8
         IF(IN.LT.NS(I))THEN
           REXP(1,111+(I-1)*4)=1.                                 !D
           REXP(1,26)=1.                                          !D
           IF (MSG) PRINT *,' 59)TOTAL CLOUD LESS THAN IN ONE 8-GROUP'
         ENDIF
        ENDDO
*
* NO LOW,MEDIUM OR HIGH CLOUD BUT CLOUD AMOUNT IN FIRST 8-GROUP
*
        IF ((ICL.EQ.30 .OR. ICL.EQ.IMIS) .AND.
     &      (ICM.EQ.20 .OR. ICM.EQ.IMIS) .AND.
     &      (ICH.EQ.10 .OR. ICH.EQ.IMIS)) THEN
          REXP(1,102)=1.                                          !D
          REXP(1,104)=1.                                          !D
          REXP(1,106)=1.                                          !D
          REXP(1,111)=1.                                          !D
          REXP(1,112)=1.                                          !D
          REXP(1,113)=1.                                          !D
          IF (MSG) PRINT *,' 60)NO CM, CL & CH, BUT 8-GROUP REPORTED'
        ENDIF
**********
*   NH   *            NH IS THE AMOUNT OF ALL LOW CLOUD OR, IF THERE IS
**********            NO LOW CLOUD, THEN THE AMOUNT OF ALL MEDIUM CLOUD
*                     - DECIDE WHICH FROM THE TYPE IN THE FIRST 8-GROUP
*
* LESS CLOUD IN NH THAN IN CORRESPONDING 8-GROUPS
*
        DO I=1,N8
         IF (C(1).GT.5) THEN
           IF (C(I).GE.6.AND.C(I).LE.9 .AND. INH.LT.NS(I)) THEN
             REXP(1,111+(I-1)*4)=1.                               !D
             REXP(1,108)=1.                                       !D
             IF (MSG) PRINT *,' 61)MORE LOW CLOUD IN 8-GROUP THAN NH'
            ENDIF
          ELSE
            IF (C(I).GE.3.AND.C(I).LE.5 .AND. INH.LT.NS(I)) THEN
              REXP(1,111+(I-1)*4)=1.                               !D
              REXP(1,108)=1.                                       !D
              IF (MSG) PRINT *,'62)MORE MEDIUM CLOUD IN 8-GROUP THAN NH'
            ENDIF
          ENDIF
         ENDDO
*
* FIRST 8-GROUP OBSCURED BUT AMOUNT OF CLOUD REPORTED.
*
        IF (NS(1).EQ.9 .AND. INH.NE.IMIS.AND.INH.LT.9) THEN
          REXP(1,111)=1.                                          !D
          REXP(1,108)=1.                                          !D
          IF (MSG) PRINT *,' 63)NH REPORTED BUT FIRST 8-GROUP OBSCURED'
        ENDIF
*
* LOW/MEDIUM CLOUD REPORTED BUT HEIGHT INCONSISTENT IN 1ST 8-GROUP.
*
        IF (INH.GT.0 .AND. HSHS(1).GE.6000.) THEN
          REXP(1,113)=1.                                          !D
          REXP(1,108)=1.                                          !D
       IF (MSG) PRINT *,' 64)LOW/MEDIUM CLOUD BUT FIRST 8-GP HT HIGHER'
        ENDIF
*****************
*  CL SECTION   *
*****************
*
* LOW CLOUD REPORTED BUT FIRST 8-GROUP NOT LOW CLOUD.
*
        IF (ICL.GE.31 .AND. ICL.LE.39 .AND.
     &      C(1).GE.0 .AND. C(1).LE.5) THEN
          REXP(1,112)=1.                                          !D
          REXP(1,102)=1.                                          !D
       IF (MSG) PRINT *,' 65)LOW CLOUD BUT WRONG TYPE IN FIRST 8-GROUP'
        ENDIF
*
* LOW CLOUD REPORTED BUT FIRST 8-GROUP CLOUD AMOUNT OBSCURED.
*
        IF (ICL.GE.31 .AND. ICL.LE.39 .AND. NS(1).EQ.9) THEN
          REXP(1,111)=1.                                          !D
          REXP(1,102)=1.                                          !D
          IF (MSG) PRINT *,' 66)LOW CLOUD BUT FIRST 8-GROUP OBSCURED'
        ENDIF
*
* LOW CLOUD REPORTED IN FIRST 8-GROUP BUT HEIGHT INCONSISTENT.
*
        IF (ICL.GE.31 .AND. ICL.LE.39 .AND. HSHS(1).GT.2700.) THEN
          REXP(1,102)=1.                                          !D
          REXP(1,113)=1.                                          !D
       IF (MSG) PRINT *,' 67)LOW CLOUD BUT HEIGHT WRONG IN 1ST 8-GROUP'
        ENDIF
*
* CUMULONIMBUS IN MAIN CLOUD GROUP BUT NOT IN 8-GROUPS.
*
        IF (ICL.EQ.33 .OR. ICL.EQ.39) THEN
          DO I=1,N8
           IF (C(I).EQ.9) GO TO 100
          ENDDO
          REXP(1,102)=1.                                          !D
          DO I=1,N8
           REXP(1,112+(I-1)*4)=1.                                 !D
          ENDDO
          IF (MSG) PRINT *,' 68)CB IN MAIN CLOUD GROUP BUT NO 8-GROUP'
        ENDIF
 100    CONTINUE
*
* CUMULONIMBUS IN 8-GROUP BUT NOT IN MAIN CLOUD GROUP.
*
        DO I=1,N8
         IF (C(I).EQ.9 .AND. ICL.NE.33.AND.ICL.NE.39)THEN
           REXP(1,112+(I-1)*4)=1.                                 !D
           REXP(1,102)=1.                                         !D
       IF (MSG) PRINT *,' 69)CB IN 8-GROUP BUT NOT IN MAIN CLOUD GROUP'
         ENDIF
        ENDDO
*****************
*  CM SECTION   *
*****************
*
* NO MEDIUM CLOUD IN MAIN GROUP BUT MEDIUM HEIGHT OR TYPE IN 8-GROUP
*
        IF (ICM.EQ.IMIS .OR. ICM.EQ.20) THEN
          DO I=1,N8
           IF (HSHS(I).GE.2700. .AND. HSHS(I).LE.5100.) THEN
             REXP(1,113+(I-1)*4)=1.                               !1.6
             REXP(1,104)=1.                                       !D
             IF (MSG) PRINT *,' 70)NO CM BUT MEDIUM CLOUD HT IN 8-GROUP'
           ENDIF
*
           IF (C(I).GE.3 .AND. C(I).LE.5) THEN
             REXP(1,112+(I-1)*4)=1.                               !D
             REXP(1,104)=1.                                       !D
       IF (MSG) PRINT *,' 71)NO CM BUT MEDIUM CLOUD TYPE IN 8-GROUP'
           ENDIF
          ENDDO
        ENDIF
*
* NO MEDIUM/HIGH CLOUD IN MAIN GROUP BUT HIGH CLOUD HEIGHT IN 8-GROUP
*
        IF (ICM.EQ.20 .AND. ICH.EQ.10) THEN
          DO I=1,N8
           IF (HSHS(I).GT.5100.) THEN
             REXP(1,113+(I-1)*4)=1.                               !1.6
             REXP(1,104)=1.                                       !D
             REXP(1,106)=1.                                       !D
       IF (MSG) PRINT *,' 72)NO CM OR CH BUT HIGH CLOUD IN 8-GROUP'
           ENDIF
          ENDDO
        ENDIF
*
* FIRST 8-GROUP INDICATES OBSCURED BUT MEDIUM CLOUD REPORTED.
*
        IF (ICM.NE.IMIS .AND. NS(1).EQ.9) THEN
          REXP(1,111)=1.                                          !D
          REXP(1,104)=1.                                          !D
          IF (MSG) PRINT *,' 73)MEDIUM CLOUD BUT FIRST 8-GROUP OBSCURED'
        ENDIF
*
* FIRST 8-GROUP HAS HIGH CLOUD HEIGHT, BUT MEDIUM CLOUD IN MAIN GROUP
*
        IF (ICM.GE.21 .AND. ICM.LE.29 .AND. HSHS(1).GE.6300.) THEN
          REXP(1,113)=1.                                          !D
          REXP(1,104)=1.                                          !D
          IF (MSG) PRINT *,' 74)MEDIUM CLOUD BUT FIRST 8-GROUP HIGH'
        ENDIF
*
* ALTOCUMULUS REPORTED IN 8-GROUP BUT NOT IN MAIN GROUP.
*
        IF (ICM.EQ.21 .OR. ICM.EQ.22) THEN
          DO I=1,N8
           IF (C(I).EQ.3) THEN
             REXP(1,112+(I-1)*4)=1.                               !D
             REXP(1,104)=1.                                       !D
          IF (MSG) PRINT *,' 75)AC IN 8-GROUP BUT NOT MAIN CLOUD GROUP'
           ENDIF
          ENDDO
        ENDIF
*
* NIMBOSTRATUS OR ALTOSTRATUS IN 8-GROUP BUT NOT IN MAIN CLOUD GROUP
*
        IF (ICM.GE.23 .AND. ICM.LE.26) THEN
          DO I=1,N8
           IF (C(I).EQ.4. .OR. C(I).EQ.5.) THEN
             REXP(1,112+(I-1)*4)=1.                               !D
             REXP(1,104)=1.                                       !D
             IF (MSG) PRINT *,' 76)NS OR AS IN 8-GROUP BUT NOT MAIN'
           ENDIF
          ENDDO
        ENDIF
*
* MEDIUM (BUT NO LOW) CLOUD PRESENT, BUT NO 8-GROUPS FOR MEDIUM CLOUD.
*
        IF (ICM.GE.21.AND.ICM.LE.29 .AND. ICL.EQ.30) THEN
          DO I=1,N8
           IF (C(I).GE.3.AND.C(I).LE.5) GO TO 104
          ENDDO
*
          DO I=1,N8
           REXP(1,112+(I-1)*4)=1.                                 !D
          ENDDO
          REXP(1,104)=1.                                          !D
          REXP(1,106)=1.                                          !D
          IF (MSG) PRINT *,' 77)CM & NO CL, BUT NO 8-GROUPS FOR MEDIUM'
        ENDIF
  104   CONTINUE
*****************
*  CH SECTION   *
*****************
*
* NO HIGH CLOUD IN MAIN GROUP BUT HIGH CLOUD TYPE OR HEIGHT IN 8-GROUP
*
        IF (ICH.EQ.10 .OR. ICH.EQ.IMIS) THEN
          DO I=1,N8
           IF (C(I).GE.0 .AND. C(I).LE.2) THEN
             REXP(1,112+(I-1)*4)=1.                               !D
             REXP(1,106)=1.                                       !D
             IF (MSG) PRINT *,' 78)NO CH BUT HIGH CLOUD TYPE IN 8-GROUP'
           ENDIF
*
           IF (HSHS(I).GE.6300.) THEN
             REXP(1,113+(I-1)*4)=1.                               !D
             REXP(1,106)=1.                                       !D
          IF (MSG) PRINT *,' 79)NO CH BUT HIGH CLOUD HEIGHT IN 8-GROUP'
           ENDIF
          ENDDO
        ENDIF
*
* FIRST 8-GROUP INDICATES OBSCURED BUT HIGH CLOUD REPORTED.
*
        IF (ICH.NE.IMIS .AND. NS(1).EQ.9) THEN
          REXP(1,111)=1.                                          !D
          REXP(1,106)=1.                                          !D
          IF (MSG) PRINT *,' 80)HIGH CLOUD BUT FIRST 8-GROUP OBSCURED'
        ENDIF
*
* CIRROSTRATUS REPORTED IN 8-GROUP BUT NOT IN MAIN GROUP.
*
        IF (ICH.GE.11 .AND. ICH.LE.14) THEN                       !1.6
          DO I=1,N8
           IF (C(I).EQ.2.) THEN
             REXP(1,112+(I-1)*4)=1.                               !D
             REXP(1,106)=1.                                       !D
          IF (MSG) PRINT *,' 81)CS IN 8-GROUP BUT NOT MAIN CLOUD GROUP'
           ENDIF
          ENDDO
        ENDIF
*****************
*  INTERNAL     *
*****************
*
* HEIGHT IN 8-GROUP TOO HIGH FOR LOW CLOUD.
*
        DO I=1,N8
         IF (HSHS(I).GT.2700. .AND. C(I).GT.6.) THEN
           REXP(1,112+(I-1)*4)=1.                                 !D
           REXP(1,113+(I-1)*4)=1.                                 !D
           IF (MSG) PRINT *,' 82)8-GROUP HEIGHT WRONG FOR LOW CLOUD'
         ENDIF
        ENDDO
*
* CLOUD HEIGHT IN 8-GROUP FOR MEDIUM CLOUD BUT CLOUD TYPE NOT MEDIUM.
*
        DO I=1,N8
         IF ((C(I).LT.3.OR.C(I).GT.5) .AND.
     &       HSHS(I).GE.2700. .AND. HSHS(I).LE.5100.) THEN
           REXP(1,112+(I-1)*4)=1.                                 !D
           REXP(1,113+(I-1)*4)=1.                                 !D
           IF (MSG) PRINT *,' 83)HEIGHT MEDIUM IN 8-GROUP BUT NOT TYPE'
         ENDIF
        ENDDO
*
* HEIGHT IN 8-GROUP IMPLIES HIGH CLOUD BUT TYPE NOT HIGH.
*
        DO I=1,N8
         IF (HSHS(I).GE.5400. .AND. C(I).GT.2.) THEN
           REXP(1,112+(I-1)*4)=1.                                 !D
           REXP(1,113+(I-1)*4)=1.                                 !D
           IF (MSG) PRINT *,' 84)8-GROUP HEIGHT HIGH BUT TYPE NOT'
         ENDIF
        ENDDO
*
* INVALID 8-GROUP, TYPE MISSING BUT AMOUNT NOT MISSING.
*
        DO I=1,N8
         IF (C(I).EQ.IMIS .AND. NS(I).GT.0..AND.NS(I).LT.9.) THEN
           REXP(1,111+(I-1)*4)=1.                                 !D
           REXP(1,112+(I-1)*4)=1.                                 !D
           REXP(1,113+(I-1)*4)=1.                                 !D
           IF (MSG) PRINT *,' 85)8-GROUP AMOUNT BUT NO TYPE'
         ENDIF
        ENDDO
*
* INSUFFICIENT CLOUD AMOUNTS FOR SEQUENCE OF 8-GROUPS.
*
        IF (N8.EQ.4) THEN
          DO I=2,4
           IF (NS(I).LT.NMAND(I-1)) THEN
             REXP(1,111+(I-1)*4)=1.                               !D
             REXP(1,112+(I-1)*4)=1.                               !D
             REXP(1,113+(I-1)*4)=1.                               !D
           IF (MSG) PRINT *,' 86)NOT ENOUGH CLOUD FOR 8-GROUP SEQUENCE'
           ENDIF
          ENDDO
        ENDIF
      ENDIF
      RETURN
      END
