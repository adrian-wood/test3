SUBROUTINE SYNQC(REXP,WXMAN,IR,IX,N8,WPER,MSG,SYNTAX,SHIP)

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
! ARGUMENTS     : (1) REXP    EXPANSION ARRAY
!                 (2) WXMAN   TRUE FOR MANUAL STATION
!                 (3) IR      RAINFALL GROUP INDICATOR
!                 (4) IX      WEATHER GROUP INDICATOR
!                 (5) N8      NUMBER OF 8-GROUPS (UP TO 4)
!                 (6) WPER    WEATHER PERIOD (-6, -3 OR -1)
!                 (7) MSG     .TRUE. if diagnostic test messages
!                 (8) SYNTAX  TRUE IF THERE IS A SYNTAX ERROR
!                 (9) SHIP    TRUE IF REPORT IS FROM A SHIP
!
! REXP(2,N) HAS FLAGS IN REXP(1,N) & VALUES IN REXP(2,N)
! ONLY THE FOLLOWING ELEMENTS ARE CHECKED:
!   15        DD           WIND DIRECTION  (990 IF VARIABLE)
!   16        FF           WIND SPEED      (M/S)
!   17        T            TEMPERATURE     (KELVIN)
!   18        TD           DEW POINT       (KELVIN)
!   20        VV           VISIBILITY      (METRES, SEE BELOW)
!   22        WW           PRESENT WEATHER (BUFR TABLE 020003)
!   24        W1           PAST WEATHER    (BUFR TABLE 020004)
!   25        W2           PAST WEATHER
!   26        N            CLOUD AMOUNT
!   33        RAINFALL IN SECTION 1
!   35        RAINFALL IN SECTION 3
!   49        A            PRESSURE TENDENCY
!   50        PP           PRESSURE TENDENCY (PASCALS)
!  102        CL           LOW CLOUD TYPE  (BUFR TABLE 020012)
!  104        CM           MIDDLE CLOUD TYPE    (TABLE 020012)
!  106        CH           HIGH CLOUD TYPE (BUFR TABLE 020012)
!  108        NH           LOW OR MEDIUM CLOUD AMOUNT
!  109        H            HEIGHT OF LOWEST CLOUD (METRES, SEE BELOW)
!  111,115... NS           CLOUD AMOUNT    (8-GROUPS)
!  112,116... C            CLOUD TYPE      (8-GROUPS)
!  113,117... HSHS         CLOUD HEIGHT    (8-GROUPS) (METRES)
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
!
! $Workfile: synqc.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 10/01/2011 14:51:06$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         10/01/2011 14:51:06    Rosemary Lavery
!       corrections after review
!  1    MetDB_Refresh 1.0         04/01/2011 16:36:47    Rosemary Lavery
!       Initial Import
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

USE METDB_COM_mod, ONLY : MISSIN, RMISS     ! missing data value

IMPLICIT NONE

! Interface Arguments

REAL, INTENT(INOUT)     :: REXP(2,*) ! (A1)
LOGICAL, INTENT(IN)     :: WXMAN     ! (A2) TRUE IF STATION MANNED
INTEGER, INTENT(IN)     :: IR        ! (A3)
INTEGER, INTENT(IN)     :: IX        ! (A4)
INTEGER, INTENT(IN)     :: N8        ! (A5)
REAL, INTENT(IN)        :: WPER      ! (A6)
LOGICAL, INTENT(IN)     :: MSG       ! (A7) TRUE IF QC MESSAGES ARE TO BE OUTPUT (TEST)
LOGICAL, INTENT(OUT)    :: SYNTAX    ! (A8) TRUE IF THERE IS A SYNTAX ERROR
LOGICAL, INTENT(IN)     :: SHIP      ! (A9) TRUE IF REPORT IS FROM A SHIP

! Local Variables

INTEGER  :: ATEND
INTEGER  :: I
INTEGER  :: ICH
INTEGER  :: ICM
INTEGER  :: ICL
INTEGER  :: IH
INTEGER  :: IN
INTEGER  :: INH
INTEGER  :: IRRR
INTEGER  :: IRRR3
INTEGER  :: IWW
INTEGER  :: IW1
INTEGER  :: IW2
INTEGER  :: NMAND(3) = (/3,5,8/)
INTEGER  :: PCHANG

REAL     :: C(4)
REAL     :: DD
REAL     :: FF
REAL     :: HSHS(4)
REAL     :: NS(4)
REAL     :: TDTD
REAL     :: TTT
REAL     :: VVV

LOGICAL  :: LF
LOGICAL  :: WWREPT  ! TRUE IF PRESENT WEATHER REPORTED
LOGICAL  :: WXGRP   ! TRUE IF PRESENT OR PAST WEATHER REPORTED

!----------------------------------------------------------------------

! Initialize
SYNTAX=.FALSE.

!**********************************************************************

!  IR & RAINFALL

!**********************************************************************
IRRR=REXP(2,33)
IRRR3=REXP(2,35)

! IR INDICATED RAINFALL IN SECTIONS 1 AND 3 BUT NONE FOUND

IF (IR == 0 .AND. IRRR == MISSIN .AND. IRRR3 == MISSIN) THEN
  SYNTAX=.TRUE.
  IF (MSG) PRINT *,' 1)IR=0 BUT NO RAINFALL IN SECTION 1 OR 3'
END IF

! IR INDICATED RAINFALL IN SECTION 1 BUT NONE FOUND

IF (IR == 1 .AND. IRRR == MISSIN) THEN
  SYNTAX=.TRUE.
  IF (MSG) PRINT *,' 2)IR=1 BUT NO RAINFALL IN SECTION 1'
END IF

! IR INDICATED RAINFALL IN SECTION 3 BUT NONE FOUND

IF (IR == 2 .AND. IRRR3 == MISSIN) THEN
  SYNTAX=.TRUE.
  IF (MSG) PRINT *,' 3)IR=2 BUT NO RAINFALL IN SECTION 3'
END IF

! IR INDICATED NO RAINFALL BUT RAINFALL FOUND

IF (IR == 3 .OR. IR == 4) THEN
  IF (IRRR /= MISSIN .AND. IRRR /= 0) THEN
    REXP(1,33)=1.
    IF (MSG) PRINT *,' 4)IR>2 BUT RAINFALL IN SECTION 1'
  END IF
  IF (IRRR3 /= MISSIN) THEN
    REXP(1,35)=1.
    IF (MSG) PRINT *,' 5)IR>2 BUT RAINFALL IN SECTION 3'
  END IF
END IF
!**********************************************************************

!  IX & WEATHER

!**********************************************************************
IWW=REXP(2,22)
IW1=REXP(2,24)
IW2=REXP(2,25)

WXGRP=.FALSE.
WWREPT=.FALSE.
IF(IWW /= MISSIN .AND. IWW /= 508 .AND. IWW /= 509)WWREPT=.TRUE.
IF (WWREPT .OR. IW1 /= MISSIN .OR. IW2 /= MISSIN) WXGRP=.TRUE.

! IX INDICATED NO WEATHER GROUP, BUT WEATHER GROUP FOUND (SUSPECT ID?)

IF ((IX == 2.OR.IX == 3.OR.IX == 5.OR.IX == 6) .AND. WXGRP) THEN
  REXP(1,22)=1.
  REXP(1,24)=1.
  REXP(1,25)=1.
  IF (MSG) PRINT *,' 6)WEATHER GROUP REPORTED DESPITE IX'
END IF

! IX INDICATED WEATHER GROUP, BUT NO WEATHER GROUP FOUND.

IF((IX == 1.OR.IX == 4.OR.IX == 7) &
    .AND. .NOT.(WXGRP .OR. IWW == 510)) THEN
  SYNTAX=.TRUE.
  IF (MSG) PRINT *,' 7)NO WEATHER GROUP REPORTED DESPITE IX'
END IF
!**********************************************************************

!  WEATHER (WW, W1 & W2)

!**********************************************************************

! PRESENT AND PAST WEATHER INSIGNIFICANT BUT WEATHER GROUP
! REPORTED  (ALLOWED FOR SHIPS BUT NOT FOR LAND STATIONS
!            SEE MANUAL ON CODES 12.2.6.1 AND 12.2.6.2).

IFBLOCK1: &
IF (.NOT.SHIP) THEN
  IF (WXMAN) THEN
    IF (IWW >= 0.AND.IWW <= 3 .AND. IW1 >= 0.AND.IW1 <= 2 &
        .AND. IW2 >= 0.AND.IW2 <= 2) THEN
      REXP(1,22)=1.
      REXP(1,24)=1.
      REXP(1,25)=1.
    END IF
  ELSE
    IF (IWW >= 100 .AND. IWW <= 103 .AND. &
        IW1 == 10 .AND. IW2 == 10) THEN
      REXP(1,22)=1.
      REXP(1,24)=1.
      REXP(1,25)=1.
    END IF
  END IF
END IF IFBLOCK1

! PAST WEATHER INDICATES FOG IN THE PAST HOUR BUT PRESENT WEATHER
! DOESN''T MENTION FOG AND NO THUNDERSTORM REPORTED.

IFBLOCK2: &
IF (WXMAN) THEN
  IF ((IW1 == 4.OR.IW2 == 4) .AND. INT(WPER) == -1 .AND. &
      ((IWW >= 0 .AND.IWW < 28 .AND. IWW /= 17) .OR.     &
        .NOT.WWREPT)) THEN
    REXP(1,22)=1.
    REXP(1,24)=1.
    REXP(1,25)=1.
    IF (MSG) PRINT *,' 10)WW INCONSISTENT WITH FOG IN PAST HOUR'
  END IF
ELSE
  IF ((IW1 == 13.OR.IW2 == 13) .AND. INT(WPER) == -1 .AND. &
      ((IWW >= 100 .AND.IWW < 130 .AND. IWW /= 126) .OR.   &
        .NOT.WWREPT)) THEN
    REXP(1,22)=1.
    REXP(1,24)=1.
    REXP(1,25)=1.
    IF (MSG) PRINT *,' 11)WW INCONSISTENT WITH FOG IN PAST HOUR'
  END IF
END IF IFBLOCK2

! SECOND PAST WEATHER CODE IS GREATER THAN THE FIRST.

IF (IW2 > IW1) THEN
  REXP(1,24)=1.
  REXP(1,25)=1.
IF (MSG) PRINT *,' 12)SECOND PAST WEATHER CODE GREATER THAN FIRST'
END IF
!**********************************************************************

!  WIND

!**********************************************************************
DD=REXP(2,15)               ! MAY BE 990, "VARIABLE"
FF=REXP(2,16)

! WIND DIRECTION INDICATES CALM BUT WIND SPEED REPORTED.

IF (DD == 0 .AND. FF > 0.) THEN
  REXP(1,15)=1.
  REXP(1,16)=1.
  IF (MSG) PRINT *,' 13)WIND DIRECTION CALM BUT SPEED REPORTED'
END IF

! WIND SPEED INDICATES CALM BUT WIND DIRECTION REPORTED.

IF (FF == 0 .AND. DD > 0.) THEN
  REXP(1,15)=1.
  REXP(1,16)=1.
  IF (MSG) PRINT *,' 14)WIND SPEED CALM BUT DIRECTION REPORTED'
END IF

! WIND DIRECTION RANGE CHECK
! (DD=990, "VARIABLE", WILL BE ENCODED AS DD=0)

IF (DD > 360. .AND. DD /= 990) THEN
  REXP(1,15)=1.
  IF (MSG) PRINT *,' 15)WIND DIRECTION IMPOSSIBLE'
END IF

! VARIABLE WIND DIRECTION & INCONSISTENT SPEED (0 OR MORE THAN 5M/S)

!     IF (DD == 990 .AND. (FF == 0 .OR. FF > 5)) THEN
!       REXP(1,15)=1.
!       REXP(1,16)=1.
!       IF (MSG) PRINT *,' 16)VARIABLE WIND BUT SPEED ZERO OR TOO HIGH'
!     END IF
!**********************************************************************

!  PRESSURE TENDENCY (APP)

!**********************************************************************
ATEND=REXP(2,49)
PCHANG=REXP(2,50)

! PRESSURE CHARACTERISTIC INDICATES STEADY, BUT PRESSURE CHANGE EXISTS

IF (ATEND == 4 .AND. PCHANG /= 0) THEN
  REXP(1,49)=1.
  REXP(1,50)=1.
  IF (MSG) PRINT *,' 17)PRESSURE STEADY BUT NONZERO CHANGE'
END IF

! PRESSURE CHARACTERISTIC INDICATES CHANGE, BUT NO PRESSURE CHANGE.

! 9 changed to 8 as OPR says 8 with no pressure changes is valid
! whilst 9 was a mistake.
IF (PCHANG == 0 .AND. ATEND /= 0.AND.ATEND /= 4.AND.ATEND /= 5 &
   .AND. ATEND /= 8 .AND. ATEND /= MISSIN) THEN
  REXP(1,49)=1.
  REXP(1,50)=1.
  IF (MSG) PRINT *,' 18)NO PRESSURE CHANGE DESPITE CHARACTERISTIC'
END IF
!**********************************************************************

!  TEMPERATURE RANGE, TEMPERATURES AND WEATHER

!**********************************************************************
TTT=REXP(2,17)
TDTD=REXP(2,18)

! DEW POINT HIGHER THAN TEMPERATURE AT LAND STATION (BLOCK NUMBER SET)
! OR MORE THAN 1 DEGREE HIGHER ON SHIP (WMO BLOCK MISSING)

IF (TTT /= RMISS .AND. TDTD /= RMISS .AND. &
   (.NOT.SHIP .AND. TDTD > TTT) .OR.   &
   (SHIP .AND. TDTD > TTT+1.)) THEN
  REXP(1,17)=1.
  REXP(1,18)=1.
IF (MSG) PRINT *,' 19)DEW POINT TOO HIGH FOR TEMPERATURE',TDTD,TTT
END IF

! FREEZING RAIN, SNOW, RIME ETC BUT TTT 10C OR MORE

IFBLOCK3: &
IF (WXMAN) THEN
  IF (TTT > 283.2 .AND. &
   ((IWW >= 66.AND.IWW <= 79).OR.(IWW >= 83.AND.IWW <= 88).OR. &
    IWW == 24.OR.IWW == 48.OR.IWW == 49.OR.IWW == 56)) THEN
    REXP(1,17)=1.
    REXP(1,22)=1.
    IF (MSG) PRINT *,' 20)TEMPERATURE TOO HIGH FOR SNOW, RIME ETC'
  END IF
ELSE
  IF (TTT > 283.2 .AND.             &
   ((IWW >= 164.AND.IWW <= 166).OR. &
    (IWW >= 185.AND.IWW <= 187).OR. &
    IWW == 125 .OR. IWW == 135 .OR. &
    IWW == 154 .OR. IWW == 155 .OR. IWW == 156)) THEN
    REXP(1,17)=1.
    REXP(1,22)=1.
    IF (MSG) PRINT *,' 21)TEMPERATURE TOO HIGH FOR SNOW, RIME ETC'
  END IF
END IF IFBLOCK3

! DEW POINT DEPRESSION TOO GREAT FOR FOG

! T-Td put back, ww 40-49 changed to 41-49
IF (TTT-TDTD > 5. .AND. IWW >= 41 .AND. IWW <= 49) THEN
  REXP(1,17)=1.
  REXP(1,18)=1.
  REXP(1,22)=1.
  IF (MSG) PRINT *,' 22)DEW POINT DEPRESSION TOO GREAT FOR FOG'
END IF
!**********************************************************************

!  VISIBILITY & WEATHER

!**********************************************************************
VVV=REXP(2,20)

! VISIBILITY < 1 KM, BUT PRESENT WEATHER INCONSISTENT.

IFBLOCK4: &
IF (VVV < 1000.) THEN

IFBLOCK5: &
  IF (WXMAN) THEN
! improve readability of if statement.
    IF(IWW < 4 .OR. (IWW >= 5 .AND. IWW <= 28) &
       .OR. IWW == 40 ) THEN
      REXP(1,22)=1.
      REXP(1,20)=1.
      IF (MSG) PRINT *,' 23)WEATHER WRONG FOR VISIBILITY <1 KM'
    END IF
  ELSE
! Allow for reserved ranges being used (IWW 106-109)
    IF((IWW >= 100.AND.IWW <= 104).OR. &
       (IWW >= 106.AND.IWW <= 119).OR. &
       (IWW == 128))THEN
      REXP(1,22)=1.
      REXP(1,20)=1.
      IF (MSG) PRINT *,' 24)WEATHER WRONG FOR VISIBILITY <1 KM'
    END IF
  END IF IFBLOCK5

END IF IFBLOCK4

! VISIBILITY >= 1 KM, BUT FOG REPORTED.

IFBLOCK6: &
IF (VVV >= 1000.) THEN
  IF (WXMAN) THEN
    IF (IWW >= 41 .AND. IWW <= 49) THEN
      REXP(1,22)=1.
      REXP(1,20)=1.
      IF (MSG) PRINT *,' 25)VISIBILITY >1 KM BUT FOG REPORTED'
    END IF
  ELSE
! flag if ww 130-135 (Fog <1Km )
    IF (IWW >= 130 .AND. IWW <= 135 ) THEN
      REXP(1,22)=1.
      REXP(1,20)=1.
      IF (MSG) PRINT *,' 26)VISIBILITY >1 KM BUT FOG REPORTED'
    END IF
  END IF
END IF IFBLOCK6

! VISIBILITY >= 2 KM, BUT HEAVY SNOW REPORTED.

IF (WXMAN) THEN
  IF (VVV >= 2000. .AND. (IWW == 74.OR.IWW == 75)) THEN
    REXP(1,20)=1.
    REXP(1,22)=1.
    IF (MSG) PRINT *,' 27)VISIBILITY >2 KM BUT HEAVY SNOW'
  END IF
ELSE
  IF (VVV >= 2000. .AND. IWW == 173) THEN
    REXP(1,20)=1.
    REXP(1,22)=1.
    IF (MSG) PRINT *,' 28)VISIBILITY >2 KM BUT HEAVY SNOW'
  END IF
END IF

! VVV>1KM BUT WEATHER IS HAZE WITH VISIBILITY <1KM  (N.B. THIS CHECK
! IS FOR AUTOMATIC STATIONS ONLY: NO VISIBILITY IN MANUAL HAZE CODE)

IF (.NOT.WXMAN) THEN
  IF (VVV >= 1000. .AND. IWW == 105) THEN
    REXP(1,20)=1.
    REXP(1,22)=1.
    IF (MSG) PRINT *,' 29)VISIBILITY >1 KM BUT WW=105'
  END IF
END IF
!**********************************************************************

!  TOTAL CLOUD AMOUNT (N) & HEIGHT OF LOWEST CLOUD (H)

!**********************************************************************
IH=REXP(2,109)
IN=REXP(2,26)

! SKY NOT OBSCURED BUT NO LOW/MEDIUM CLOUD BASE REPORTED.

IF (IN /= 9 .AND. IN /= MISSIN .AND. IH == MISSIN) THEN
  REXP(1,109)=1.
  REXP(1,26)=1.
  IF (MSG) PRINT *,' 30)SKY NOT OBSCURED BUT NO CLOUD BASE'
END IF

! NO CLOUD AMOUNT BUT CLOUD BASE REPORTED.

! Decision made that if a 0 is reported for lowest cloud
! yet the lowest cloud group is reported as '/' (missing) then
! this should be flagged. change made to flag if IH missing.
IF (IN == 0 .AND. IH < 2500) THEN
  REXP(1,26)=1.
  REXP(1,109)=1.
  IF (MSG) PRINT *,' 31)CLOUD BASE BUT NO CLOUD AMOUNT'
END IF
!**********************************************************************

!  TOTAL CLOUD AMOUNT (N), VISIBILITY & WEATHER

!**********************************************************************

! TOTAL CLOUD AMOUNT OBSCURED BUT VISIBILITY 2KM OR MORE

IF (IN == 9 .AND. VVV >= 2000.) THEN
  REXP(1,26)=1.
  REXP(1,20)=1.
  IF (MSG) PRINT *,' 32)CLOUD OBSCURED BUT VISIBILITY >2 KM'
END IF

! check 33 deleted: ww=0 can mean sky not observed, not cloud
! cloud missing

! TOTAL CLOUD AMOUNT OBSCURED BUT SKY VISIBLE.

IFBLOCK7: &
IF (WXMAN) THEN
  IF (IN == 9 .AND. ((IWW >= 0.AND.IWW <= 29).OR.IWW == 40.OR. &
     IWW == 42.OR.IWW == 44.OR.IWW == 46.OR.IWW == 48))THEN
    REXP(1,22)=1.
    REXP(1,26)=1.
    IF (MSG) PRINT *,' 34)CLOUD OBSCURED BUT SKY VISIBLE'
  END IF
ELSE
! flag only Iww between 100 & 103 as these imply sky visible
  IF (IN == 9 .AND. IWW >= 100.AND.IWW <= 103) THEN
    REXP(1,22)=1.
    REXP(1,26)=1.
    IF (MSG) PRINT *,' 35)CLOUD OBSCURED BUT SKY VISIBLE'
  END IF
END IF IFBLOCK7

! NO TOTAL CLOUD AMOUNT BUT CLOUDS FORMING OR PRECIPITATION REPORTED.

IFBLOCK8: &
IF(WXMAN)THEN
  IF((IN == 0.OR.IN == MISSIN).AND.(IWW == 3.OR. &
     (IWW >= 14.AND.IWW <= 17).OR.            &
     (IWW >= 50.AND.IWW <= 75).OR.            &
     (IWW >= 77.AND.IWW <= 99)))THEN
    REXP(1,22)=1.
    REXP(1,26)=1.
    IF (MSG) PRINT *,' 36)NO TOTAL CLOUD AMOUNT DESPITE WEATHER'
  END IF
ELSE
! decimal point removed
  IF((IN == 0.OR.IN == MISSIN).AND.(IWW == 103 .OR. &
     (IWW >= 150.AND.IWW <= 199)))THEN
    REXP(1,22)=1.
    REXP(1,26)=1.
    IF (MSG) PRINT *,' 37)NO TOTAL CLOUD AMOUNT DESPITE WEATHER'
  END IF
END IF IFBLOCK8

! HALF CLOUD COVER OR LESS BUT PAST WEATHER INDICATES MORE THAN
! HALF CLOUD COVER THROUGHOUT PERIOD.

IF (IN >= 0 .AND. IN <= 4 .AND. (IW1 == 2.OR.IW2 == 2))THEN
  REXP(1,26)=1.
  REXP(1,24)=1.
  REXP(1,25)=1.
  IF (MSG) PRINT *,' 38)CLOUD COVER & PAST WEATHER INCONSISTENT'
END IF

! MORE THAN HALF CLOUD COVER BUT PAST WEATHER INDICATES HALF CLOUD
! COVER OR LESS THROUGHOUT PERIOD.

IF (IN >= 5 .AND. IN <= 8 .AND. (IW1 == 0.OR.IW2 == 0))THEN
  REXP(1,26)=1.
  REXP(1,24)=1.
  REXP(1,25)=1.
  IF (MSG) PRINT *,' 39)CLOUD COVER & PAST WEATHER INCONSISTENT'
END IF
!**********************************************************************
!**********************************************************************

!  MAIN CLOUD GROUP (8-GROUP IN SECTION 1)
!  WW & MAIN CLOUD GROUP

!**********************************************************************
!**********************************************************************
INH=REXP(2,108)
ICL=REXP(2,102)
ICM=REXP(2,104)
ICH=REXP(2,106)

! THUNDERSTORM AT OR IN SIGHT OF THE STATION, CLOUD AMOUNT NONZERO,
! BUT NO SUITABLE CLOUD IN 8-GROUP (ALLOW FOR A BLANKET OF LOW CLOUD).

IFBLOCK9: &
IF (IN >= 0.AND.IN <= 8 .AND. ICL /= 33.AND.ICL /= 39 .AND. &
    ICM /= 28.AND.ICM /= 29 .AND. ICH /= 13 .AND.           &
    ICL /= 36.AND.ICL /= 37 .AND. (INH == 7.OR.INH == 8)) THEN
  IF (WXMAN) THEN
    IF (IWW == 17 .OR. (IWW >= 95.AND.IWW <= 99)) THEN
      REXP(1,22)=1.        ! FLAG WW & CL,CM,CH
      REXP(1,102)=1.
      REXP(1,104)=1.
      REXP(1,106)=1.
      IF (MSG) PRINT *,' 40)THUNDERSTORM BUT NO SUITABLE CLOUD'
    END IF
  ELSE
    IF (IWW == 117 .OR. IWW == 191 .OR.  &
       (IWW >= 193.AND.IWW <= 196)) THEN
      REXP(1,22)=1.
      REXP(1,102)=1.
      REXP(1,104)=1.
      REXP(1,106)=1.
      IF (MSG) PRINT *,' 41)THUNDERSTORM BUT NO SUITABLE CLOUD'
    END IF
  END IF
END IF IFBLOCK9
!**********************************************************************

!  H & MAIN CLOUD GROUP

!**********************************************************************

! CLOUD BASE IS MISSING BUT LOW/MEDIUM CLOUD AMOUNT IS REPORTED.

IF (IH == MISSIN .AND. INH /= 9 .AND. INH /= MISSIN) THEN
  REXP(1,108)=1.
  REXP(1,109)=1.
  IF (MSG) PRINT *,' 42)LOW/MEDIUM CLOUD BUT NO CLOUD BASE'
END IF

! CLOUD BASE REPORTED BUT NO AMOUNT OF LOW/MEDIUM CLOUD IS REPORTED.

IF (INH == 0 .AND. IH >= 0.AND.IH < 2500) THEN
  REXP(1,108)=1.
  REXP(1,109)=1.
  IF (MSG) PRINT *,' 43)CLOUD BASE BUT NO LOW/MEDIUM CLOUD'
END IF
!**********************************************************************

!  N & MAIN CLOUD GROUP

!**********************************************************************

! TOTAL CLOUD AMOUNT IS LESS THAN LOW/MEDIUM CLOUD AMOUNT.

IF(IN < INH)THEN
  REXP(1,26)=1.
  REXP(1,108)=1.
  IF (MSG) PRINT *,' 44)TOTAL CLOUD LESS THAN LOW/MEDIUM CLOUD'
END IF

! SKY OBSCURED BUT LOW OR MEDIUM CLOUD PRESENT.

! flag if a value given to INH but IN states sky obscured
! OPR have said that a INH value of 9 is invalid
IF (IN == 9 .AND. INH /= MISSIN) THEN
  REXP(1,26)=1.
  REXP(1,108)=1.
  IF (MSG) PRINT *,' 45)SKY OBSCURED BUT LOW/MEDIUM CLOUD PRESENT'
END IF

! NO MEDIUM/HIGH CLOUD, BUT NOT ALL CLOUD IS IN LOWEST LAYER.

IF (INH /= MISSIN .AND. IN /= INH .AND. &
    ICM == MISSIN .AND. ICH == MISSIN) THEN
  REXP(1,26)=1.           ! FLAG N & NH
  REXP(1,108)=1.
  IF (MSG) PRINT *,' 46)NOT ALL CLOUD LOWEST, BUT NO MEDIUM/HIGH'
END IF

! NO LOW/HIGH CLOUD, BUT NOT ALL CLOUD IS IN MEDIUM LAYER.

IF (INH /= MISSIN .AND. IN /= INH .AND. &
    ICL == MISSIN .AND. ICH == MISSIN) THEN
  REXP(1,26)=1.           ! FLAG N & NH
  REXP(1,108)=1.
  IF (MSG) PRINT *,' 47)NOT ALL CLOUD MEDIUM, BUT NO LOW/HIGH'
END IF

! CIRROSTRATUS COVERS SKY BUT CLOUD AMOUNT NOT 8/8.

IF(IN < 8.AND.ICH == 17)THEN
  REXP(1,26)=1.
  REXP(1,106)=1.
  IF (MSG) PRINT *,' 48)CS COVERS SKY BUT AMOUNT NOT 8/8'
END IF

! TOTAL CLOUD AMOUNT REPORTED BUT NO LOW, MEDIUM OR HIGH CLOUD.

IF (ICL == 30 .AND. ICM == 20 .AND. ICH == 10) THEN
  IF (IN >= 1 .AND. IN <= 8) THEN
    REXP(1,26)=1.
    REXP(1,102)=1.
    REXP(1,104)=1.
    REXP(1,106)=1.
    IF (MSG) PRINT *,' 49)TOTAL CLOUD BUT NO LOW, MEDIUM OR HIGH'
  END IF
END IF

! NO TOTAL CLOUD AMOUNT BUT LOW, MEDIUM OR HIGH CLOUD REPORTED.

IF (ICL > 30 .OR. ICM > 20 .OR. ICH > 10) THEN
  IF (IN == 0) THEN
    REXP(1,26)=1.
    REXP(1,102)=1.
    REXP(1,104)=1.
    REXP(1,106)=1.
    IF (MSG) PRINT *,' 50)NO TOTAL CLOUD BUT LOW, MEDIUM OR HIGH'
  END IF
END IF

! SKY OBSCURED BUT CLOUD TYPE REPORTED (PERHAPS AS ZERO)

IF (IN == 9 .AND. (ICH >= 10.OR.ICM >= 20.OR.ICL >= 30)) THEN
  REXP(1,26)=1.
  REXP(1,102)=1.
  REXP(1,104)=1.
  REXP(1,106)=1.
  IF (MSG) PRINT *,' 51)SKY OBSCURED BUT CLOUD TYPE REPORTED'
END IF
!**********************************************************************

!  MAIN CLOUD GROUP: INTERNAL CHECKS

!**********************************************************************

! LOW CLOUD MISSING BUT MEDIUM CLOUD REPORTED.

IF (ICM /= MISSIN .AND. ICL == MISSIN) THEN
  REXP(1,104)=1.    ! ONLY CM FLAGGED (CL MISSING)
  IF (MSG) PRINT *,' 52)MEDIUM CLOUD, BUT LOW CLOUD MISSING'
END IF

! LOW AND MEDIUM CLOUD MISSING BUT HIGH CLOUD REPORTED.

IF (ICH /= MISSIN .AND. ICL == MISSIN .AND. ICM == MISSIN) THEN
  REXP(1,106)=1.    ! ONLY CH FLAGGED (CL & CM MISSING)
  IF (MSG) PRINT *,' 53)HIGH CLOUD, BUT LOW/MEDIUM CLOUD MISSING'
END IF

! NO CLOUD AMOUNT REPORTED BUT LOW CLOUD TYPE GIVEN.

IF ((INH == MISSIN .OR. INH == 0) .AND. ICL > 30) THEN
  REXP(1,102)=1.
  REXP(1,108)=1.
  IF (MSG) PRINT *,' 54)LOW CLOUD TYPE, BUT NO CLOUD AMOUNT'
END IF

! CLOUD AMOUNT REPORTED BUT NO LOW CLOUD TYPE GIVEN.

IF (INH < 9.AND.INH > 0 .AND. ICL == MISSIN) THEN
  REXP(1,102)=1.
  REXP(1,108)=1.
  IF (MSG) PRINT *,' 55)CLOUD AMOUNT, BUT NO LOW CLOUD TYPE'
END IF

! MEDIUM CLOUD REPORTED BUT NO CLOUD AMOUNT.

IF (INH == 0 .AND. ICM > 20) THEN
  REXP(1,104)=1.
  REXP(1,108)=1.
  IF (MSG) PRINT *,' 56)MEDIUM CLOUD, BUT NO CLOUD AMOUNT'
END IF

! 8/8 LOW/MEDIUM CLOUD AMOUNT BUT CH NOT MISSING.

IF (INH == 8 .AND. ICH > 10) THEN   ! ICH=60 not set  in
                                    ! MetDB, so not tested
  REXP(1,106)=1.
  REXP(1,108)=1.
  IF (MSG) PRINT *,' 57)8/8 LOW/MEDIUM CLOUD, BUT CH NOT MISSING'
END IF
!**********************************************************************
!**********************************************************************

!  CLOUD CHECKS INVOLVING 8-GROUPS IN SECTION 3
! - ALL THE TESTS FROM HERE TO THE END ASSUME 8-GROUPS IN SECTION 3

!**********************************************************************
!**********************************************************************

IF8GP: &
IF (N8 > 0) THEN

! CLOUD NOT OBSERVED, TOTAL CLOUD IS 0, BUT 8-GROUPS REPORTED.
!  (N & WW AGREE, SO THE 8-GROUPS ARE ASSUMED TO BE WRONG)

  IF (IWW == 0 .AND. IN == 0.) THEN
    DO I=1,N8
     REXP(1,111+(I-1)*4)=1.
     REXP(1,112+(I-1)*4)=1.
     REXP(1,113+(I-1)*4)=1.
     IF (MSG) PRINT *,' 58)8-GROUPS THOUGH TOTAL CLOUD ZERO'
    END DO
  END IF

! FIRST PUT NS, C & HSHS IN ARRAYS.  FLAG ANY ZERO NS.

  LF=.FALSE.
  DO I=1,N8
   NS(I)=REXP(2,111+(I-1)*4)
   C(I)=REXP(2,112+(I-1)*4)
   HSHS(I)=REXP(2,113+(I-1)*4)
   IF (NS(I) == 0) REXP(1,111+(I-1)*4)=1.
  END DO

! TOTAL CLOUD AMOUNT LESS THAN CLOUD AMOUNT IN ONE 8-GROUP

  DO I=1,N8
   IF(IN < NS(I))THEN
     REXP(1,111+(I-1)*4)=1.
     REXP(1,26)=1.
     IF (MSG) PRINT *,' 59)TOTAL CLOUD LESS THAN IN ONE 8-GROUP'
   END IF
  END DO

! NO LOW,MEDIUM OR HIGH CLOUD BUT CLOUD AMOUNT IN FIRST 8-GROUP

  IF ((ICL == 30 .OR. ICL == MISSIN) .AND. &
      (ICM == 20 .OR. ICM == MISSIN) .AND. &
      (ICH == 10 .OR. ICH == MISSIN)) THEN
    REXP(1,102)=1.
    REXP(1,104)=1.
    REXP(1,106)=1.
    REXP(1,111)=1.
    REXP(1,112)=1.
    REXP(1,113)=1.
    IF (MSG) PRINT *,' 60)NO CM, CL & CH, BUT 8-GROUP REPORTED'
  END IF
!*********
!   NH   *            NH IS THE AMOUNT OF ALL LOW CLOUD OR, IF THERE IS
!*********            NO LOW CLOUD, THEN THE AMOUNT OF ALL MEDIUM CLOUD
!                     - DECIDE WHICH FROM THE TYPE IN THE FIRST 8-GROUP

! LESS CLOUD IN NH THAN IN CORRESPONDING 8-GROUPS

DO_8GP1: &
  DO I=1,N8
    IF (C(1) > 5.) THEN
      IF (C(I) >= 6. .AND.C(I) <= 9. .AND. INH < NS(I)) THEN
        REXP(1,111+(I-1)*4)=1.
        REXP(1,108)=1.
        IF (MSG) PRINT *,' 61)MORE LOW CLOUD IN 8-GROUP THAN NH'
      END IF
    ELSE
      IF (C(I) >= 3. .AND.C(I) <= 5. .AND. INH < NS(I)) THEN
        REXP(1,111+(I-1)*4)=1.
        REXP(1,108)=1.
        IF (MSG) PRINT *,'62)MORE MEDIUM CLOUD IN 8-GROUP THAN NH'
      END IF
    END IF
  END DO DO_8GP1

! FIRST 8-GROUP OBSCURED BUT AMOUNT OF CLOUD REPORTED.

  IF (NS(1) == 9 .AND. INH /= MISSIN.AND.INH < 9) THEN
    REXP(1,111)=1.
    REXP(1,108)=1.
    IF (MSG) PRINT *,' 63)NH REPORTED BUT FIRST 8-GROUP OBSCURED'
  END IF

! LOW/MEDIUM CLOUD REPORTED BUT HEIGHT INCONSISTENT IN 1ST 8-GROUP.

  IF (INH > 0 .AND. HSHS(1) >= 6000.) THEN
    REXP(1,113)=1.
    REXP(1,108)=1.
 IF (MSG) PRINT *,' 64)LOW/MEDIUM CLOUD BUT FIRST 8-GP HT HIGHER'
  END IF
!****************
!  CL SECTION   *
!****************

! LOW CLOUD REPORTED BUT FIRST 8-GROUP NOT LOW CLOUD.

  IF (ICL >= 31 .AND. ICL <= 39 .AND. &
      C(1) >= 0. .AND. C(1) <= 5.) THEN
    REXP(1,112)=1.
    REXP(1,102)=1.
    IF (MSG) PRINT *,' 65)LOW CLOUD BUT WRONG TYPE IN FIRST 8-GROUP'
  END IF

! LOW CLOUD REPORTED BUT FIRST 8-GROUP CLOUD AMOUNT OBSCURED.

  IF (ICL >= 31 .AND. ICL <= 39 .AND. NS(1) == 9) THEN
    REXP(1,111)=1.
    REXP(1,102)=1.
    IF (MSG) PRINT *,' 66)LOW CLOUD BUT FIRST 8-GROUP OBSCURED'
  END IF

! LOW CLOUD REPORTED IN FIRST 8-GROUP BUT HEIGHT INCONSISTENT.

  IF (ICL >= 31 .AND. ICL <= 39 .AND. HSHS(1) > 2700.) THEN
    REXP(1,102)=1.
    REXP(1,113)=1.
    IF (MSG) PRINT *,' 67)LOW CLOUD BUT HEIGHT WRONG IN 1ST 8-GROUP'
  END IF

! CUMULONIMBUS IN MAIN CLOUD GROUP BUT NOT IN 8-GROUPS.

  IF (ICL == 33 .OR. ICL == 39) THEN
    DO I=1,N8
      IF (C(I) == 9.) GO TO 100
    END DO
    REXP(1,102)=1.
    DO I=1,N8
      REXP(1,112+(I-1)*4)=1.
    END DO
    IF (MSG) PRINT *,' 68)CB IN MAIN CLOUD GROUP BUT NO 8-GROUP'
  END IF

 100 CONTINUE

! CUMULONIMBUS IN 8-GROUP BUT NOT IN MAIN CLOUD GROUP.

  DO I=1,N8
    IF (C(I) == 9. .AND. ICL /= 33.AND.ICL /= 39)THEN
      REXP(1,112+(I-1)*4)=1.
      REXP(1,102)=1.
      IF (MSG) PRINT *,' 69)CB IN 8-GROUP BUT NOT IN MAIN CLOUD GROUP'
    END IF
  END DO
!****************
!  CM SECTION   *
!****************

! NO MEDIUM CLOUD IN MAIN GROUP BUT MEDIUM HEIGHT OR TYPE IN 8-GROUP

IFBLOCK10: &
  IF (ICM == MISSIN .OR. ICM == 20) THEN
DO_8GP2: &
    DO I=1,N8
      IF (HSHS(I) >= 2700. .AND. HSHS(I) <= 5100.) THEN
        REXP(1,113+(I-1)*4)=1.
        REXP(1,104)=1.
        IF (MSG) PRINT *,' 70)NO CM BUT MEDIUM CLOUD HT IN 8-GROUP'
      END IF

      IF (C(I) >= 3. .AND. C(I) <= 5.) THEN
        REXP(1,112+(I-1)*4)=1.
        REXP(1,104)=1.
        IF (MSG) PRINT *,' 71)NO CM BUT MEDIUM CLOUD TYPE IN 8-GROUP'
      END IF
    END DO DO_8GP2
  END IF IFBLOCK10

! NO MEDIUM/HIGH CLOUD IN MAIN GROUP BUT HIGH CLOUD HEIGHT IN 8-GROUP

  IF (ICM == 20 .AND. ICH == 10) THEN
    DO I=1,N8
      IF (HSHS(I) > 5100.) THEN
        REXP(1,113+(I-1)*4)=1.
        REXP(1,104)=1.
        REXP(1,106)=1.
        IF (MSG) PRINT *,' 72)NO CM OR CH BUT HIGH CLOUD IN 8-GROUP'
      END IF
    END DO
  END IF

! FIRST 8-GROUP INDICATES OBSCURED BUT MEDIUM CLOUD REPORTED.

  IF (ICM /= MISSIN .AND. NS(1) == 9) THEN
    REXP(1,111)=1.
    REXP(1,104)=1.
    IF (MSG) PRINT *,' 73)MEDIUM CLOUD BUT FIRST 8-GROUP OBSCURED'
  END IF

! FIRST 8-GROUP HAS HIGH CLOUD HEIGHT, BUT MEDIUM CLOUD IN MAIN GROUP

  IF (ICM >= 21 .AND. ICM <= 29 .AND. HSHS(1) >= 6300.) THEN
    REXP(1,113)=1.
    REXP(1,104)=1.
    IF (MSG) PRINT *,' 74)MEDIUM CLOUD BUT FIRST 8-GROUP HIGH'
  END IF

! ALTOCUMULUS REPORTED IN 8-GROUP BUT NOT IN MAIN GROUP.

  IF (ICM == 21 .OR. ICM == 22) THEN
    DO I=1,N8
      IF (C(I) == 3.) THEN
        REXP(1,112+(I-1)*4)=1.
        REXP(1,104)=1.
        IF (MSG) PRINT *,' 75)AC IN 8-GROUP BUT NOT MAIN CLOUD GROUP'
      END IF
    END DO
  END IF

! NIMBOSTRATUS OR ALTOSTRATUS IN 8-GROUP BUT NOT IN MAIN CLOUD GROUP

  IF (ICM >= 23 .AND. ICM <= 26) THEN
    DO I=1,N8
      IF (C(I) == 4. .OR. C(I) == 5.) THEN
        REXP(1,112+(I-1)*4)=1.
        REXP(1,104)=1.
        IF (MSG) PRINT *,' 76)NS OR AS IN 8-GROUP BUT NOT MAIN'
      END IF
    END DO
  END IF

! MEDIUM (BUT NO LOW) CLOUD PRESENT, BUT NO 8-GROUPS FOR MEDIUM CLOUD.

  IF (ICM >= 21.AND.ICM <= 29 .AND. ICL == 30) THEN
    DO I=1,N8
      IF (C(I) >= 3. .AND.C(I) <= 5.) GO TO 104
    END DO

    DO I=1,N8
      REXP(1,112+(I-1)*4)=1.
    END DO
    REXP(1,104)=1.
    REXP(1,106)=1.
    IF (MSG) PRINT *,' 77)CM & NO CL, BUT NO 8-GROUPS FOR MEDIUM'
  END IF

  104 CONTINUE

!****************
!  CH SECTION   *
!****************

! NO HIGH CLOUD IN MAIN GROUP BUT HIGH CLOUD TYPE OR HEIGHT IN 8-GROUP

IFBLOCK11: &
  IF (ICH == 10 .OR. ICH == MISSIN) THEN

DO_8GP3: &
    DO I=1,N8
      IF (C(I) >= 0. .AND. C(I) <= 2.) THEN
        REXP(1,112+(I-1)*4)=1.
        REXP(1,106)=1.
        IF (MSG) PRINT *,' 78)NO CH BUT HIGH CLOUD TYPE IN 8-GROUP'
      END IF

      IF (HSHS(I) >= 6300.) THEN
        REXP(1,113+(I-1)*4)=1.
        REXP(1,106)=1.
        IF (MSG) PRINT *,' 79)NO CH BUT HIGH CLOUD HEIGHT IN 8-GROUP'
      END IF
    END DO DO_8GP3

  END IF IFBLOCK11

! FIRST 8-GROUP INDICATES OBSCURED BUT HIGH CLOUD REPORTED.

  IF (ICH /= MISSIN .AND. NS(1) == 9) THEN
    REXP(1,111)=1.
    REXP(1,106)=1.
    IF (MSG) PRINT *,' 80)HIGH CLOUD BUT FIRST 8-GROUP OBSCURED'
  END IF

! CIRROSTRATUS REPORTED IN 8-GROUP BUT NOT IN MAIN GROUP.

  IF (ICH >= 11 .AND. ICH <= 14) THEN
    DO I=1,N8
      IF (C(I) == 2.) THEN
        REXP(1,112+(I-1)*4)=1.
        REXP(1,106)=1.
        IF (MSG) PRINT *,' 81)CS IN 8-GROUP BUT NOT MAIN CLOUD GROUP'
      END IF
    END DO
  END IF
!****************
!  INTERNAL     *
!****************

! HEIGHT IN 8-GROUP TOO HIGH FOR LOW CLOUD.

  DO I=1,N8
    IF (HSHS(I) > 2700. .AND. C(I) > 6.) THEN
      REXP(1,112+(I-1)*4)=1.
      REXP(1,113+(I-1)*4)=1.
      IF (MSG) PRINT *,' 82)8-GROUP HEIGHT WRONG FOR LOW CLOUD'
    END IF
  END DO

! CLOUD HEIGHT IN 8-GROUP FOR MEDIUM CLOUD BUT CLOUD TYPE NOT MEDIUM.

  DO I=1,N8
    IF ((C(I) < 3. .OR.C(I) > 5.) .AND. &
       HSHS(I) >= 2700. .AND. HSHS(I) <= 5100.) THEN
      REXP(1,112+(I-1)*4)=1.
      REXP(1,113+(I-1)*4)=1.
      IF (MSG) PRINT *,' 83)HEIGHT MEDIUM IN 8-GROUP BUT NOT TYPE'
    END IF
  END DO

! HEIGHT IN 8-GROUP IMPLIES HIGH CLOUD BUT TYPE NOT HIGH.

  DO I=1,N8
    IF (HSHS(I) >= 5400. .AND. C(I) > 2.) THEN
      REXP(1,112+(I-1)*4)=1.
      REXP(1,113+(I-1)*4)=1.
      IF (MSG) PRINT *,' 84)8-GROUP HEIGHT HIGH BUT TYPE NOT'
    END IF
  END DO

! INVALID 8-GROUP, TYPE MISSING BUT AMOUNT NOT MISSING.

  DO I=1,N8
    IF (C(I) == RMISS .AND. NS(I) > 0..AND.NS(I) < 9.) THEN
      REXP(1,111+(I-1)*4)=1.
      REXP(1,112+(I-1)*4)=1.
      REXP(1,113+(I-1)*4)=1.
      IF (MSG) PRINT *,' 85)8-GROUP AMOUNT BUT NO TYPE'
    END IF
  END DO

! INSUFFICIENT CLOUD AMOUNTS FOR SEQUENCE OF 8-GROUPS.

  IF (N8 == 4) THEN
    DO I=2,4
      IF (NS(I) < NMAND(I-1)) THEN
        REXP(1,111+(I-1)*4)=1.
        REXP(1,112+(I-1)*4)=1.
        REXP(1,113+(I-1)*4)=1.
        IF (MSG) PRINT *,' 86)NOT ENOUGH CLOUD FOR 8-GROUP SEQUENCE'
      END IF
    END DO
  END IF
END IF IF8GP

RETURN
END SUBROUTINE SYNQC
