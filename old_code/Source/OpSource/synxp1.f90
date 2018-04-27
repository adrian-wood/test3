SUBROUTINE SYNXP1(REPORT,POINT,NGRPS,SYNTAX,IWMOB, &
  WXMAN,TTT,TDTDTD,UUU,P0P0P0,PPPP,A3,HHH,ATEND, &
  PCHANG,RRR,TR,WW,W1,W2,NH,CL,CM,CH,HOUR,MINS,IX,IDATIM)

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
! ARGUMENTS     : (1) REPORT CHAR    COMPLETE REPORT              (I)
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
! $Workfile: synxp1.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 23/12/2010 10:48:08$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         23/12/2010 10:48:08    John Norton     Ported
!        version for MDBSTOR batches 6 & 7
!  1    MetDB_Refresh 1.0         23/12/2010 10:16:13    John Norton
!       Pre-ported version but with changed extension for synop routines.
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

! Use statements:
! <Interfaces>

USE ivalue_mod   !function

! <Data Modules>

USE metdb_com_mod, only : MISSIN, RMISS, TCONV

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: REPORT     !a01
INTEGER,          INTENT(INOUT) :: POINT      !a02
INTEGER,          INTENT(IN)    :: NGRPS      !a03
LOGICAL,          INTENT(INOUT) :: SYNTAX     !a04
INTEGER,          INTENT(IN)    :: IWMOB      !a05
LOGICAL,          INTENT(IN)    :: WXMAN      !a06
REAL,             INTENT(INOUT) :: TTT        !a07 Air temperature (TTT)
REAL,             INTENT(INOUT) :: TDTDTD     !a08 Dew point temperature (TdTdTd)
REAL,             INTENT(INOUT) :: UUU        !a09 Relative humidity (UUU)
REAL,             INTENT(INOUT) :: P0P0P0     !a10 Station level Pressure (P0P0P0P0)
REAL,             INTENT(INOUT) :: PPPP       !a11 MSL pressure (PPPP)
REAL,             INTENT(INOUT) :: A3         !a12 Standard isobaric surface (a3)
REAL,             INTENT(INOUT) :: HHH        !a13 Geopotential height (hhh)
REAL,             INTENT(INOUT) :: ATEND      !a14 Pressure tendency amount (a)
REAL,             INTENT(INOUT) :: PCHANG     !a15 3 hour Pressure change amount(ppp)
REAL,             INTENT(INOUT) :: RRR        !a16 Section 1 Precipitation amount (RRR)
REAL,             INTENT(INOUT) :: TR         !a17 Section 1 type of precipitation (tR)
REAL,             INTENT(INOUT) :: WW         !a18 Present Weather (ww)
REAL,             INTENT(INOUT) :: W1         !a19 Past Weather 1 (W1)
REAL,             INTENT(INOUT) :: W2         !a20 Past Weather 2 (W2)
REAL,             INTENT(INOUT) :: NH         !a21 Amount of low cloud (Nh)
REAL,             INTENT(INOUT) :: CL         !a22 Type Low Cloud (CL)
REAL,             INTENT(INOUT) :: CM         !a23 Type Medium Cloud (CM)
REAL,             INTENT(INOUT) :: CH         !a24 Type High Cloud (CH)
REAL,             INTENT(INOUT) :: HOUR       !a25 Hour (GG)
REAL,             INTENT(INOUT) :: MINS       !a26 Minutes (gg)
INTEGER,          INTENT(IN)    :: IX         !a27
INTEGER,          INTENT(IN)    :: IDATIM(5)  !a28

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  I
INTEGER          ::  I1
INTEGER          ::  J
INTEGER          ::  LASTID
LOGICAL          ::  THREEGP
LOGICAL          ::  WWMISS

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!**********************************************************************
!
! CHECK SEQUENCE OF GROUPS: SET SYNTAX FLAG & STOP IF NOT ASCENDING
!
!**********************************************************************
THREEGP=.FALSE.
LASTID=0
WWMISS = .TRUE.
!
DOLABEL1: &
DO J=1,NGRPS
I=IVALUE(REPORT(POINT:POINT))
IF(I <= LASTID)THEN
  SYNTAX=.TRUE.
  RETURN
END IF
LASTID=I
!**********************************************************************
!
! DRY BULB TEMPERATURE (FROM DEGREES AND TENTHS TO K)
!
!**********************************************************************
IFLABEL1: &
IF (I == 1) THEN
  I=IVALUE(REPORT(POINT+2:POINT+4))
  IF (I /= MISSIN) THEN
    IF (REPORT(POINT+1:POINT+1) == '1') THEN
      TTT=-I*0.1+TCONV
    ELSE IF (REPORT(POINT+1:POINT+1) == '0') THEN
      TTT=I*0.1+TCONV
    END IF
  END IF
!**********************************************************************
!
! DEW POINT TEMPERATURE (FROM DEGREES AND TENTHS TO K) OR
! RELATIVE HUMIDITY (IN PERCENT)
!
!**********************************************************************
ELSE IF (I == 2) THEN
  I=IVALUE(REPORT(POINT+2:POINT+4))
  IF (REPORT(POINT+1:POINT+1) == '9') THEN
    IF (I <= 100) UUU=I
  ELSE
    IF (I /= MISSIN) THEN
      IF (REPORT(POINT+1:POINT+1) == '1') THEN
        TDTDTD=-I*0.1+TCONV
      ELSE IF (REPORT(POINT+1:POINT+1) == '0') THEN
        TDTDTD=I*0.1+TCONV
      END IF
    END IF
  END IF
!**********************************************************************
!
! STATION LEVEL PRESSURE (CONVERT FROM MB AND TENTHS TO PA).
! SYNTAX WILL BE FLAGGED IF 4-GROUP WITH HEIGHT BUT NO 3-GROUP
!
!**********************************************************************
ELSE IF (I == 3) THEN
  I=IVALUE(REPORT(POINT+1:POINT+4))
  IF (I /= MISSIN) THEN
    IF (REPORT(POINT+1:POINT+1) == '0') THEN
      P0P0P0=(I+10000.)*10.
    ELSE
      P0P0P0=I*10.
    END IF
  END IF
  THREEGP=.TRUE.
!**********************************************************************
!
! GEOPOTENTIAL LEVEL AND ITS HEIGHT, OR MEAN SEA-LEVEL PRESSURE.
!
!**********************************************************************
ELSE IF (I == 4) THEN
IFLABEL2: &
  IF (REPORT(POINT+1:POINT+1) == '0' .OR. &
      REPORT(POINT+1:POINT+1) == '9') THEN
    I=IVALUE(REPORT(POINT+1:POINT+4))
    IF (I /= MISSIN) THEN
      IF (I < 9000) THEN
        PPPP=(I+10000.)*10.
      ELSE
        PPPP=I*10.
      END IF
    END IF
  ELSE
!
! IF SECOND FIGURE IS NOT 0 OR 9, IT''S A3 AS BELOW & LAST 3 FIGURES
! GIVE HEIGHT (WITHOUT THOUSANDS FIGURE) OF THAT STANDARD LEVEL.
!
    I=IVALUE(REPORT(POINT+1:POINT+1))
    HHH=IVALUE(REPORT(POINT+2:POINT+4))
IFLABEL3: &
    IF (I /= MISSIN .AND. HHH /= RMISS) THEN
IFLABEL4: &
      IF (I == 1) THEN              ! NO THOUSANDS TO ADD
        A3=100000.
      ELSE IF (I == 2) THEN         ! NO THOUSANDS TO ADD
        A3=92500.
      ELSE IF (I == 8) THEN
        A3=85000.
        HHH=HHH+1000.
      ELSE IF (I == 7) THEN
        A3=70000.
        IF (HHH < 400) THEN       ! TAKE RANGE AS 2400-3399M
          HHH=HHH+3000.
        ELSE
          HHH=HHH+2000.
        END IF
      ELSE IF (I == 5) THEN
        A3=50000.
        HHH=HHH+5000.
      END IF IFLABEL4
    END IF IFLABEL3
    IF (.NOT.THREEGP) SYNTAX=.TRUE.
  END IF IFLABEL2
!**********************************************************************
!
! PRESSURE TENDENCY AND AMOUNT OF CHANGE.
! NO CONVERSION NEEDED BETWEEN SYNOP CODE 0200 AND BUFR CODE 010063.
! CONVERT CHANGE FROM TENTHS OF MB TO PA.
! SET A CHANGE OF MORE THAN 50MB TO MISSING - TOO BIG FOR BUFR RANGE.
!
!**********************************************************************
ELSE IF (I == 5) THEN
  I=IVALUE(REPORT(POINT+1:POINT+1))
  ATEND=I
  I1=IVALUE(REPORT(POINT+2:POINT+4))
  IF (I /= MISSIN .AND. I1 /= MISSIN .AND. I1 <= 500) THEN
    IF (I >= 5) THEN
      PCHANG=-I1*10.
    ELSE
      PCHANG=I1*10.
    END IF
  END IF
!**********************************************************************
!
! RAINFALL AMOUNT AND PERIOD.
! CONVERT AMOUNT FROM SYNOP CODE TABLE 3590 TO KG/M**2 (NUMERICALLY
! SAME AS MM) & PERIOD FROM SYNOP CODE TABLE 4019 TO HOURS.
!
!**********************************************************************
ELSE IF (I == 6) THEN
  I=IVALUE(REPORT(POINT+1:POINT+3))
  IF(I >= 0.AND.I <= 989) THEN           !2.2
    RRR=I
  ELSE IF (I > 990 .AND. I <= 999) THEN
    RRR=(I-990)*0.1
  ELSE IF (I == 990) THEN
    RRR=-1    !  A TRACE
  END IF
!
!1.8 For Indian and Sri-Lanka a Tr value of / means that it is a
!1.8 Rainfall period since 0300Z
IFLABEL5: &
  IF ((REPORT(POINT+4:POINT+4) == '/').AND.(IWMOB >= 42) &
    .AND.(IWMOB <= 43)) THEN
    TR = -(MOD(IDATIM(4)+20,24)+1)
  ELSE
    I=IVALUE(REPORT(POINT+4:POINT+4))
    IF (I >= 1 .AND. I <= 4) THEN
      TR=-I*6
    ELSE IF (I >= 5 .AND. I <= 7) THEN
      TR=-(I-4)
    ELSE IF (I == 8) THEN
      TR=-9.
    ELSE IF (I == 9) THEN
      TR=-15.
    ELSE
      TR=I
    END IF
  END IF IFLABEL5
!
! SOME BLOCKS IN THE TWENTIES & THIRTIES REPORT 12-HOUR RAINFALL WITH
! TR=0 BECAUSE THE 12-HOUR PERIOD DEPENDS ON LOCAL TIME RATHER THAN
! ENDING AT THE REPORT TIME.
!
  IF (I == 0 .AND.IWMOB >= 20.AND.IWMOB <= 39) TR=-12.
!**********************************************************************
!
! PRESENT AND PAST WEATHER GROUP 7WWW1W1 FROM MANNED STNS
! OR 7WAWAWA1WA2 FROM AUTOMATIC STNS.
! WW SYNOP CODE 4677 = BUFR CODE 20003
! WAWA SYNOP CODE 4680 (0-99)= BUFR CODE 20003 (100-199)
! W1,W2 SYNOP CODE 4561 = BUFR CODE 20004/5
! WA1,WA2 SYNOP CODE 4531 (0-9) = BUFR CODE 20004/5 (10-19)
!
!**********************************************************************
ELSE IF (I == 7) THEN
  WW=IVALUE(REPORT(POINT+1:POINT+2)) ! PRESENT WX
  IF (.NOT.WXMAN .AND. WW > RMISS) WW=WW+100.
  W1=IVALUE(REPORT(POINT+3:POINT+3)) ! PAST WX 1
  IF (.NOT.WXMAN .AND. W1 > RMISS) W1=W1+10.
  W2=IVALUE(REPORT(POINT+4:POINT+4)) ! PAST WX 2
  IF (.NOT.WXMAN .AND. W2 > RMISS) W2=W2+10.
  WWMISS = .FALSE.
!**********************************************************************
!
! MAIN CLOUD GROUP
! TOTAL CLOUD SYNOP CODE 2700 TO BUFR TBL 020011
! CL SYNOP CODE 0513 (+30) = BUFR CODE TBL 020012
! CM SYNOP CODE 0515 (+20) = BUFR CODE TBL 020012
! CH SYNOP CODE 0509 (+10) = BUFR CODE TBL 020012
!
!**********************************************************************
ELSE IF (I == 8) THEN
  NH=IVALUE(REPORT(POINT+1:POINT+1))
  I=IVALUE(REPORT(POINT+2:POINT+2))
  IF (I /= MISSIN) CL=I+30.
  I=IVALUE(REPORT(POINT+3:POINT+3))
  IF (I /= MISSIN) CM=I+20.
  I=IVALUE(REPORT(POINT+4:POINT+4))
  IF (I /= MISSIN) CH=I+10.
!**********************************************************************
!
! SUPPLEMENTARY TIME GROUP:
! IGNORE IT IF TIME WITHIN 10 MINUTES OF HOUR
!
! >>> THE CHANGE TO IGNORE A TIME GROUP WITHIN 10 MINUTES OF THE HOUR
! >>>  WAS MADE IN MAY 97 TO COPE WITH BLOCK 72 OBS WHICH MISUSE THE
! >>>   9-GROUP & WERE STORED AS OBS FOR THE PREVIOUS HOUR
!
! >>> The change above is removed so that the processing of the time
! >>> can be done in SYNEXP.
!
!**********************************************************************
ELSE IF(I == 9) THEN
  I=IVALUE(REPORT(POINT+3:POINT+4))
  IF (I /= MISSIN .AND. I < 60 .AND. I >= 00) THEN
    MINS=I
    I=IVALUE(REPORT(POINT+1:POINT+2))
    IF (I /= MISSIN .AND. I <= 23) HOUR=I
  END IF
END IF IFLABEL1
!**********************************************************************
!
!   INCREMENT POINTER TO NEXT GROUP
!
!**********************************************************************
POINT=POINT+6
END DO DOLABEL1
IF(WWMISS)THEN
  IF    (IX == 2.OR.IX == 5)THEN
     WW = 508
  ELSE IF(IX == 3.OR.IX == 6)THEN
     WW = 509
  END IF
END IF
RETURN
END SUBROUTINE SYNXP1
