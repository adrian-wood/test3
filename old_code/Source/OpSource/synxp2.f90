SUBROUTINE SYNXP2(REPORT,POINT,NGRPS,SYNTAX, &
        LSEA,ISWELL,TWTYPE,TWTWTW, &
        DS,VS,TBTYPE,TBTBTB,WVMS,WVHT,WVPER,SWELWV, &
        ISICE,ESES,RS,CI,SI,BI,DI,ZI,REPLEN)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNXP2
!
! PURPOSE       : TO EXPAND SECTION 2 OF A SYNOP/SHIP REPORT INTO
!                 REAL VARIABLES.
!
! DESCRIPTION   :  WMO CODE FM 12-IX (SYNOP COASTAL STNS)
!                  WMO CODE FM 13-IX (SHIP)
!
! CALLED BY     : SYNEXP
!
! CALLS         : IVALUE (FUNCTION)
!
! ARGUMENTS     : (1)REPORT CHAR      FULL REPORT                 (I)
!                 (2)POINT  INTEGER   START OF 222.. GROUP        (I)
!                 (3)NGRPS  INTEGER   NUMBER OF GROUPS IN SECTION (I)
!                 (4)SYNTAX LOGICAL   TRUE IF SYNTAX ERROR FOUND  (O)
!                 (5)LSEA   LOGICAL   TRUE IF USEFUL DATA FOUND   (O)
!                 (6)-(24)  REAL      VARIABLES (SEE SHP/SYNINI)  (O)
!                 (25)      INTEGER   LENGTH OF REPORT            (I)
!
! REVISION INFO :
!
! $Workfile: synxp2.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 23/12/2010 14:12:00$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         23/12/2010 14:12:00    John Norton     After
!       rework for MDBSTOR port.
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

USE metdb_com_mod, only : MISSIN, TCONV

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: REPORT     !a01
INTEGER,          INTENT(INOUT) :: POINT      !a02
INTEGER,          INTENT(IN)    :: NGRPS      !a03
LOGICAL,          INTENT(INOUT) :: SYNTAX     !a04
LOGICAL,          INTENT(OUT)   :: LSEA       !a05
INTEGER,          INTENT(OUT)   :: ISWELL     !a06
REAL,             INTENT(INOUT) :: TWTWTW     !a07 Sea temperature(TwTwTw)
REAL,             INTENT(INOUT) :: TWTYPE     !a08 SST measurement method (ss)
REAL,             INTENT(INOUT) :: DS         !a09 Direction of movement (Ds)
REAL,             INTENT(INOUT) :: VS         !a10 Speed of movement(vs)
REAL,             INTENT(INOUT) :: TBTYPE     !a11 Wet-bulb temperature measurement method (sw)
REAL,             INTENT(INOUT) :: TBTBTB     !a12 Wet-bulb temperature(TbTbTb)
REAL,             INTENT(INOUT) :: WVMS       !a13 Wave measurement method
REAL,             INTENT(INOUT) :: WVHT       !a14 Wave height (HwaHwa)
REAL,             INTENT(INOUT) :: WVPER      !a15 Wave period (PwaPwa)
REAL,             INTENT(INOUT) :: SWELWV(6)  !a16 Swell Waves (2*dwdwPwPwHwHw)
REAL,             INTENT(INOUT) :: ISICE      !a17 Cause of ice accretion on ship (Is)
REAL,             INTENT(INOUT) :: ESES       !a18 Ice thickness on ship(EsEs)
REAL,             INTENT(INOUT) :: RS         !a19 Rate of ice accretion on ship(Rs)
REAL,             INTENT(INOUT) :: CI         !a20 Sea ice concentration(ci)
REAL,             INTENT(INOUT) :: SI         !a21 Sea ice development(Si)
REAL,             INTENT(INOUT) :: BI         !a22 Amount & type of sea ice(bi)
REAL,             INTENT(INOUT) :: DI         !a23 Bearing of nearest sea ice(Di)
REAL,             INTENT(INOUT) :: ZI         !a24 Sea ice situation (zi)
INTEGER,          INTENT(IN)    :: REPLEN     !a25

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  I
INTEGER          ::  IG
INTEGER          ::  IS
INTEGER          ::  LASTID

LOGICAL          ::  WAVE

REAL             ::  SPCONV(0:9)

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DATA SPCONV/0.,1.,4.,7.,9.,12.,14.,17.,19.,21./

!
ISWELL=0
WAVE=.FALSE.
IG=1
LASTID=0
LSEA=.FALSE.
!
! CHECK THAT GROUPS ARE IN ASCENDING ORDER AFTER 222 GROUP ITSELF
!
 10    CONTINUE
I=IVALUE(REPORT(POINT:POINT))
IF (I /= MISSIN .AND. IG > 1) THEN
  IF (I < LASTID) THEN
    SYNTAX=.TRUE.
    RETURN
  END IF
  LASTID=I
END IF
!
! 222DSVS DIRECTION AND SPEED OF MOVEMENT
! CONVERT DS FROM COMPASS POINTS (TBL 0700) TO DEGREES
! CONVERT VS FROM MULTIPLES OF 5KTS TO M/S (USING CONVERSIONS PROPOSED
! BY WMO CBS - WG DATA MAN. 2ND SESSION FEB 1994)
!
IFLABEL1: &
IF (REPORT(POINT:POINT+2) == '222') THEN
  I=IVALUE(REPORT(POINT+3:POINT+3))
  IF (I /= MISSIN .AND. I < 9) DS=I*45.
  I=IVALUE(REPORT(POINT+4:POINT+4))
  IF (I /= MISSIN) VS=SPCONV(I)
  LSEA=.TRUE.
!
! SEA TEMPERATURE GROUP, INDICATOR IS SIGN AND MEASUREMENT METHOD
! CONVERT TEMP IN TENTHS TO K
! CONVERT MEASURMENT METHOD TO BUFR TBL 002038
!
ELSE IF (REPORT(POINT:POINT) == '0') THEN
  I=IVALUE(REPORT(POINT+2:POINT+4))
IFLABEL2: &
  IF (I /= MISSIN) THEN
    IS=IVALUE(REPORT(POINT+1:POINT+1))
IFLABEL3: &
    IF (IS /= MISSIN) THEN
      IF (MOD(IS,2) == 1) THEN
        TWTWTW=-I*0.1+TCONV
      ELSE
        TWTWTW=I*0.1+TCONV
      END IF
      LSEA=.TRUE.
      IF (IS == 0.OR.IS == 1) THEN
        TWTYPE=0.
      ELSE IF (IS == 2.OR.IS == 3) THEN
        TWTYPE=1.
      ELSE IF (IS == 4.OR.IS == 5) THEN
        TWTYPE=2.
      END IF
    END IF IFLABEL3
  END IF IFLABEL2
!
! INSTRUMENTAL WAVE DATA HEIGHT IN UNITS OF 0.5 M.
! PERIOD OF 99 (CONFUSED SEA) SET TO MISSING.
!
ELSE IF (REPORT(POINT:POINT) == '1') THEN
  WVMS=0.             ! INSTRUMENTAL MEASUREMENT GROUP
  I=IVALUE(REPORT(POINT+1:POINT+2))
  IF (I /= MISSIN .AND. I /= 99) WVPER=I
  I=IVALUE(REPORT(POINT+3:POINT+4))
  IF (I /= MISSIN) WVHT=I*0.5   ! METRES
  LSEA=.TRUE.
  WAVE=.TRUE.         ! MAY BE MORE ACCURATE DATA IN '70' GROUP
!
! WIND WAVE GROUP; AS ABOVE BUT ESTIMATED.
!
ELSE IF (REPORT(POINT:POINT) == '2') THEN
  IF (.NOT.WAVE) THEN
    WVMS=1.  !  ESTIMATED WAVE GROUP
    I=IVALUE(REPORT(POINT+1:POINT+2))
    IF (I /= MISSIN .AND. I /= 99) WVPER=I
    I=IVALUE(REPORT(POINT+3:POINT+4))
    IF (I /= MISSIN) WVHT=I*0.5   ! METRES
    LSEA=.TRUE.
  END IF
!
!  SWELL WAVE GROUP, TWO POSSIBLE SWELLS. THIS GROUP CONTAINS BOTH
!  DIRECTIONS; GROUP '4' HAS PERIOD AND HT OF FIRST; GROUP '5' THEN
!  PERIOD AND HEIGHT OF THE SECOND
!
ELSE IF (REPORT(POINT:POINT) == '3') THEN
  I=IVALUE(REPORT(POINT+1:POINT+2))
  IF (I /= MISSIN) THEN
    ISWELL=ISWELL+1
    IF (I == 99) THEN
      SWELWV(1)=990.  ! VARIABLE
    ELSE
      SWELWV(1)=I*10. !  DIRECTION IN DEGREES.
    END IF
    LSEA=.TRUE.
  END IF
  I=IVALUE(REPORT(POINT+3:POINT+4))
  IF (I /= MISSIN) THEN
    ISWELL=ISWELL+1
    IF (I == 99) THEN
      SWELWV(4)=990.  ! VARIABLE
    ELSE
      SWELWV(4)=I*10. ! DIRECTION IN DEGREES.
    END IF
    LSEA=.TRUE.
  END IF
!
!  SWELL WAVE PERIOD AND HEIGHT FOR FIRST SWELL GROUP
!
ELSE IF (REPORT(POINT:POINT) == '4') THEN
  I=IVALUE(REPORT(POINT+1:POINT+2))
  IF (I /= 99) SWELWV(2)=I         ! PERIOD IN SECONDS
  I=IVALUE(REPORT(POINT+3:POINT+4))
  IF (I /= MISSIN) SWELWV(3)=I*0.5 ! WAVE HEIGHT, M.
  LSEA=.TRUE.
!
!  SWELL WAVE GROUP, SECOND OF TWO GROUPS ALLOWED.
!
ELSE IF (REPORT(POINT:POINT) == '5') THEN
  I=IVALUE(REPORT(POINT+1:POINT+2))
  IF (I /= 99) SWELWV(5)=I         ! PERIOD IN SECONDS
  I=IVALUE(REPORT(POINT+3:POINT+4))
  IF (I /= MISSIN) SWELWV(6)=I*0.5 ! WAVE HEIGHT, M.
  LSEA=.TRUE.
!
! ICE ACCRETION, THICKNESS AND RATE.
! CONVERT SHIP CODE TBL 1751 TO BUFR FLAG TBL 20033
! ICE THICKNESS FROM CM TO M
! RATE OF ACCRETION CODE TBL 3551 = BUFR CODE 20032
!
ELSE IF (REPORT(POINT:POINT) == '6') THEN
  I=IVALUE(REPORT(POINT+1:POINT+1))
  IF (I /= MISSIN) THEN
    IF (I == 1) ISICE=8
    IF (I == 2) ISICE=4
    IF (I == 3) ISICE=12
    IF (I == 4) ISICE=2
    IF (I == 5) ISICE=10
  END IF
  I=IVALUE(REPORT(POINT+2:POINT+3))
  IF (I /= MISSIN) ESES=I*0.01
  RS=IVALUE(REPORT(POINT+4:POINT+4))
  LSEA=.TRUE.
!
! MORE ACCURATE MEASURED WAVE HEIGHTS. OVERWRITE THOSE FROM 1-GROUP
!
ELSE IF (REPORT(POINT:POINT+1) == '70') THEN
  IF (WAVE) THEN
    I=IVALUE(REPORT(POINT+2:POINT+4))
    IF (I /= MISSIN) WVHT=I*0.1
  END IF
!
! WET BULB TEMPERATURE AND MEASUREMENT TYPE
! SHIP CODE TBL 3855 TO BUFR TBL 002039
!
ELSE IF (REPORT(POINT:POINT) == '8') THEN
  I=IVALUE(REPORT(POINT+2:POINT+4))
IFLABEL4: &
  IF (I /= MISSIN) THEN
    IS=IVALUE(REPORT(POINT+1:POINT+1))
IFLABEL5: &
    IF (IS /= MISSIN) THEN
      IF (IS == 0.OR.IS == 5) THEN
        TBTBTB=I*0.1+TCONV
      ELSE
        TBTBTB=-I*0.1+TCONV
      END IF
      LSEA=.TRUE.
      IF (IS == 0.OR.IS == 1) THEN
        TBTYPE=0.
      ELSE IF (IS == 2) THEN
        TBTYPE=1.
      ELSE IF (IS == 5.OR.IS == 6) THEN
        TBTYPE=2.
      ELSE IF (IS == 7) THEN
        TBTYPE=3.
      END IF
    END IF IFLABEL5
  END IF IFLABEL4
!
! ICE DATA - 5 DIGIT GROUP FOLLOWING THE WORD 'ICE '
!
ELSE IF (POINT+3 <=  REPLEN) THEN
IFLABEL6: &
  IF (REPORT(POINT:POINT+3) == 'ICE ') THEN
    POINT=POINT+4
    IG=IG+1
    IF (IG > NGRPS) GOTO 999
!
! ASSUME THE NEXT GROUP IS ICE INFO - CISIBIDIZI
! (CONCENTRATION, DEVELOPMENT, AMOUNT & TYPE, BEARING, SITUATION/TREND)
! CI IS CODE TABLE 0639, 020034 IN BUFR, SI IS TABLE 3739/020037,
! BI IS TABLE 0439/020035, ZI IS 5239/020036.
!
    CI=IVALUE(REPORT(POINT:POINT))
    SI=IVALUE(REPORT(POINT+1:POINT+1))
    BI=IVALUE(REPORT(POINT+2:POINT+2))
    I=IVALUE(REPORT(POINT+3:POINT+3))
    IF (I >= 1.AND.I <= 8) DI=I*45.
    ZI=IVALUE(REPORT(POINT+4:POINT+4))
    LSEA=.TRUE.
  END IF IFLABEL6
END IF IFLABEL1
!
! INCREMENT GROUP POINTERS (ALL GROUPS HAVE 5 FIGURES)
!
POINT=POINT+6
IG=IG+1
IF (IG <= NGRPS) GOTO 10
 999   CONTINUE
RETURN
END SUBROUTINE SYNXP2
