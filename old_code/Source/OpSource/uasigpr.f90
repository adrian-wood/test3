SUBROUTINE UASIGPR(STRING,PTR,ARRAY,NUM_LEV,PART,WINDS,KNOTS, &
                   BADSEQ,SIG_BASE,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : UASIGPR (Upper Air SIGnificant levels with Pressure)
!
! PURPOSE       : EXPAND SIGNIFICANT LEVELS WITH PRESSURE AS VERTICAL
!                 COORDINATE IN TEMP/PILOT PART B OR D
!
! DATA TYPE(S)  : UPPER AIR TEMP B/D & PILOT B/D WITH PRESSURE SENSOR
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION)
!
! ARGUMENTS     : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                 (2) INPUT POINTER TO FIRST SIGNIFICANT LEVEL
!                      (POINTS PAST END OF SECTION ON RETURN)
!                 (3) OUTPUT ARRAY, WITH HEADER DATA ALREADY IN IT
!                 (4) OUTPUT SUBSCRIPT (NOT INCREMENTED HERE)
!                 (5) PART (B OR D OR MISSING IF 51515 SECTION)
!                 (6) TEMPERATURES OR WINDS (TRUE IF WINDS)
!                 (7) FLAG SET ON IF WINDS IN KNOTS  (--> *.5148)
!                 (8) FLAG SET ON IF PRESSURES NOT DECREASING
!                 (9) NUMBERS OF TEMPERATURES & WINDS IN REPORT
!
! REVISION INFO :
!
!
! $Workfile: uasigpr.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 11/04/2011 14:18:12$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         11/04/2011 14:18:12    Brian Barwell
!       Correct error accidentally introduced in version 3.
!  3    MetDB_Refresh 1.2         29/03/2011 14:44:54    Brian Barwell   Check
!       number of levels to prevent array overflow.
!  2    MetDB_Refresh 1.1         26/01/2011 14:22:21    Rosemary Lavery
!       updates post review
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE IVALUE_mod
USE METDB_COM_mod, ONLY: MISSIN, RMISS, TCONV, KTS2MPS

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)  :: STRING    ! (a1) report
INTEGER, INTENT(INOUT)         :: PTR       ! (a2) pointer within report
REAL, INTENT(OUT)              :: ARRAY(:)  ! (a3) expanded elements array
INTEGER, INTENT(INOUT)         :: NUM_LEV   ! (a4) number of levels
CHARACTER (LEN=1), INTENT(IN)  :: PART      ! (a5) report part
LOGICAL, INTENT(IN)            :: WINDS     ! (a6) indicates wind or temp decode
LOGICAL, INTENT(IN)            :: KNOTS     ! (a7) indicates speed in knots
LOGICAL, INTENT(OUT)           :: BADSEQ    ! (a8) invalid pressure trend
INTEGER, INTENT(IN)            :: SIG_BASE  ! (a9) array displacement base sig report
REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)  ! (a10) qcbit array

! Local Variables

INTEGER  :: DD              !wind direction
INTEGER  :: FF              !wind speed
INTEGER  :: HEIGHT
INTEGER  :: L
INTEGER  :: LASTP           !last pressure
INTEGER  :: LENG
INTEGER  :: MAXLEV          !maximum no. of levels
INTEGER  :: P               !pressure
INTEGER  :: T               !air temperature
INTEGER  :: TD              !dew point temperature

CHARACTER (LEN=2)  :: IND(10)     !group indicator

SAVE

DATA IND/'00','11','22','33','44','55','66','77','88','99'/

! ---------------------------------------------------------------------

P=MISSIN
T=MISSIN
TD=MISSIN
DD=MISSIN
FF=MISSIN
LASTP=MISSIN
BADSEQ=.FALSE.
LENG=LEN(STRING)
MAXLEV = (993-SIG_BASE)/7  ! Max. levels for ARRAY and QCBIT_ARRAY size 999

! ---------------------------------------------------------------------
! the data consists of 2 groups per level, the first having a repeated
! figure as a sequence number, then a 3-figure pressure (mb if part b,
! tenths if part d) with any thousands figure omitted; the second is
! either a temperature or wind group as for standard levels.
! ---------------------------------------------------------------------

IF (PART == 'B') L = 1                  ! '00' FOR SURFACE
IF (PART == ' ') L = 2                  ! 11, 22, 33 IF 51515
IF (PART == 'D') L = 2                  ! NO SURFACE IF PART D

! ---------------------------------------------------------------------
! convert the pressure (mb in part b, tenths in part d), separate dd
! & ff, construct td from t and the dewpoint depression.
! ---------------------------------------------------------------------

 1 CONTINUE

IF_Block1: &
IF (STRING(PTR:PTR+1) == IND(L)) THEN         ! IF NOT OUT OF STEP,
  IF (PART == ' ' .AND. L == 2) THEN
    HEIGHT=900.0
  ELSE
    HEIGHT=MISSIN
  END IF

  IF ((PTR+4) <= LENG) THEN
    P=IVALUE(STRING(PTR+2:PTR+4))             ! CONVERT PRESSURE
  ELSE
    P=MISSIN
  END IF

IF_Block2: &
  IF (P /= MISSIN) THEN
    IF (PART == 'B' .AND. P < 100) P=P+1000   ! P CAN BE >1000
    IF (PART == 'B') P=P*100                  ! MB TO PASCALS
    IF (PART == ' ') P=P*100                  ! MB TO PASCALS (51515)
    IF (PART == 'D') P=P*10                   ! MB IN B, TENTHS IN D
IF_Block3: &
    IF (WINDS) THEN
      IF ((PTR+7) <= LENG) THEN
        DD=IVALUE(STRING(PTR+6:PTR+7))
      ELSE
        DD=MISSIN
      END IF
      IF ((PTR+10) <= LENG) THEN
        FF=IVALUE(STRING(PTR+8:PTR+10))
      ELSE
        FF=MISSIN
      END IF
      IF (DD /= MISSIN) THEN
        DD=DD*10
        IF (FF >= 500) THEN
          FF=FF-500
          DD=DD+5
        END IF
      END IF
    ELSE
      IF ((PTR+8) <= LENG) THEN
        T=IVALUE(STRING(PTR+6:PTR+8))     ! TEMP IN 10THS
      ELSE
        T=MISSIN
      END IF
      IF (T /= MISSIN) THEN
        IF (MOD(T,2) == 1) T=-T           ! <0 IF TENTHS FIGURE ODD
        IF ((PTR+10) <= LENG) THEN
          TD=IVALUE(STRING(PTR+9:PTR+10)) ! DEW PT DEPRESSION
        ELSE
          TD=MISSIN
        END IF
        IF (TD > 50) TD=(TD-50)*10        ! IN WHOLE DEGREES IF >50
        IF (TD /= MISSIN) TD=T-TD         ! DEPRESSION TO DEW POINT
      END IF
    END IF IF_Block3

! ---------------------------------------------------------------------
! a slashed level (missing data) means the wind or temperature was
! unknown between the pressures above & below.  set the pressure to
! just above the previous level & leave the temperature or wind
! missing to stop interpolation in that layer.
! ---------------------------------------------------------------------

  ELSE IF (LASTP /= MISSIN) THEN
    P=LASTP-1
    IF (WINDS) THEN
      DD=MISSIN
      FF=MISSIN
    ELSE
      T=MISSIN
      TD=MISSIN
    END IF
  END IF IF_Block2

  PTR=PTR+12                             ! POINT PAST TWO GROUPS
  L=L+1                                  ! NEXT INDICATOR
  IF (L > 10) L=2                        ! '11' FOLLOWS '99'

  IF (LASTP /= MISSIN .AND. P > LASTP) BADSEQ=.TRUE.
  LASTP=P                                ! IN CASE NEXT P MISSING

! ---------------------------------------------------------------------
! deal with pressure wind direction and wind speed
! ---------------------------------------------------------------------

  ARRAY(SIG_BASE+(7*NUM_LEV)+1)=P              !set pressure
  QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+1)=0.

IF_Block4: &
  IF (WINDS) THEN                              !sig level winds
    IF (DD /= MISSIN) THEN
      IF (DD > 360) THEN                       !check valid direction
        DD=MISSIN
        ARRAY(SIG_BASE+(7*NUM_LEV)+5)=DD
        QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=1. !suspect direction
      ELSE
        ARRAY(SIG_BASE+(7*NUM_LEV)+5)=DD
        QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=0. !OKAY direction
      END IF
    ELSE
      ARRAY(SIG_BASE+(7*NUM_LEV)+5)=RMISS      !direction missing
      QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=0.   !direction okay
    END IF

    IF (FF /= MISSIN) THEN
      IF (DD /= MISSIN) THEN
        IF (KNOTS) THEN                          !speed in knots
          ARRAY(SIG_BASE+(7*NUM_LEV)+6)=FF*KTS2MPS !and convert
        ELSE
          ARRAY(SIG_BASE+(7*NUM_LEV)+6)=FF       !speed in m/s
        END IF
      END IF
    ELSE
      ARRAY(SIG_BASE+(7*NUM_LEV)+6)=RMISS      !speed missing
    END IF

! ---------------------------------------------------------------------
! set qcbit for wind speed
! ---------------------------------------------------------------------

    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+6)=0.     !speed okay

! ---------------------------------------------------------------------
! sig level winds,so height,temperature and dew point must be set to
! missing.
! ---------------------------------------------------------------------

    ARRAY(SIG_BASE+(7*NUM_LEV)+2)=HEIGHT       !height missing
    ARRAY(SIG_BASE+(7*NUM_LEV)+3)=RMISS        !temp. missing
    ARRAY(SIG_BASE+(7*NUM_LEV)+4)=RMISS        !dew point missing
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+2)=0.     !height okay
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+3)=0.     !temp okay
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+4)=0.     !dew point okay

  ELSE                                         !sig temps
    IF (T /= MISSIN) THEN                      !air temperature
      ARRAY(SIG_BASE+(7*NUM_LEV)+3)=T*0.1 + TCONV
    ELSE                                       !temp. missing
      ARRAY(SIG_BASE+(7*NUM_LEV)+3)=RMISS
    END IF

    IF (TD /= MISSIN) THEN                     !dew point
      ARRAY(SIG_BASE+(7*NUM_LEV)+4)=TD*0.1 + TCONV
    ELSE
      ARRAY(SIG_BASE+(7*NUM_LEV)+4)=RMISS      !dew point missing
    END IF

    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+3)=0.     !temp. okay
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+4)=0.     !dew point okay

! ---------------------------------------------------------------------
! sig level temps so no height wind direction or wind speed. These must
! be set to missing
! ---------------------------------------------------------------------

    ARRAY(SIG_BASE+(7*NUM_LEV)+2)=RMISS        !height missing
    ARRAY(SIG_BASE+(7*NUM_LEV)+5)=RMISS        !direction missing
    ARRAY(SIG_BASE+(7*NUM_LEV)+6)=RMISS        !speed missing
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+2)=0.     !height okay
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=0.     !direction okay
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+6)=0.     !speed okay
  END IF IF_Block4

  IF (WINDS) THEN
    ARRAY(SIG_BASE+(7*NUM_LEV))=2              !sig level WINDS
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV))=0.
  ELSE
    ARRAY(SIG_BASE+(7*NUM_LEV))=4              !sig level temps
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV))=0.
  END IF

  IF (P /= MISSIN) NUM_LEV=NUM_LEV+1           !Next level

  IF (PTR < LENG .AND. NUM_LEV <= MAXLEV) GO TO 1

END IF IF_Block1

RETURN
END SUBROUTINE UASIGPR
