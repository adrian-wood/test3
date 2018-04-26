SUBROUTINE UASIGHT(STRING,PTR,ARRAY,NUM_LEV,PART,STNHT,KNOTS,  &
                   SIG_BASE,QCBIT_ARRAY,LENG)

!-----------------------------------------------------------------------
!
! PROGRAM       : UASIGHT  (Upper Air SIGnificant winds with HeighT)
!
! PURPOSE       : EXPAND SIGNIFICANT WINDS WITH HEIGHT AS COORDINATE
!                 IN PILOT B/D
!
! DATA TYPE(S)  : UPPER AIR PILOT PART B/D
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION)
!
! ARGUMENTS     : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                 (2) INPUT POINTER TO START OF FIRST STANDARD LEVEL
!                      (POINTS PAST END OF SECTION ON RETURN)
!                 (3) OUTPUT ARRAY, WITH HEADER DATA ALREADY IN IT
!                 (4) OUTPUT SUBSCRIPT (NOT INCREMENTED HERE)
!                 (5) PART (B OR D)
!                 (6) STATION HEIGHT
!                 (7) FLAG SET ON IF WINDS IN KNOTS
!                 (8) NUMBER OF WINDS IN REPORT
!                 (9) QC 1BIT FLAG OUTPUT
!                (10) LENGTH OF REPORT
!
! REVISION INFO :
!
! $Workfile: uasight.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 12/04/2011 10:48:23$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         12/04/2011 10:48:23    Brian Barwell   Check
!       report length before decoding altitude data.
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
USE METDB_COM_mod, ONLY: MISSIN, RMISS, KTS2MPS

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)  :: STRING    ! report string being expanded
INTEGER, INTENT(INOUT)         :: PTR       ! pointer within report
REAL, INTENT(OUT)              :: ARRAY(:)  ! expanded elements array
INTEGER, INTENT(INOUT)         :: NUM_LEV
CHARACTER (LEN=1), INTENT(IN)  :: PART      ! report type
INTEGER, INTENT(IN)            :: STNHT     ! height of station
LOGICAL,INTENT(IN)             :: KNOTS     ! indicates wind speed in knots
INTEGER, INTENT(IN)            :: SIG_BASE
REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)  ! qc 1 bit flag array
INTEGER, INTENT(IN)            :: LENG      ! Length of report

! Local Variables

INTEGER  :: DD                 ! wind direction
INTEGER  :: FF                 ! wind speed
INTEGER  :: HT                 ! height from report
INTEGER  :: I                  ! used as loop counter
INTEGER  :: NWINDS             ! displacement of current wind
INTEGER  :: UNITS(3)           ! from report
INTEGER  :: STEP               ! indicates which multiple
INTEGER  :: TENS               ! from report

CHARACTER (LEN=1)  :: IND      ! indicator for height from report

! --------------------------------------------------------------------
! the data consists of a height group followed by up to 3 winds, this
! sequence being repeated as often as necessary.  a height group has
! an indicator followed by a tens figure and up to 3 units figures.
! heights are multiples of 300m (indicator 9) or 500m (indicator 8).
! --------------------------------------------------------------------

SAVE

!initialise variables
NWINDS=0

 1 CONTINUE

IND=STRING(PTR:PTR)

IFBLOCK1: &
IF (IND == '8' .OR. IND == '9') THEN     !check for indicator
  IF (IND == '8') THEN
    STEP=500
  ELSE
    STEP=300
  END IF

  IF (PTR+6 > LENG) THEN
    TENS     = MISSIN
    UNITS(1) = MISSIN
    UNITS(2) = MISSIN
    UNITS(3) = MISSIN
  ELSE
    TENS     = IVALUE(STRING(PTR+1:PTR+1))
    UNITS(1) = IVALUE(STRING(PTR+2:PTR+2))
    UNITS(2) = IVALUE(STRING(PTR+3:PTR+3))
    UNITS(3) = IVALUE(STRING(PTR+4:PTR+4))
  END IF

  IF ((UNITS(2) /= MISSIN .AND. UNITS(1) > UNITS(2)) .OR.    &
  (UNITS(3) /= MISSIN .AND. UNITS(2) > UNITS(3))) RETURN
  PTR=PTR+6

! --------------------------------------------------------------------
! combine the tens & units figures to give multiples of 300/500m.
! --------------------------------------------------------------------

DOBLOCK1: &
  DO I=1,3
    IF (PART == 'B' .AND. I == 1 .AND. NWINDS == 0  &
    .AND. UNITS(1) == MISSIN) THEN
      HT=STNHT
    ELSE IF (TENS /= MISSIN .AND. UNITS(I) /= MISSIN) THEN
      HT=(TENS*10+UNITS(I))*STEP
    ELSE
      GO TO 20                       ! next cycle
    END IF

! --------------------------------------------------------------------
!deal with wind direction
! --------------------------------------------------------------------

    IF ((PTR+1) <= LENG) THEN
      DD=IVALUE(STRING(PTR:PTR+1))  !get direction from report
    ELSE
      DD=MISSIN
    END IF
    IF ((PTR+4) <= LENG) THEN
      FF=IVALUE(STRING(PTR+2:PTR+4))  !get speed from report
    ELSE
      FF=MISSIN
    END IF

    IF (DD /= MISSIN) THEN
      DD=DD*10
      IF (FF >= 500) THEN             !check to see if direction
        FF=FF-500                     !end in a 5. Correct speed
        DD=DD+5                       !correct direction
      END IF
      IF (DD > 360) THEN              !check for valid direction
        DD=MISSIN                     !set missing if invalid
        ARRAY(SIG_BASE+(7*NUM_LEV)+5)=DD
        QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=1.
      ELSE
        ARRAY(SIG_BASE+(7*NUM_LEV)+5)=DD
        QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=0.
      END IF
    END IF

    PTR=PTR+6                         !move pointer past group

! --------------------------------------------------------------------
! store wind speed: convert to m/s if given in knots
! --------------------------------------------------------------------

    IF (FF /= MISSIN) THEN
      IF (KNOTS) THEN
        ARRAY(SIG_BASE+(7*NUM_LEV)+6)=FF*KTS2MPS
      ELSE

        ARRAY(SIG_BASE+(7*NUM_LEV)+6)=FF
      END IF
    ELSE                              !wind speed missing
      ARRAY(SIG_BASE+(7*NUM_LEV)+6)=RMISS
    END IF

    ARRAY(SIG_BASE+(7*NUM_LEV))=2               !SET LEVEL TYPE
    ARRAY(SIG_BASE+(7*NUM_LEV)+2)=HT            !set height
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV))=0.        !LEVEL ID OKAY
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+1)=0.      !PRESS OKAY
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+2)=0.      !HT OKAY
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+3)=0.      !TEMP OKAY
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+4)=0.      !DEW POINT OKAY
    QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+6)=0.      !WIND SPEED OKAY

    NUM_LEV=NUM_LEV+1

 20 CONTINUE

  END DO DOBLOCK1

  IF (PTR < LENG) GO TO 1
END IF IFBLOCK1

RETURN
END SUBROUTINE UASIGHT
