SUBROUTINE UAPSTD(OB,PTR,ARRAY,NUM_LEV,PART,BLOCK,STN,KNOTS,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : UAPSTD  (Upper Air Pilot STanDard levels)
!
! PURPOSE       : EXPAND STANDARD LEVEL DATA IN PILOT PART A OR C
!
! DATA TYPE(S)  : UPPER AIR PILOT PART A/C
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION), STDPRHT (FUNCTION)
!
! ARGUMENTS     : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                 (2) INPUT POINTER TO START OF FIRST STANDARD LEVEL
!                      (POINTS PAST END OF SECTION ON RETURN)
!                 (3) OUTPUT ARRAY, WITH HEADER DATA ALREADY IN IT
!                 (4) OUTPUT SUBSCRIPT
!                 (5) PART (A OR C)
!                 (6) BLOCK NUMBER
!                 (7) STATION NUMBER
!                 (8) FLAG SET ON IF WINDS IN KNOTS  (--> *.5148)
!                 (9) NUMBER OF WINDS IN REPORT
!                 (10) 1bit qc bit array output
!
! REVISION INFO :
!
!
! $Workfile: uapstd.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 19/04/2011 12:14:35$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         19/04/2011 12:14:35    Brian Barwell   Test
!       on level number moved to start of DO loop.
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
USE STDPRHT_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)  :: OB       ! report being decoded
INTEGER, INTENT(INOUT)         :: PTR      ! pointer within report
REAL, INTENT(OUT)              :: ARRAY(:) ! array of decoded elements
INTEGER, INTENT(INOUT)         :: NUM_LEV
CHARACTER (LEN=1), INTENT(IN)  :: PART     ! report part
INTEGER, INTENT(IN)            :: BLOCK    ! WMO block no.
INTEGER, INTENT(IN)            :: STN      ! WMO station number
LOGICAL, INTENT(IN)            :: KNOTS    ! indicates wind speed in knots if true
REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)  ! array of 1 bit qc flags

! Local Parameters

INTEGER, PARAMETER             :: BASE = 8 ! BASE displacement of array

! Local Variables

INTEGER  :: DD              !wind direction
INTEGER  :: FF              !wind speed
INTEGER  :: HT              !height
INTEGER  :: I
INTEGER  :: L               ! level subscript
INTEGER  :: LENG
INTEGER  :: N
INTEGER  :: P               !pressure
INTEGER  :: PRESS(14)

CHARACTER (LEN=2)  :: IND
CHARACTER (LEN=2)  :: LEV
CHARACTER (LEN=2)  :: LEVEL(14)

! --------------------------------------------------------------------

SAVE

DATA PRESS/  &
 85000, 70000, 50000, 40000, 30000, 25000, 20000, 15000, 10000, &
  7000,  5000,  3000,  2000,  1000/

DATA LEVEL/  &
 '85',  '70',  '50',  '40',  '30',  '25',  '20',  '15',  '10',  &
 '70',  '50',  '30',  '20',  '10'/

! -------------------------------------------------------------------

LENG=LEN(OB)

! ---------------------------------------------------------------------
! the above list gives the standard levels in part a and then part c
! (pilots have no 1000mb or 925mb level)
! The first group must end 85, 70 or 50 to get through UAXPAND.
! ---------------------------------------------------------------------

IF (PART == 'A') THEN
  DO I=1,3
    IF (OB(PTR+3:PTR+4) == LEVEL(I)) L=I
  END DO
ELSE IF (PART == 'C') THEN
  L=10
END IF

! ---------------------------------------------------------------------
! the data consists of a pressure group followed by up to 3 winds,
! this sequence being repeated as often as necessary.  the pressure
! group has an indicator (44 or 55), a count (up to 3) and a 2-figure
! level (as in the above array) for the first of the winds that follow
! ---------------------------------------------------------------------

 1 CONTINUE

IND=OB(PTR:PTR+1)           ! '44' or '55'
N=IVALUE(OB(PTR+2:PTR+2))   ! Number of levels (1-3)
LEV=OB(PTR+3:PTR+4)         ! Code for first level

! ---------------------------------------------------------------------
! an indicator of 44 means that the pilot has a pressure sensor, so the
! pressures are genuine pressures and the heights unknown; 55 indicates
! that heights have been measured and the pressures are only nominal -
! heights are generated for all reports regardless of instumentation
! trailer byte 17 bit 5 is set to indicate if the part being decoded had
! a pressure sensor - this is used later in retrieval to inform users
! ---------------------------------------------------------------------

IFBLOCK1: &
IF (IND == '44' .OR. IND == '55') THEN
  PTR=PTR+6                                 ! past 44/55 group

IFBLOCK2: &
  IF (N /= MISSIN .AND. N <= 3 .AND. LEV == LEVEL(L)) THEN

DOBLOCK1: &
    DO I=1,N
      IF ((PART == 'A' .AND. L > 9) .OR.     &
          (PART == 'C' .AND. L > 14)) GO TO 100
      P=PRESS(L)
      IF (BLOCK /= MISSIN .AND. STN /= MISSIN) THEN
        HT=STDPRHT(BLOCK,STN,L)
      ELSE
        HT=MISSIN
      END IF

      IF ((PTR+4) <= LENG) THEN
        DD=IVALUE(OB(PTR:PTR+1))            !get direction
        FF=IVALUE(OB(PTR+2:PTR+4))          !get wind speed
      ELSE
        DD=MISSIN
        FF=MISSIN
      END IF

      IF (DD /= MISSIN) THEN
        DD=DD*10
        IF (FF >= 500) THEN
          FF=FF-500
          DD=DD+5
        END IF
        IF (DD > 360) THEN                  !check direction valid
          DD=MISSIN
        ELSE
        END IF
      END IF
      PTR=PTR+6                             ! past wind group

! ---------------------------------------------------------------------
!set level type indicator
! ---------------------------------------------------------------------

      ARRAY(BASE+(7*NUM_LEV))=32            !indicate sig level winds
      QCBIT_ARRAY(BASE+(7*NUM_LEV))=0.
! ---------------------------------------------------------------------
!deal with pressure and height
! ---------------------------------------------------------------------
      ARRAY(BASE+(7*NUM_LEV)+1)=P           !set pressure
      ARRAY(BASE+(7*NUM_LEV)+2)=HT          !set height
      QCBIT_ARRAY(BASE+(7*NUM_LEV)+1)=0.
      QCBIT_ARRAY(BASE+(7*NUM_LEV)+2)=0.

! ---------------------------------------------------------------------
!deal with temperature and dew point. NOTE in PILOT reports there is
!no temperature or dew point
! ---------------------------------------------------------------------

      ARRAY(BASE+(7*NUM_LEV)+3)=RMISS
      ARRAY(BASE+(7*NUM_LEV)+4)=RMISS
      QCBIT_ARRAY(BASE+(7*NUM_LEV)+3)=0.
      QCBIT_ARRAY(BASE+(7*NUM_LEV)+4)=0.

! ---------------------------------------------------------------------
!deal with wind direction and wind speed
! ---------------------------------------------------------------------

      ARRAY(BASE+(7*NUM_LEV)+5)=DD
      QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0.

      IF (FF /= MISSIN) THEN
        IF (KNOTS) THEN
          ARRAY(BASE+(7*NUM_LEV)+6)=FF*KTS2MPS
        ELSE
          ARRAY(BASE+(7*NUM_LEV)+6)=FF
        END IF
      ELSE
        ARRAY(BASE+(7*NUM_LEV)+6)=RMISS
      END IF
      QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0.
      QCBIT_ARRAY(BASE+(7*NUM_LEV)+6)=0.

! ---------------------------------------------------------------------
!increment number of levels
! ---------------------------------------------------------------------

      NUM_LEV=NUM_LEV+1
      L=L+1                                 ! next standard level

    END DO DOBLOCK1

    IF (PTR <= LENG) THEN
      IF (LEN(OB(PTR:)) > 10) GO TO 1
    END IF
  END IF IFBLOCK2
END IF IFBLOCK1

 100 CONTINUE

NUM_LEV=NUM_LEV-1

RETURN
END SUBROUTINE UAPSTD
