SUBROUTINE UATROP(OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : UATROP
!
! PURPOSE       : EXPAND TROPOPAUSE DATA IN TEMP PART A/C
!
! DATA TYPE(S)  : UPPER AIR TEMP PART A/C
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION)
!
! ARGUMENTS     : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                 (2) INPUT POINTER TO START OF FIRST TROPOPAUSE
!                      (POINTS PAST END OF SECTION ON RETURN)
!                 (3) OUTPUT ARRAY, WITH STANDARD LEVEL DATA IN IT
!                 (4) number of levels already in array    (i/o)
!                 (5) PART (A OR C)
!                 (6) FLAG SET ON IF WINDS IN KNOTS
!                 (7) output 1 bit QC flags
!
! REVISION INFO :
!
!
! $Workfile: uatrop.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 24/01/2011 13:05:29$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE METDB_COM_mod, ONLY: MISSIN, RMISS, TCONV, KTS2MPS
USE IVALUE_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)  :: OB       ! (a1) report being expanded
INTEGER, INTENT(INOUT)         :: PTR      ! (a2) pointer within report
REAL, INTENT(OUT)              :: ARRAY(:) ! (a3) array of decoded v    alues
INTEGER, INTENT(INOUT)         :: NUM_LEV  ! (a4) number of levels
CHARACTER (LEN=1), INTENT(IN)  :: PART     ! (a5) message part (A or C)
LOGICAL, INTENT(IN)            :: KNOTS    ! (a6) indicates wind speed knots
REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)   ! (a7) array of 1 bit QC flags

! Local Parameters

INTEGER, PARAMETER :: BASE = 8 ! Use in array subscript

! Local Variables

INTEGER  :: DD                        !wind direction
INTEGER  :: FF                        !wind speed
INTEGER  :: L
INTEGER  :: NTROP                     !number of trops. found
INTEGER  :: P
INTEGER  :: T                         !temperature
INTEGER  :: TD                        !dew point depression

SAVE

! -------------------------------------------------------------------

!initialize variables

NTROP=0

 1 CONTINUE

P=MISSIN
T=MISSIN
TD=MISSIN
DD=MISSIN
FF=MISSIN

! --------------------------------------------------------------------
! 3 groups per tropopause (can be >1 trop): 88 with 3-fig press, temp
! group, wind.
! --------------------------------------------------------------------
P=IVALUE(OB(PTR+2:PTR+4))

IFBlock1: &
IF (P /= MISSIN) THEN
  NUM_LEV=NUM_LEV+1                   ! one more level
  IF (PART == 'A') P=P*100            ! PRESSURE TO PASCALS
  IF (PART == 'C') P=P*10             ! (PART C TENTHS OF HPA)

! --------------------------------------------------------------------
! temperature & dew point
! --------------------------------------------------------------------

  T=IVALUE(OB(PTR+6:PTR+8))           ! TEMPERATURE IN TENTHS
  IF (T /= MISSIN) THEN
    IF (MOD(T,2) == 1) T=-T           ! <0 IF TENTHS FIGURE ODD
    TD=IVALUE(OB(PTR+9:PTR+10))       ! DEW POINT DEPRESSION
    IF (TD > 50) TD=(TD-50)*10        ! IN WHOLE DEGREES IF >50
    IF (TD /= MISSIN) TD=T-TD         ! DEPRESSION TO DEW POINT
  END IF

! --------------------------------------------------------------------
! wind             (there should always be a wind, at least slashes
!                    - but check for next indicator just in case)
! --------------------------------------------------------------------

  L=LEN(OB(PTR:))
  IF (L >= 14) THEN
    DD=IVALUE(OB(PTR+12:PTR+13))
  END IF
  IF (L >= 17 .AND. DD /= 88 .AND. DD /= 77 .AND. DD /= 66) THEN
    FF=IVALUE(OB(PTR+14:PTR+16))
    IF (DD /= MISSIN) THEN
      DD=DD*10
      IF (FF >= 500) THEN            !check for accuracy in dir.
        FF=FF-500                    !500 added to speed if 5 deg.
        DD=DD+5
      END IF
      IF (DD > 360) THEN             !check for valid direction
        DD=MISSIN
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=1
      ELSE
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0
      END IF
    END IF
    PTR=PTR+18                       !move pointer on past group
  ELSE
    PTR=PTR+12
    DD=MISSIN                        !don't leave dd=77 etc
  END IF

! --------------------------------------------------------------------
! set values in output array for encoding
! deal with level type, pressure and height
! --------------------------------------------------------------------

  ARRAY(BASE+(7*NUM_LEV))=16                     !level type=trop
  ARRAY(BASE+(7*NUM_LEV)+1)=P                    !set pressure
  ARRAY(BASE+(7*NUM_LEV)+2)=MISSIN               !no height
  QCBIT_ARRAY(BASE+(7*NUM_LEV))=0
  QCBIT_ARRAY(BASE+(7*NUM_LEV)+1)=0
  QCBIT_ARRAY(BASE+(7*NUM_LEV)+2)=0

! --------------------------------------------------------------------
!deal with air temperature and dew point
! --------------------------------------------------------------------

  IF (T /= MISSIN) THEN
    ARRAY(BASE+(7*NUM_LEV)+3)=T*0.1 + TCONV         !temperature
  ELSE
    ARRAY(BASE+(7*NUM_LEV)+3)=RMISS
  END IF

  IF (TD /= MISSIN) THEN
    ARRAY(BASE+(7*NUM_LEV)+4)=TD*0.1 + TCONV        !temperature
  ELSE
    ARRAY(BASE+(7*NUM_LEV)+4)=RMISS
  END IF

  QCBIT_ARRAY(BASE+(7*NUM_LEV)+3)=0
  QCBIT_ARRAY(BASE+(7*NUM_LEV)+4)=0

! --------------------------------------------------------------------
!deal with wind direction and speed.
! --------------------------------------------------------------------

  ARRAY(BASE+(7*NUM_LEV)+5)=DD                      !wind direct
  IF (FF /= MISSIN) THEN
    IF (KNOTS) THEN
      ARRAY(BASE+(7*NUM_LEV)+6)=FF*KTS2MPS          !wind speed
    ELSE
      ARRAY(BASE+(7*NUM_LEV)+6)=FF                  !wind speed
    END IF
  ELSE
    ARRAY(BASE+(7*NUM_LEV)+6)=RMISS
  END IF
  QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0
  QCBIT_ARRAY(BASE+(7*NUM_LEV)+6)=0

! --------------------------------------------------------------------
! increment NUM_LEV at start of IF block, not end;.
! check to see if another tropopause has been reported. If there is then
! go back to the beginning and decode.
! --------------------------------------------------------------------

  IF (PTR < LEN(OB)) THEN
    IF (OB(PTR:PTR+1) == '88') THEN
      GOTO 1
    END IF

  END IF
END IF IFBlock1

RETURN
END SUBROUTINE UATROP
