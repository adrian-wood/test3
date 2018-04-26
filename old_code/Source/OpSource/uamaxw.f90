SUBROUTINE UAMAXW (OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,QCBIT_ARRAY,LENG)

!-----------------------------------------------------------------------
!
! PROGRAM       : UAMAXW
!
! PURPOSE       : EXPAND MAX WIND DATA IN TEMP OR PILOT PART A/C
!                 This version only stores one wind shear but all
!                 max winds
!
! DATA TYPE(S)  : UPPER AIR TEMP PART A & PILOT PART A/C
!
! CALLED BY     : UAXPAND (separate calls for TEMP & PILOT)
!
! CALLS         : IVALUE (FUNCTION)
!
! ARGUMENTS     : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                 (2) INPUT POINTER TO START OF FIRST MAX WIND
!                      (POINTS PAST END OF SECTION ON RETURN)
!                 (3) OUTPUT ARRAY, WITH STANDARD LEVEL DATA IN IT
!                 (4) number of levels already in array    (i/o)
!                 (5) PART (A OR C)
!                 (6) FLAG SET ON IF WINDS IN KNOTS
!                 (7) qc 1 bit flags output
!                 (8) length of report
!
! REVISION INFO :
!
!
! $Workfile: uamaxw.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 12/04/2011 10:45:44$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         12/04/2011 10:45:44    Brian Barwell   Change
!        '>=' to '<=' in several places. Change QCBIT_ARRAY to INOUT and
!       delete initialisation.
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

CHARACTER (LEN=*), INTENT(IN)  :: OB             ! (a1) report being decoded
INTEGER, INTENT(INOUT)         :: PTR            ! (a2) pointer within report
REAL, INTENT(OUT)              :: ARRAY(:)       ! (a3) array of decoded values
INTEGER, INTENT(INOUT)         :: NUM_LEV        ! (a4) number of levels
CHARACTER (LEN=1), INTENT(IN)  :: PART           ! (a5) report type
LOGICAL, INTENT(IN)            :: KNOTS          ! (a6) set if speed in knots
REAL, INTENT(INOUT)            :: QCBIT_ARRAY(:) ! (a7) array of 1-bit qc f lags
INTEGER, INTENT(IN)            :: LENG           ! (a8) Length of report

! Local Parameters

INTEGER, PARAMETER  :: BASE = 8     !used to determine subscripts

! Local Variables

INTEGER  :: DD                   !wind direction
INTEGER  :: FF                   !wind speed
INTEGER  :: HT                   !height
INTEGER  :: P                    !pressure
INTEGER  :: VB                   !wind shear above and
INTEGER  :: VA                   !below max wind

LOGICAL  :: GOT_SHEAR            ! set if wind shear found

SAVE

! ---------------------------------------------------------------------
! More than one max wind can be handled, including 77... followed by
! 66... (but UAXPAND looks for 77... first, so they must be in order).
! ---------------------------------------------------------------------

! Initialize variables

! Why is base set to 8?  There are 14 elements before the replication
! over levels.  NUM_LEV is incremented at the start of the level.  So
! the first element for the first level, the 15th in the array, has
! subscript 14+7*(NUM_LEV-1)+1 = 8+7*NUM_LEV.

GOT_SHEAR=.FALSE.                !reset while looking for more

VB=MISSIN
VA=MISSIN

1  CONTINUE

P=MISSIN
HT=MISSIN
DD=MISSIN
FF=MISSIN

! ---------------------------------------------------------------------
! TEMPs & some PILOTs have 77 or 66 followed by a pressure in millibars
! (tenths if part C),
! other pilots a single 6 or 7 followed by a height in tens of metres
! There are 2 or 3 groups per max wind, the shear group being optional
! ---------------------------------------------------------------------

IF (OB(PTR:PTR+1) == '77' .OR. OB(PTR:PTR+1) == '66') THEN
  IF ((PTR+4) <= LENG) P=IVALUE(OB(PTR+2:PTR+4))
  IF (P /= MISSIN .AND. PART == 'A') P=P*100
  IF (P /= MISSIN .AND. PART == 'C') P=P*10
ELSE IF (OB(PTR:PTR) == '7' .OR. OB(PTR:PTR) == '6') THEN
  IF ((PTR+4) <= LENG) HT=IVALUE(OB(PTR+1:PTR+4))
  IF (HT /= MISSIN) HT=HT*10
END IF

! ---------------------------------------------------------------------
! Wind  (a max wind must be above 500mb)
! ---------------------------------------------------------------------

IF_Search: &
IF ((P /= MISSIN .AND. P <= 50000) .OR.   &
    (P == MISSIN .AND. HT /= MISSIN)) THEN
  IF ((PTR+7) <= LENG) DD=IVALUE(OB(PTR+6:PTR+7))
  IF ((PTR+10) <= LENG) FF=IVALUE(OB(PTR+8:PTR+10))
  IF (DD /= MISSIN .AND.DD <= 36 .AND. FF > 30) THEN
    NUM_LEV=NUM_LEV+1            ! one more level
    DD=DD*10
    IF (FF >= 500) THEN          !see if direction ends in 5
      FF=FF-500                  !correct the speed
      DD=DD+5                    !correct wind direction
    END IF
    PTR=PTR+12                   !move past groups

! ---------------------------------------------------------------------
! Wind shears in kilometre above & below  (optional group 4VbVa)
! Only the first wind shear group is used. GOT_SHEAR is set to
! TRUE when the first wind shear is decoded. After that UAMAXW will
! check for a wind shear group and then skip over it.
! ---------------------------------------------------------------------

    IF (PTR < LENG) THEN
      IF (OB(PTR:PTR) == '4') THEN
        IF (.NOT.GOT_SHEAR) THEN
          IF ((PTR+2) <= LENG) VB=IVALUE(OB(PTR+1:PTR+2))
          IF ((PTR+4) <= LENG) VA=IVALUE(OB(PTR+3:PTR+4))
          GOT_SHEAR=.TRUE.
        END IF
        PTR=PTR+6
      END IF
    END IF

! ---------------------------------------------------------------------
! Set level type, pressure and height in output array.
! ---------------------------------------------------------------------

    ARRAY(BASE+(7*NUM_LEV))=8
    ARRAY(BASE+(7*NUM_LEV+1))=P
    ARRAY(BASE+(7*NUM_LEV+2))=HT

    QCBIT_ARRAY(BASE+(7*NUM_LEV))=0.
    QCBIT_ARRAY(BASE+(7*NUM_LEV)+1)=0.
    QCBIT_ARRAY(BASE+(7*NUM_LEV)+2)=0.

! ---------------------------------------------------------------------
! Air temperature and dewpoint are not reported in this section,
! so missing.
! ---------------------------------------------------------------------

    ARRAY(BASE+(7*NUM_LEV)+3)=MISSIN
    ARRAY(BASE+(7*NUM_LEV)+4)=MISSIN

    QCBIT_ARRAY(BASE+(7*NUM_LEV)+3)=0.
    QCBIT_ARRAY(BASE+(7*NUM_LEV)+4)=0.

! ---------------------------------------------------------------------
!Wind direction and speed. Wind shear is kept for the first max
!wind group found, but only put in the encoding array (outside
!the replication) when all the max winds have been decoded.
! ---------------------------------------------------------------------

    ARRAY(BASE+(7*NUM_LEV)+5)=DD

    IF (KNOTS) THEN
      ARRAY(BASE+(7*NUM_LEV)+6)=FF*KTS2MPS
    ELSE
      ARRAY(BASE+(7*NUM_LEV)+6)=FF
    END IF

    QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0.
    QCBIT_ARRAY(BASE+(7*NUM_LEV)+6)=0.

! ---------------------------------------------------------------------
!Check if more max winds. If so go back and get next level. If not then
!set the wind shear group found with the first max wind.
! ---------------------------------------------------------------------

    IF (PTR < LENG) THEN
      IF (OB(PTR:PTR) == '7' .OR. OB(PTR:PTR) == '6') THEN
        IF (OB(PTR+2:PTR+3) /= '999') GO TO 1
      END IF
    END IF

    IF (KNOTS) THEN
      IF (VB > 0) ARRAY(BASE+(7*NUM_LEV)+7) = KTS2MPS*FLOAT(VB)
      IF (VA > 0) ARRAY(BASE+(7*NUM_LEV)+8) = KTS2MPS*FLOAT(VA)
    ELSE
      ARRAY(BASE+(7*NUM_LEV)+7)=FLOAT(VB)
      ARRAY(BASE+(7*NUM_LEV)+8)=FLOAT(VA)
    END IF
  END IF
END IF IF_Search

RETURN
END SUBROUTINE UAMAXW
