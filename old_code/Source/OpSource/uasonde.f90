SUBROUTINE UASONDE(OB,ARRAY,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : UASONDE
!
! PURPOSE       : TO HANDLE 31313 SECTION IN TEMP (SONDE
!                 DETAILS & LAUNCH TIME; WATER TEMPERATURE IF SHIP)
!
! DATA TYPE(S)  : UPPER AIR TEMP
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION), DATE31,DATE13
!
! ARGUMENTS     : (1) INPUT REPORT (5-FIG GROUPS) STARTING AT 31313
!                 (2) OUTPUT ARRAY, WITH HEADER DATA ALREADY IN IT
!                 (3) Q/C FLAG ARRAY                       (output)
!
! REVISION INFO :
!
!
! $Workfile: uasonde.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 26/01/2011 14:22:21$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         26/01/2011 14:22:21    Rosemary Lavery
!       updates post review
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

USE IVALUE_mod
USE METDB_COM_mod, ONLY: MISSIN, RMISS, TCONV
USE ZPDATE_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)  :: OB                 ! (a1) report being expanded
REAL, INTENT(INOUT)            :: ARRAY(:)           ! (a2)
REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)   ! (a3) array of 1bit qc flags

! Interface Arguments

INTEGER  :: CENDAY
INTEGER  :: DAY
INTEGER  :: HOUR                    !hour of report
INTEGER  :: IHM
INTEGER  :: LENG                    !length of OB
INTEGER  :: MINUTE                  !minutes of report
INTEGER  :: NOMHOUR
INTEGER  :: MONTH
INTEGER  :: RADCOR                  !radiation correction
INTEGER  :: SONDE                   !type of sonde
INTEGER  :: TRACK                   !type of tracking
INTEGER  :: TW                      !water temperature
INTEGER  :: YEAR

SAVE

! --------------------------------------------------------------------

! Initialize variables

LENG=LEN(OB)

!----------------------------------------------------------------------
! The data consists of an 8-group (time of launch) perhaps preceded by
! a group starting with a radiation correction from 0 to 7, and
! perhaps followed by a 9-group (water temperature).
!----------------------------------------------------------------------

ARRAY(14)=RMISS
ARRAY(15)=RMISS
ARRAY(16)=RMISS
IHM=0

IF (LENG >= 13) THEN
  IF (OB(7:7) <= '7' .AND. OB(13:13) == '8') THEN
    RADCOR=IVALUE(OB(7:7))             ! RADIATION CORRECTION
    SONDE=IVALUE(OB(8:9))              ! TYPE OF RADIOSONDE
    TRACK=IVALUE(OB(10:11))            ! TRACKING SYSTEM
    ARRAY(14)=SONDE
    ARRAY(15)=TRACK
    ARRAY(16)=RADCOR
    IHM=13                             ! POINTER TO LAUNCH TIME
  END IF
END IF

IF (LENG >= 7) THEN
  IF (OB(7:7) == '8') THEN
    IHM=7                            ! LAUNCH TIME IN 1ST GROUP
  END IF
END IF

QCBIT_ARRAY(14)=0                    !set all qc bits to
QCBIT_ARRAY(15)=0                    !okay
QCBIT_ARRAY(16)=0

IF_Block1: &
IF (IHM > 0) THEN
  IF ((IHM+2) <= LENG) THEN
    HOUR=IVALUE(OB(IHM+1:IHM+2))     ! HOUR OF LAUNCH
  ELSE
    HOUR=MISSIN
  END IF
  IF ((IHM+4) <= LENG) THEN
    MINUTE=IVALUE(OB(IHM+3:IHM+4))   ! MINUTE OF LAUNCH
  ELSE
    MINUTE=MISSIN
  END IF

!-----------------------------------------------------------------------
! If the launch hour is the same as the nominal hour or the next
! hour, set the minutes.  Launch time is mandatory in all parts
! from May 2000.
!-----------------------------------------------------------------------

  NOMHOUR=ARRAY(12)                  ! HOUR FROM START OF REPORT
  IF (MINUTE >= 0 .AND. MINUTE < 60) THEN
    IF (HOUR == NOMHOUR .OR. HOUR == NOMHOUR-1  &
        .OR. (HOUR == 23.AND.NOMHOUR == 0)) THEN
      ARRAY(12)=HOUR                 ! reset hour
      ARRAY(13)=MINUTE               ! set minute in array
      IF (HOUR == 23.AND.NOMHOUR == 0) THEN
        YEAR=ARRAY(9)
        MONTH=ARRAY(10)
        DAY=ARRAY(11)
        CALL DATE31(DAY,MONTH,YEAR,CENDAY)
        CALL DATE13(CENDAY-1,DAY,MONTH,YEAR)
        ARRAY(9)=YEAR
        ARRAY(10)=MONTH
        ARRAY(11)=DAY
      END IF
    END IF
  END IF
  QCBIT_ARRAY(12)=0                  !set qc bit for hour = okay
  QCBIT_ARRAY(13)=0                  !set qc bit for minute =okay

!----------------------------------------------------------------------
! Only ships have water temperature group, so see if call sign set.
!----------------------------------------------------------------------

  IF (IHM+10 < LENG) THEN
    IF (ARRAY(3) > RMISS .AND. OB(IHM+6:IHM+6) == '9') THEN
      TW=IVALUE(OB(IHM+8:IHM+10))        ! WATER TEMPERATURE
      IF (OB(IHM+7:IHM+7) == '1') TW=-TW ! PRECEDED BY SIGN
      ARRAY(17)=TCONV+TW*0.1             !convert to kelvin
    ELSE
      ARRAY(17)=RMISS
    END IF
  ELSE
    ARRAY(17)=RMISS
  END IF
  QCBIT_ARRAY(17)=0                    !set qc bit for water temp.
END IF IF_Block1

RETURN                                 !return to uaxpand
END SUBROUTINE UASONDE
