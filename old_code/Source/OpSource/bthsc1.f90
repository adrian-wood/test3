SUBROUTINE BTHSC1(REPORT,REPLEN,EXPARR,POS,DATIME,MIMJ,IERR)

!-----------------------------------------------------------------------
!
! PROGRAM      : BTHSC1
!
! PURPOSE      : TO EXPAND SECTION 1 OF BATHY (TIME,
!                POSITION, WIND & SURFACE TEMPERATURE)
!
! CALLED BY    : BTHEXP
!
! CALLS        : IVALUE   CONVERTS FROM CHAR TO NUMERIC TYPE
!              : DATIM
!
! ARGUMENTS    : REPORT   CHARACTER STRING OF REPORT (I)
!                REPLEN   LENGTH OF REPORT  (I)
!                EXPARR   EXPANSION ARRAY   (O)
!                POS      SECTION LOCATING VARIABLE (I/O)
!                DATIME   DATE/TIME ARRAY (O)
!                MIMJ     REPORT IDENTIFIER  (I)
!                IERR     REPORT STATUS (DUFF REPORT = 16)
!
! REVISION INFO:
!
! $Workfile: bthsc1.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 18/04/2011 16:42:32$
!
! CHANGE RECORD:
!
! $Log:
!  7    MetDB_Refresh 1.6         18/04/2011 16:42:32    Brian Barwell   EXPARR
!        changed to INOUT to preserve whole array.
!  6    MetDB_Refresh 1.5         05/04/2011 11:15:54    Brian Barwell   EXPARR
!        declaration changed.
!  5    MetDB_Refresh 1.4         28/03/2011 17:11:22    Richard Weedon
!       redrafted after testing of MDBSTOR
!  4    MetDB_Refresh 1.3         25/01/2011 15:30:49    Rosemary Lavery minor
!       uopdates post review
!  3    MetDB_Refresh 1.2         19/01/2011 11:26:04    Richard Weedon  added
!       USE statement for call to DATIM subroutine
!  2    MetDB_Refresh 1.1         19/01/2011 10:33:36    Richard Weedon
!       amended var declaration statements
!  1    MetDB_Refresh 1.0         18/01/2011 16:11:33    Richard Weedon
!       Initial draft. VAR,INTENT(OUT) not yet set. Passes basic compilation
!       test. Added ivalue_mod statement and removed var declaration for
!       IVALUE.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!Modules
USE datim_mod
USE ivalue_mod

IMPLICIT NONE
!
! Arguments
!
CHARACTER(LEN=*), INTENT(IN)      :: REPORT
INTEGER,          INTENT(IN)      :: REPLEN
REAL,             INTENT(INOUT)   :: EXPARR(0:)
INTEGER,          INTENT(INOUT)   :: POS
INTEGER,          INTENT(OUT)     :: DATIME(5)
CHARACTER(LEN=4), INTENT(IN)      :: MIMJ
INTEGER,          INTENT(OUT)     :: IERR

! Local Variable Declarations

REAL                                :: MISING = -9999999.
REAL                                :: KT2MPS
REAL                                :: YEAR
REAL                                :: MONTH
REAL                                :: DAY
REAL                                :: HOUR
REAL                                :: MIN
REAL                                :: LAT
REAL                                :: LONG
REAL                                :: TEMP
REAL                                :: WDIR
REAL                                :: WSPEED

INTEGER                   ::    IDEG   ! copy of deg for range checks
INTEGER                   ::    IMIN   ! copy of mins for range checks
INTEGER                   ::    ITHOU  ! thousandths of lat or long
INTEGER                   ::    IYEAR
INTEGER                   ::    IMONTH
INTEGER                   ::    IDAY
INTEGER                   ::    SIGN
INTEGER                   ::    IU     ! INDICATOR FOR UNITS IN WHICH
                                       ! WINDSPEED HAS BEEN REPORTED
INTEGER                   ::    NOW(8) ! FOR STORING SYS DATE AND TIME
INTEGER                   ::    SYSYR  ! SYSTEM YEAR = ELEMENT NOW(8)
INTEGER                   ::    QUAD   ! QUADRANT OF GLOBE
!
LOGICAL                   ::    KNOTS  ! FLAG FOR WINDSPEED UNITS
LOGICAL                   ::    VALDDY ! TRUE IF DATE VALUES CORRECT

!************************************************************
!
!     ON ENTRY, POS IS INDICATING THE FIRST CHARACTER
!     AFTER MIMJ I.E. BEGINNING OF SECTION 1
!
!     SET INITIAL VALUES
!     KT2MPS - KNOTS TO M/S CONVERSION FACTOR
!     WDIR - WIND DIRECTION
!     WSPEED - WIND SPEED
!
!************************************************************
! Initialise variables

KT2MPS = 0.5148
YEAR = MISING
MONTH = MISING
DAY = MISING
HOUR = MISING
MIN = MISING
LAT = MISING
LONG = MISING
WDIR = MISING
WSPEED = MISING
TEMP = MISING
IERR=0
KNOTS = .FALSE.

!************************************************************
!
!     RESET POSITION BACK TO THE BEGINNING OF REPORT
!     THEN MOVE PAST MIMJ
!
!************************************************************

POS = 1
IF (REPORT(POS:POS+3) == MIMJ) THEN
  POS = POS + 5
  IF (REPORT(POS:POS) == ' ') POS=POS+1
END IF

!************************************************************
!
!     START STORING ELEMENTS INTO ARRAY
!     ALSO CHECKING VALIDITY OF THE DATE AND LOCATION DATA
!
!************************************************************

DAY = IVALUE(REPORT(POS:POS+1))

MONTH = IVALUE(REPORT(POS+2:POS+3))

!     ONLY UNIT OF YEAR IS REPORTED
!     USE SYSTEM YEAR TO EXPAND AND VALIDATE YEAR IN REPORT
!     CONSIDERING ONLY DATA A YEAR OLD. ANY OTHER YEAR WILL
!     RESULT IN THE REPORT BEING REJECTED.

YEAR = IVALUE(REPORT(POS+4:POS+4))

CALL DATIM(NOW)
SYSYR = MOD(NOW(8),10)
IF (SYSYR == INT(YEAR)) THEN
  YEAR = NOW(8)

!     Convert single-figure year to 4-figure
ELSE IF ((SYSYR-1) == INT(YEAR)             .OR.&
            (SYSYR == 0 .AND. INT(YEAR) == 9)) THEN
  YEAR = NOW(8) - 1

END IF

!     Vailidate year, month and day using VALDDY
!       function VALDDY (day,month,year)

  IYEAR=INT(YEAR)
  IMONTH=INT(MONTH)
  IDAY=INT(DAY)

IF(.NOT.(VALDDY(IDAY,IMONTH,IYEAR)))THEN
  PRINT*, ' BTHSC1 INVALID DATE >',REPORT(1:REPLEN),'<'&
       ,'DAY = ',DAY,' MONTH = ',MONTH,' YEAR = ',YEAR
  IERR = 16
  GOTO 999
END IF

!************************************************************
!
!     TIME GROUP GGGG/ - STORE AND VALIDATE
!
!************************************************************

POS = POS + 6

HOUR = IVALUE(REPORT(POS:POS+1))
IF (HOUR < 0 .OR. HOUR > 23) THEN
  PRINT*,' BTHSC1 INVALID HOUR >',REPORT(1:REPLEN),'<'
  IERR = 16
  GOTO 999
END IF

MIN = IVALUE(REPORT(POS+2:POS+3))
IF (MIN < 0 .OR. MIN > 59) THEN
  PRINT*,' BTHSC1 INVALID MINUTE >',REPORT(1:REPLEN),'<'
  IERR = 16
  GOTO 999
END IF

POS = POS + 6

!************************************************************
!
!     LATITUDE GROUP - QCLALALALA(La)
!
!     QUAD= QUADRANT OF GLOBE - USE TO MAKE NECESSARY MODS
!          TO DATA
!
!     CONVERT FROM DEGREES AND MINUTES TO DEGREES with fraction
!      (thousandths if MiMiMjMj is JJVV)
! (But 6-figure KKYY groups can end with one or two slashes,
!  meaning hundredths (not minutes!) or tenths.)
!
!************************************************************

QUAD = IVALUE(REPORT(POS:POS))
IF(.NOT.(QUAD == 1.OR.QUAD == 3.OR.QUAD == 5.OR.QUAD == 7))THEN
  PRINT*, ' BTHSC1 INVALID QUAD >',REPORT(1:REPLEN),'<'
  IERR=16
  GOTO 999
END IF

IDEG = IVALUE(REPORT(POS+1:POS+2))
IF (MIMJ /= 'JJVV') THEN
  IMIN = IVALUE(REPORT(POS+3:POS+4))
  IF(IDEG >= 0.AND.IDEG <= 90.AND.IMIN >= 0.AND.IMIN < 60)THEN
    LAT=REAL(IDEG)+REAL(IMIN)*0.016667
    IF (QUAD == 3.OR.QUAD == 5) LAT = -LAT
  ELSE
    PRINT*, ' BTHSC1: INVALID LAT >',REPORT(1:REPLEN),'<'
    IERR=16
    GOTO 999
  END IF
  POS=POS+6
ELSE
  IF (REPORT(POS+4:POS+5) == '//') THEN
    ITHOU=IVALUE(REPORT(POS+3:POS+3))*100
  ELSE IF (REPORT(POS+5:POS+5) == '/') THEN
    ITHOU=IVALUE(REPORT(POS+3:POS+4))*10
  ELSE
    ITHOU=IVALUE(REPORT(POS+3:POS+5))
  END IF
  IF (IDEG >= 0 .AND. IDEG <= 90 .AND. ITHOU >= 0) THEN
    LAT=REAL(IDEG)+REAL(ITHOU)*0.001
    IF (QUAD == 3.OR.QUAD == 5) LAT=-LAT
  ELSE
    PRINT*, ' BTHSC1: INVALID LAT >',REPORT(1:REPLEN),'<'
    IERR=16
    GOTO 999
  END IF
  POS=POS+7
END IF

!************************************************************
!
!     NEXT GROUP = LONGITUDE GROUP  LOLOLOLOLO(Lo)                 !1.5
!
!************************************************************

IDEG = IVALUE(REPORT(POS:POS+2))
IF (MIMJ /= 'JJVV') THEN
  IMIN = IVALUE(REPORT(POS+3:POS+4))
  IF(IDEG >= 0.AND.IDEG <= 180.AND.IMIN >= 0.AND.IMIN < 60)THEN
    LONG=REAL(IDEG)+REAL(IMIN)*0.016667
    IF (QUAD == 5.OR.QUAD == 7) LONG=-LONG
  ELSE
    PRINT*, ' BTHSC1: INVALID LONG >',REPORT(1:REPLEN),'<'
    IERR=16
    GOTO 999
  END IF
  POS=POS+6
ELSE
  IF (REPORT(POS+4:POS+5) == '//') THEN
    ITHOU=IVALUE(REPORT(POS+3:POS+3))*100
  ELSE IF (REPORT(POS+5:POS+5) == '/') THEN
    ITHOU=IVALUE(REPORT(POS+3:POS+4))*10
  ELSE
    ITHOU=IVALUE(REPORT(POS+3:POS+5))
  END IF
  IF (IDEG >= 0 .AND. IDEG <= 180 .AND. ITHOU >= 0) THEN
    LONG=REAL(IDEG)+REAL(ITHOU)*0.001
    IF (QUAD == 5.OR.QUAD == 7) LONG=-LONG
  ELSE
    PRINT*, ' BTHSC1: INVALID LONG >',REPORT(1:REPLEN),'<'
    IERR=16
    GOTO 999
  END IF
  POS=POS+7
END IF
!************************************************************
!
!     NEXT GROUPS ARE OPTIONAL = WIND GROUP AND TEMP GROUP
!
!     TEST FOR WIND GROUP -(IUDDFF) - IU TAKES VALUES 0 TO 3.
!     DIRECTION IS IN TENS OF DEGREES, SPEED IN WHOLE M/S OR KNOTS.
!     CONVERT FROM KNOTS TO M/S IF REQUIRED.
!
!************************************************************

IU = IVALUE(REPORT(POS:POS))
IF (IU >= 0.AND.IU <= 3) THEN
  IF (IU == 1.OR.IU == 3) KNOTS = .TRUE.
!       WIND DIRECTION
  WDIR = IVALUE(REPORT(POS+1:POS+2))
  IF (WDIR /= MISING) WDIR=WDIR*10.
!       WIND SPEED
  WSPEED = IVALUE(REPORT(POS+3:POS+4))
  IF ((KNOTS).AND.(WSPEED /= MISING)) WSPEED = WSPEED * KT2MPS
!       MOVE TO NEXT GROUP
  POS = POS + 6
END IF

!************************************************************
!
!     TEST FOR TEMPERATURE GROUP - 4SNTTT
!
!************************************************************

IF (REPORT(POS:POS) == '4') THEN
  SIGN = IVALUE(REPORT(POS+1:POS+1))
  TEMP = IVALUE(REPORT(POS+2:POS+4))
  IF(TEMP /= MISING) THEN
    IF (SIGN == 1) TEMP = -TEMP
    TEMP = (TEMP * 0.1) + 273.1  ! air temp in tenths
  END IF
!       MOVE TO NEXT GROUP - THIS POSITION WILL INDICATE THE
!       START OF SECTION 2 WITHIN THE REPORT
  POS = POS + 6
END IF

!************************************************************
!
!     ASSIGN VARIABLES TO VALUES AND DATIME ARRAY
!
!************************************************************

999   EXPARR(6) = YEAR
EXPARR(8) = MONTH
EXPARR(10) = DAY
EXPARR(12) = HOUR
EXPARR(14) = MIN
EXPARR(16) = LAT
EXPARR(18) = LONG
EXPARR(20) = WDIR
EXPARR(22) = WSPEED
EXPARR(24) = TEMP

DATIME(1) = YEAR
DATIME(2) = MONTH
DATIME(3) = DAY
DATIME(4) = HOUR
!     DATIME(5) = MINUTE      ! old line
DATIME(5) = MIN         ! hh+/-10mins=0 ?????

RETURN
END SUBROUTINE BTHSC1
