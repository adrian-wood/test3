SUBROUTINE TESSC1(REPORT,POS,EXPARR,DATIME,IERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : TESSC1
!
! PURPOSE       : To expand section 1 of TESAC (time,
!                 position, wind and surface temperature)
!
! CALLED BY     : TESAC
!
! CALLS         : IVALUE   converts figures to numbers
!                 VALDDY   function to check date
!                 DATIM    get current date and time
!
! ARGUMENTS     : REPORT   character string starting with MiMiMjMj  (I)
!                 POS      pointer to report                        (O)
!                 EXPARR   expansion array                         (I/O)
!                 DATIME   data/time array                          (O)
!                 IERR     =16 if duff report                       (O)
!
! REVISION INFO :
!
! $Workfile: tessc1.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 19/01/2011 10:25:53$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         19/01/2011 10:25:53    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         17/01/2011 13:15:24    Alison Weir
!       Initial f77 version - MDBSTOR batch 18.
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

! Use statements:
USE metdb_com_mod, only : RMISS, KTS2MPS
USE ivalue_mod
USE valddy_mod
USE datim_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    ::  REPORT      !a01
INTEGER,          INTENT(OUT)   ::  POS         !a02
REAL,             INTENT(INOUT) ::  EXPARR(0:*) !a03
INTEGER,          INTENT(OUT)   ::  DATIME(5)   !a04
INTEGER,          INTENT(OUT)   ::  IERR        !a05

! Local declarations:

CHARACTER(LEN=4) ::     MIMJ
REAL             ::     YEAR
REAL             ::     MONTH
REAL             ::     DAY
REAL             ::     HOUR
REAL             ::     MIN
REAL             ::     LAT
REAL             ::     LONG
REAL             ::     WDIR
REAL             ::     WSPEED
REAL             ::     TEMP
INTEGER          ::     IYEAR
INTEGER          ::     IMONTH
INTEGER          ::     IDAY
INTEGER          ::     SIGN
INTEGER          ::     IDEG   ! copy of Degrees
INTEGER          ::     IMIN   ! copy of Degrees
INTEGER          ::     ITHOU  ! thousandths of lat or long
INTEGER          ::     IU     ! wind speed units indicator
INTEGER          ::     NOW(8) ! system date/time
INTEGER          ::     SYSYR  ! system year from NOW(8)
INTEGER          ::     QUAD   ! quadrant of globe
LOGICAL          ::     KNOTS  ! FLAG FOR WINDSPEED UNITS

YEAR=RMISS
MONTH=RMISS
DAY=RMISS
HOUR=RMISS
MIN=RMISS
LAT=RMISS
LONG=RMISS
WDIR=RMISS
WSPEED=RMISS
TEMP=RMISS
IU=RMISS
QUAD=RMISS
IERR=0

! Skip MiMiMjMj at start of report and one or two spaces after it

MIMJ=REPORT(1:4)
POS=6
IF (REPORT(POS:POS) == ' ') POS=POS+1

! Next group is date (ddmmy)

DAY=IVALUE(REPORT(POS:POS+1))
MONTH=IVALUE(REPORT(POS+2:POS+3))

! Convert single-figure year to either this year or last

YEAR=IVALUE(REPORT(POS+4:POS+4))
CALL DATIM(NOW)
SYSYR=MOD(NOW(8),10)
IF (SYSYR == INT(YEAR)) THEN
  YEAR=NOW(8)
ELSE IF (SYSYR-1 == INT(YEAR) .OR.    &
        (SYSYR == 0 .AND. INT(YEAR) == 9)) THEN
  YEAR=NOW(8)-1
END IF

! Check date and give up if invalid

IYEAR=INT(YEAR)
IMONTH=INT(MONTH)
IDAY=INT(DAY)

IF(.NOT.(VALDDY(IDAY,IMONTH,IYEAR)))THEN
  PRINT*, ' TESSC1 INVALID DATE >',REPORT,'<'     &
  ,'DAY=',DAY,' MONTH=',MONTH,' YEAR=',YEAR
  IERR=16
  GOTO 999
END IF

POS=POS+6

! Next group is time (hhmm/).  Give up if hour or minute invalid.

HOUR=IVALUE(REPORT(POS:POS+1))
IF (HOUR < 0 .OR. HOUR > 23) THEN
  PRINT*,'TESSC1: invalid hour >',REPORT,'<'
  IERR=16
  GOTO 999
END IF

MIN=IVALUE(REPORT(POS+2:POS+3))
IF (MIN < 0 .OR. MIN > 59) THEN
  PRINT*,'TESSC1: invalid minute >',REPORT,'<'
  IERR=16
  GOTO 999
END IF

POS=POS+6

! Latitude group can have 5 figures or 6, starting with quadrant.
! Minutes if 5 figures, thousandths if 6 (if MiMiMjMj is KKYY);
! but 6-figure KKYY groups can end with one or two slashes,
! meaning hundredths (not minutes!) or tenths.

QUAD=IVALUE(REPORT(POS:POS))
IF (.NOT.(MOD(QUAD,2) == 1)) THEN
  PRINT*,' TESSC1: invalid quadrant >',REPORT,'<'
  IERR=16
  GOTO 999
END IF

IDEG=IVALUE(REPORT(POS+1:POS+2))

! Minutes

IFLABEL1: &
IF (MIMJ /= 'KKYY') THEN
  IMIN=IVALUE(REPORT(POS+3:POS+4))
  IF (IDEG >= 0.AND.IDEG <= 90.AND.IMIN >= 0.AND.IMIN < 60) THEN
    LAT=REAL(IDEG)+REAL(IMIN)*0.016667
    IF (QUAD == 3 .OR. QUAD == 5) LAT=-LAT
  ELSE
    PRINT*,' TESSC1: invalid latitude >',REPORT,'<'
    IERR=16
    GOTO 999
  END IF
  POS=POS+6

! Thousandths

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
    IF (QUAD == 3 .OR. QUAD == 5) LAT=-LAT
  ELSE
    PRINT*,' TESSC1: INVALID LAT >',REPORT,'<'
    IERR=16
    GOTO 999
  END IF
  POS=POS+7
END IF IFLABEL1

! Longitude group can have 5 or 6 figures like latitude

IDEG=IVALUE(REPORT(POS:POS+2))

! Minutes

IFLABEL2: &
IF (MIMJ /= 'KKYY') THEN
  IMIN=IVALUE(REPORT(POS+3:POS+4))
  IF (IDEG >= 0.AND.IDEG <= 180.AND.IMIN >= 0.AND.IMIN < 60) THEN
    LONG=REAL(IDEG)+REAL(IMIN)*0.016667
    IF (QUAD == 5 .OR. QUAD == 7) LONG=-LONG
  ELSE
    PRINT*, ' TESSC1: INVALID LONG >',REPORT,'<'
    IERR=16
    GOTO 999
  END IF
  POS=POS+6

! Thousandths

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
    IF (QUAD == 5 .OR. QUAD == 7) LONG=-LONG
  ELSE
    PRINT*, ' TESSC1: INVALID LONG >',REPORT,'<'
    IERR=16
    GOTO 999
  END IF
  POS=POS+7
END IF IFLABEL2

! Wind group is ddff preceded by an indicator from 0 to 3 (odd if knots)

IU=IVALUE(REPORT(POS:POS))
IF (IU >= 0 .AND. IU <= 3) THEN
  IF (IU == 1 .OR. IU == 3) KNOTS=.TRUE.
  WDIR=IVALUE(REPORT(POS+1:POS+2))
  WSPEED=IVALUE(REPORT(POS+3:POS+4))
  IF (WDIR /= RMISS) WDIR=WDIR*10.
  IF (KNOTS .AND. WSPEED /= RMISS) WSPEED=WSPEED*KTS2MPS
  POS=POS+6
END IF

! 4-group is temperature (second figure sign) in tenths Celsius

IF (REPORT(POS:POS) == '4') THEN
  SIGN=IVALUE(REPORT(POS+1:POS+1))
  TEMP=IVALUE(REPORT(POS+2:POS+4))
  IF (TEMP /= RMISS) THEN
    TEMP=(TEMP*0.1)+273.1
    IF (SIGN == 1) TEMP=-TEMP
  END IF
  POS=POS+6
END IF

999   CONTINUE

! Put values in output arrays

EXPARR(6) =YEAR
EXPARR(8) =MONTH
EXPARR(10)=DAY
EXPARR(12)=HOUR
EXPARR(14)=MIN
EXPARR(16)=LAT
EXPARR(18)=LONG
EXPARR(20)=WDIR
EXPARR(22)=WSPEED
EXPARR(24)=TEMP

DATIME(1)=YEAR
DATIME(2)=MONTH
DATIME(3)=DAY
DATIME(4)=HOUR
DATIME(5)=MIN

RETURN
END SUBROUTINE TESSC1
