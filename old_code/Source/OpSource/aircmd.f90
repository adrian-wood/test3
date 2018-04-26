SUBROUTINE AIRCMD(OLD_LAT,OLD_LONG,LAT,LONG,                  &
  MID_LAT,MID_LONG,TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,          &
  REQ_YY,REQ_MTH,REQ_DD,REQ_HH,OLD_MINS,MID_YY,MID_MTH,MID_DD,  &
  MID_HH,MID_MM)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRCMD
!
! PURPOSE       : To calculate lat/lon of mid point data in aireps
!
! DESCRIPTION   : Using current position of aircraft and previous
!                 position (from airidx) a calculation is performed to
!                 obtain the lat/long of the mid point report and the
!                 estimated time of that report
!
! CALLED BY     : AIRIDX
!
! CALLS         : DATE13,DATE31
!
! ARGUMENTS     : OLD_LAT  - Previous position              (i)
!               : OLD_LONG - previous position              (i)
!                 LAT      - current position               (i)
!                 LONG     - current position               (i)
!                 MID_LAT  - calculated mid point position  (o)
!                 MID_LONG - calculated mid point position  (o)
!                 TIMEYY   - date/time of current report    (i)
!                 TIMETH                                    (i)
!                 TIMEDD                                    (i)
!                 TIMEHH                                    (i)
!                 TIMEMM                                    (i)
!                 REQ_YY   - date/time of previous report   (i)
!                 REQ_MTH                                   (i)
!                 REQ_DD                                    (i)
!                 REQ_HH                                    (i)
!                 OLD_MINS - previous minutes (?)           (i)
!                 MID_YY   - calculated mid time            (o)
!                 MID_MTH                                   (o)
!                 MID_DD                                    (o)
!                 MID_HH                                    (o)
!                 MID_MM                                    (o)
!
! REVISION INFO :
!
! $Workfile: aircmd.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 12/01/2011 16:43:35$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         12/01/2011 16:43:35    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         11/01/2011 10:43:12    Sheila Needham
!       Initial F77 version
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

! Interfaces
USE zpdate_mod

IMPLICIT NONE

! Arguments

REAL,INTENT(IN)       :: OLD_LAT   !(1)
REAL,INTENT(IN)       :: OLD_LONG  !(2)
REAL,INTENT(IN)       :: LAT       !(3)
REAL,INTENT(IN)       :: LONG      !(4)
REAL,INTENT(OUT)      :: MID_LAT   !(5)
REAL,INTENT(OUT)      :: MID_LONG  !(6)
INTEGER,INTENT(IN)    :: TIMEYY    !(7)
INTEGER,INTENT(IN)    :: TIMETH    !(8)
REAL,INTENT(IN)       :: TIMEDD    !(9)
REAL,INTENT(IN)       :: TIMEHH    !(10)
REAL,INTENT(IN)       :: TIMEMM    !(11)
INTEGER,INTENT(IN)    :: REQ_YY    !(12)
INTEGER,INTENT(IN)    :: REQ_MTH   !(13)
INTEGER,INTENT(IN)    :: REQ_DD    !(14)
INTEGER,INTENT(IN)    :: REQ_HH    !(15)
INTEGER,INTENT(IN)    :: OLD_MINS  !(16)
REAL,INTENT(OUT)      :: MID_YY    !(17)
REAL,INTENT(OUT)      :: MID_MTH   !(18)
REAL,INTENT(OUT)      :: MID_DD    !(19)
REAL,INTENT(OUT)      :: MID_HH    !(20)
REAL,INTENT(OUT)      :: MID_MM    !(21)

! Local Variables

INTEGER :: CENDAY
INTEGER :: ID
INTEGER :: IM
INTEGER :: IY
INTEGER :: MID_MINS
INTEGER :: MINS_NOW
INTEGER :: MINS_OLD
INTEGER :: NEW_HH
INTEGER :: NEW_MM

! Initialise

CENDAY=0
MID_MINS=0.0
ID=0
IM=0
IY=0
NEW_MM=0
NEW_HH=0

MID_LAT=(LAT+OLD_LAT)/2
MID_LONG=(LONG+OLD_LONG)/2

! If the signs of the longitudes are different and the midpoint
! must be nearer to 180 than 0, go round 180 degrees from the
! mean calculated above to a value in the range -180 to +180.

IF (((LONG > 0 .AND. OLD_LONG < 0) .OR.    &
     (LONG < 0 .AND. OLD_LONG > 0)) .AND.    &
      ABS(LONG)+ABS(OLD_LONG) > 180.) THEN
  IF (MID_LONG > 0) THEN
    MID_LONG=MID_LONG-180.
  ELSE
    MID_LONG=MID_LONG+180.
  END IF
END IF

! This section calculates the MID point time. The current and previous
! times are converted into minutes and the calculation is performed
! from here.

! Calculate time of current report as century-minute

CALL DATE31(INT(TIMEDD),TIMETH,TIMEYY,CENDAY)
MINS_NOW=((CENDAY-1)*1440)+(INT(TIMEHH)*60)+INT(TIMEMM)

! Calculate time of previous observation as century-minute

CALL DATE31(REQ_DD,REQ_MTH,REQ_YY,CENDAY)
MINS_OLD=((CENDAY-1)*1440)+(INT(REQ_HH)*60)+OLD_MINS

! Calculate the mid point time from the two century-minutes

MID_MINS=(MINS_NOW+MINS_OLD)/2

! Convert the mid point time form minutes back into year month day
! hours and minutes

CENDAY=MID_MINS/1440+1
CALL DATE13(CENDAY,ID,IM,IY)
MID_MINS=MOD(MID_MINS,1440)
MID_YY=FLOAT(IY)
MID_MTH=FLOAT(IM)
MID_DD=FLOAT(ID)
MID_HH=MID_MINS/60
MID_MM=MOD(MID_MINS,60)

RETURN
END SUBROUTINE AIRCMD
