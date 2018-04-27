SUBROUTINE AIRSET(LAT,LONG,LAT2,LONG2,MATCH,TIMEYY,TIMEMNTH,&
TIMEDD,TIMEHH,TIMEMM,LEVEL,TEMP,WINDD,WINDS,OPT_TEMP,OPT_WIND,&
OPT_WNDS,OPT_TURB,OPT_ICE,MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,&
ARYFLG,AIR_ARAY1,AIR_ARAY2,INDX_ARAY1,INDX_ARAY2)

!-----------------------------------------------------------------------
!
! PROGRAM       : SUBROUTINE AIRSET
!
! PURPOSE       : TO SET UP ARRAYS FOR BUFR ENCODE ROUTINES
!
! DESCRIPTION   : THE AIREP ELEMENTS ARE PASSED TO THIS ROUTINE WHERE
!                 THEY ARE THEN STORED IN AN ARRAY IN THE ORDER  OF THE
!                 BUFR DESCRIPTORS. THE ARRAY IS THEN PASSED TO
!                 AIRCOD
!
! CALLED BY     : AIRARP
!
! CALLS TO      : NONE
!
!
! REVISION INFO:
!
! $Workfile: airset.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 25/01/2011 17:08:10$
!
! CHANGE RECORD:
!
! $Log:
!  5    MetDB_Refresh 1.4         25/01/2011 17:08:10    Richard Weedon
!       intents amended
!  4    MetDB_Refresh 1.3         25/01/2011 12:52:34    Richard Weedon
!       Revision template amended
!  3    MetDB_Refresh 1.2         25/01/2011 12:49:45    Richard Weedon
!       cosmetic changes made
!  2    MetDB_Refresh 1.1         10/01/2011 16:58:05    Richard Weedon  END
!       SUBROUTINE added to end of code (missed on first revision)
!  1    MetDB_Refresh 1.0         10/01/2011 16:55:30    Richard Weedon
!       initial version. Passed compilation test only.
! $
!
!
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

IMPLICIT NONE

! Arguments
! Integers
INTEGER,INTENT(IN)    ::    MATCH
INTEGER,INTENT(OUT)   ::    ARYFLG
!
! real
REAL,INTENT(IN)       ::    LAT
REAL,INTENT(IN)       ::    LONG
REAL,INTENT(IN)       ::    LAT2
REAL,INTENT(IN)       ::    LONG2
REAL,INTENT(IN)       ::    TIMEDD
REAL,INTENT(IN)       ::    TIMEHH
REAL,INTENT(IN)       ::    TIMEMM
REAL,INTENT(IN)       ::    TIMEMNTH
REAL,INTENT(IN)       ::    TIMEYY
REAL,INTENT(IN)       ::    LEVEL
REAL,INTENT(IN)       ::    TEMP
REAL,INTENT(IN)       ::    WINDD
REAL,INTENT(IN)       ::    WINDS
REAL,INTENT(IN)       ::    OPT_TEMP
REAL,INTENT(IN)       ::    OPT_WIND
REAL,INTENT(IN)       ::    OPT_WNDS
REAL,INTENT(IN)       ::    OPT_TURB
REAL,INTENT(IN)       ::    OPT_ICE
REAL,INTENT(OUT)    ::    AIR_ARAY1(18)
REAL,INTENT(OUT)    ::    AIR_ARAY2(18)
REAL,INTENT(OUT)    ::    INDX_ARAY1(8)
REAL,INTENT(OUT)    ::    INDX_ARAY2(8)
REAL,INTENT(IN)       ::    MID_YY
REAL,INTENT(IN)       ::    MID_MTH
REAL,INTENT(IN)       ::    MID_DD
REAL,INTENT(IN)       ::    MID_HH
REAL,INTENT(IN)       ::    MID_MM

! variables
INTEGER               ::    MID_FLAG
INTEGER               ::    NELM         ! count of good values
!
!declare logical
LOGICAL DEBUG

SAVE

DEBUG=.FALSE.

IF (MATCH  ==  1) THEN
  MID_FLAG=21
END IF

!store the elements in the air_aray
if_constr1 : &
IF (MID_YY  <=  0) THEN
  NELM=0                     ! initialise count of good values
  ARYFLG=1

  AIR_ARAY1(1)=1               !Aircraft id displacement
  AIR_ARAY1(2)=9             !BEACON REPORTING POINT
  AIR_ARAY1(3)=1             !Flag - always obs. for first report
  AIR_ARAY1(4)=TIMEYY          !Year
  AIR_ARAY1(5)=TIMEMNTH        !Month
  AIR_ARAY1(6)=TIMEDD        !Day
  AIR_ARAY1(7)=TIMEHH        !Hour
  AIR_ARAY1(8)=TIMEMM        !Minute
  AIR_ARAY1(9)=1             !FLAG INDICATES OBSERVED LAT/LONG
  AIR_ARAY1(10)=LAT          !Latitude
  AIR_ARAY1(11)=LONG         !Longitude
  AIR_ARAY1(12)=1            !FLAG INDICATES OBSERVED LAT/LONG
  AIR_ARAY1(13)=LEVEL         !Flight level
  AIR_ARAY1(14)=WINDD        !Wind direction
  AIR_ARAY1(15)=WINDS        !Wind speed
  AIR_ARAY1(16)=OPT_TURB     !Turbulence
  AIR_ARAY1(17)=TEMP         !Air temperature
  AIR_ARAY1(18)=OPT_ICE      !Icing

  IF (WINDD /= -9999999.) NELM=NELM+1
  IF (WINDS /= -9999999.) NELM=NELM+1
  IF (OPT_TURB /= -9999999.) NELM=NELM+1
  IF (TEMP /= -9999999.) NELM=NELM+1
  IF (OPT_ICE /= -9999999.) NELM=NELM+1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!set up array for index entry                                        !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INDX_ARAY1(1)=TIMEYY        !CALCULATED IN AIRTIM
  INDX_ARAY1(2)=TIMEMNTH
  INDX_ARAY1(3)=TIMEDD
  INDX_ARAY1(4)=TIMEHH
  INDX_ARAY1(5)=TIMEMM
  INDX_ARAY1(6)=LAT
  INDX_ARAY1(7)=LONG
  INDX_ARAY1(8)=NELM
ELSE if_constr1
  NELM=0                     ! initialise count of good values
  ARYFLG=2

  AIR_ARAY1(1)=1               !Aircraft id displacement
  AIR_ARAY1(2)=9             !BEACON REPORTING POINT
  AIR_ARAY1(3)=1             !Flag - always obs. for first report
  AIR_ARAY1(4)=TIMEYY          !Year
  AIR_ARAY1(5)=TIMEMNTH        !Month
  AIR_ARAY1(6)=TIMEDD        !Day
  AIR_ARAY1(7)=TIMEHH        !Hour
  AIR_ARAY1(8)=TIMEMM        !Minute
  AIR_ARAY1(9)=1             !FLAG INDICATES OBSERVED LAT/LONG
  AIR_ARAY1(10)=LAT          !Latitude
  AIR_ARAY1(11)=LONG         !Longitude
  AIR_ARAY1(12)=1            !FLAG INDICATES OBSERVED LAT/LONG
  AIR_ARAY1(13)=LEVEL         !Flight level
  AIR_ARAY1(14)=WINDD        !Wind direction
  AIR_ARAY1(15)=WINDS        !Wind speed
  AIR_ARAY1(16)=OPT_TURB     !Turbulence
  AIR_ARAY1(17)=TEMP         !Air temperature
  AIR_ARAY1(18)=OPT_ICE      !Icing

  IF (WINDD /= -9999999.) NELM=NELM+1
  IF (WINDS /= -9999999.) NELM=NELM+1
  IF (OPT_TURB /= -9999999.) NELM=NELM+1
  IF (TEMP /= -9999999.) NELM=NELM+1
  IF (OPT_ICE /= -9999999.) NELM=NELM+1
  INDX_ARAY1(8)=NELM

  NELM=0                     !initialise count of good values
  AIR_ARAY2(1)=1             !Aircraft id displacement
  AIR_ARAY2(2)=9             !BECAON ID
  AIR_ARAY2(3)=21            !TIME SIGINIFICANCE FLAG
  AIR_ARAY2(4)=MID_YY        !Year
  AIR_ARAY2(5)=MID_MTH       !Month
  AIR_ARAY2(6)=MID_DD        !Day
  AIR_ARAY2(7)=MID_HH        !Hour
  AIR_ARAY2(8)=MID_MM        !Minute
  AIR_ARAY2(9)=MID_FLAG      !FLAG INDICATES OBSERVED LAT/LONG
  AIR_ARAY2(10)=LAT2         !Latitude
  AIR_ARAY2(11)=LONG2        !Longitude
  AIR_ARAY2(12)=1            !FLAG INDICATES OBSERVED LAT/LONG
  AIR_ARAY2(13)=LEVEL        !Flight level
  AIR_ARAY2(14)=OPT_WIND     !Wind direction
  AIR_ARAY2(15)=OPT_WNDS     !Wind speed
  AIR_ARAY2(16)=-9999999.    !Turbulence
  AIR_ARAY2(17)=OPT_TEMP     !Air temperature
  AIR_ARAY2(18)=-9999999.    !Icing

  IF (OPT_WIND /= -9999999.) NELM=NELM+1
  IF (OPT_WNDS /= -9999999.) NELM=NELM+1
  IF (OPT_TEMP /= -9999999.) NELM=NELM+1
  INDX_ARAY2(8)=NELM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!set up array for index entry (NELM already in INDX_ARAY(8)          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INDX_ARAY1(1)=TIMEYY
  INDX_ARAY1(2)=TIMEMNTH
  INDX_ARAY1(3)=TIMEDD
  INDX_ARAY1(4)=TIMEHH
  INDX_ARAY1(5)=TIMEMM
  INDX_ARAY1(6)=LAT
  INDX_ARAY1(7)=LONG

  INDX_ARAY2(1)=MID_YY
  INDX_ARAY2(2)=MID_MTH
  INDX_ARAY2(3)=MID_DD
  INDX_ARAY2(4)=MID_HH
  INDX_ARAY2(5)=MID_MM
  INDX_ARAY2(6)=LAT2
  INDX_ARAY2(7)=LONG2
END IF if_constr1

RETURN
END SUBROUTINE AIRSET
