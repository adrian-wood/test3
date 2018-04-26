SUBROUTINE SYNRPR(HOUR,REGION,BLOCK,STNNUM,PERIOD)

!-----------------------------------------------------------------------
!
! PROGRAM       : SYNRPR
!
! PURPOSE       : TO DECIDE THE PERIOD WITH ZERO RAINFALL WHEN IR=3
!                 IN A SYNOP.  THE RAINFALL PERIOD FOR REPORTS AT A
!                 GIVEN HOUR VARIES FROM REGION TO REGION (& WITHIN
!                 REGIONS), SO THE PERIODS BELOW ARE BASED ON WORK
!                 BY RUSSELL WATKINS IN 1993.  THE PERIOD WITH NO
!                 RAINFALL MAY BE REPORTED EXPLICITLY IN THE FUTURE.
!
! CALLED BY     : SYNEXP
!
! ARGUMENTS     : (1) HOUR    HOUR OF OBSERVATION.                  (I)
!                 (2) REGION  WMO REGION OF STATION.                (I)
!                 (3) BLOCK   WMO BLOCK Number of Station, 2 digits (I)
!                 (4) STNNUM  WMO Station Number, 3 digits          (I)
!                 (5) PERIOD  PERIOD OF NO RAIN.                    (O)
!
! REVISION INFO :
!
!
! $Workfile: synrpr.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 11/01/2011 11:54:02$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         11/01/2011 11:54:02    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         10/01/2011 14:51:06    Rosemary Lavery
!       corrections after review
!  1    MetDB_Refresh 1.0         04/01/2011 16:36:47    Rosemary Lavery
!       Initial Import
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

USE METDB_COM_mod, ONLY : RMISS   ! missing data value

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(IN)  :: HOUR
INTEGER, INTENT(IN)  :: REGION
INTEGER, INTENT(IN)  :: BLOCK    ! WMO Block number
INTEGER, INTENT(IN)  :: STNNUM   ! Station number
REAL, INTENT(OUT)    :: PERIOD

! ---------------------------------------------------------------------

! Initialize return item
PERIOD = RMISS

!    At 00z the Asian Region 2 (except for China which has a default
!    rainfall period of 6hrs) and the former USSR countries
!    have a default rainfall period of 12 hrs

IF_OBHOUR: &
IF (HOUR == 0) THEN

IF_AREA1: &
  IF ((REGION == 2 .AND.                    &     ! Asia
  .NOT.(BLOCK >= 50 .AND. BLOCK <= 59)).OR. &     ! Not China
  (BLOCK >= 20 .AND. BLOCK <= 38)) THEN           ! Former USSR
    PERIOD= -12.

!    At 00z The Antartic and Region 5 have a default of 24 hrs except
!    for Australia and New Zealand who due to their practice of
!    reporting rainfall  either side of hour are dealt with
!    at the end of the routine.
  ELSE IF (BLOCK == 89 .OR.                 &     ! Antarctic
   (REGION == 5 .AND.(BLOCK == 91 .OR.      &     ! Region 5 non-
    BLOCK >= 96))) THEN                           ! Australian/
                                                  ! New Zealand
    PERIOD= -24.

!    At 00z the default Rainfall Period for everyone else is 6 hours
  ELSE                                            ! Everyone else
    PERIOD= -6.
  END IF IF_AREA1
!
ELSE IF (HOUR == 6) THEN

!    At hour 6 Europe Reports a 12 hour Rainfall Period, Africa
!    reports a 24 hour Period. The default for the rest of the world
!    is 6 hours.
  IF (REGION == 6.OR.  &                          ! Europe or
      (BLOCK >= 20.AND.BLOCK <= 38)) THEN         ! Former USSR
    PERIOD= -12.
  ELSE IF (REGION == 1) THEN                      ! Africa
    PERIOD= -24.
  ELSE
    PERIOD= -6.
  END IF

!    At 9Z Iceland reports a 15 hour period
ELSE IF ( HOUR == 9 .AND.                     &   ! 09Z
         BLOCK == 4 .AND. STNNUM >= 000 .AND. &   ! Iceland
         STNNUM <= 199) THEN
  PERIOD = -15.
!
ELSE IF (HOUR == 12) THEN

!    At 12z South America has a 24 hour default Rainfall Period.
IF_AREA2: &
  IF (REGION == 3) THEN                           ! South America
    PERIOD = -24.

!    At 12z Antarctic and Asia (except for China) and the former
!    USSR countries have a 12 hour default Rainfall Period
  ELSE IF (BLOCK == 89 .OR.                   &   ! Antarctic
          (BLOCK >= 20.AND.BLOCK <= 39).OR.   &   ! Former USSR
          (REGION == 2 .AND.                  &   ! Asia but
  .NOT.(BLOCK >= 50 .AND. BLOCK <= 59))) THEN     ! Not China
    PERIOD = -12.

!    At 12z the rest of the world has a default Rainfall Period of
!    6 hours.
  ELSE                                            ! Everyone else
    PERIOD= -6.
  END IF IF_AREA2
!
ELSE IF (HOUR == 18) THEN

!    At 18z Iceland has a default Rainfall Period of 9 hours

IF_AREA3: &
  IF (BLOCK == 4 .AND. STNNUM >= 000 .AND. &      ! Iceland
                       STNNUM <= 199) THEN
    PERIOD= -9.

!    At 18z the Antartic has a default rainfall period of 18 hours
  ELSE IF (BLOCK == 89) THEN                      ! Antarctic
    PERIOD= -18.

!    At 18z Africa, Former USSR and Europe have a default Rainfall
!    period of 12 hours
  ELSE IF (REGION == 1 .OR.                &      ! Africa
          (BLOCK >= 20 .AND. BLOCK <= 38)  &      ! Former USSR
      .OR. REGION == 6) THEN                      ! Europe
    PERIOD= -12.

!    At 18z everyone else in world has a default rainfall Period
!    of 6 hours
  ELSE
    PERIOD= -6.
  END IF IF_AREA3
END IF IF_OBHOUR

!    For Australia and New Zealand rainfall could be reported at
!    at an hour before or after the main hour
IF (REGION == 5 .AND.                    &        ! Region 5
    BLOCK >= 92 .AND. BLOCK <= 95) THEN           ! Aus.& NZ
  IF (HOUR == 0 .OR. HOUR == 23 .OR.     &        ! 00Z ob
       HOUR == 1) THEN
    PERIOD = -24.
  ELSE                                            ! all other times
    PERIOD = -6.
  END IF
END IF

RETURN
END SUBROUTINE SYNRPR
