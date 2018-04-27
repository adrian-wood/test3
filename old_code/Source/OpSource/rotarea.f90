SUBROUTINE ROTAREA(LAT,LON,RLAT,RLON,RPOLE)

!-----------------------------------------------------------------------
!
! subroutine    : ROTAREA
!
! purpose       : To calculate latitude and longitude of an observation
!               : if placed on a grid where the pole is has been moved
!
! description   : The transformation formula are described in the
!               : unified model on-line documentation paper s1
!
! arguments     : LAT   - i/p, real value of latitude to be rotated
!               : LON   - i/p, real value of longitude to be rotated
!               : RLAT  - o/p, real value of rotated latitude
!               : RLON  - o/p, real value of rotated longitude
!               : RPOLE - i/p, real array of pole coords to use for
!               :              rotation. RPOLE(1) is the latitude and
!               :              RPOLE(2) is the longitude
!
! called by     : VALARR, SYNRET, UPRRET, TFMRET
!
! calls         : None
!
! language      : ANSI standard FORTRAN77 with accepted extensions e.g
!               : IMPLICIT NONE
!
! Year 2000     : Y2K compliant (no date calculations)
!
! revision info :
!
! $Revision: 1$
! $Date: 01/11/2010 15:48:51$
! $Source: /home/us0400/mdb/op/lib/source/RCS/rotarea.F,v $
!
! change record :
!
! $Log:
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
! Revision 2.0  2001/01/08 11:59:07  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  97/08/04  13:49:59  13:49:59  uspm (Pat McCormack)
! Initial revision
!
! 09-06-97      : Written J.Lewthwaite
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

REAL,         INTENT(IN)  ::  LAT !- Latitude to be rotated
REAL,         INTENT(IN)  ::  LON !- Longitude to be rotated
REAL,         INTENT(OUT) ::  RLAT !- Rotated latitude
REAL,         INTENT(OUT) ::  RLON !- Rotated longitude
REAL,         INTENT(IN)  ::  RPOLE(2)

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! Parameter statements
!-----------------------------------------------------------------------

REAL,        PARAMETER ::  PI = 3.1415926535879323846
REAL,        PARAMETER ::  SMALL = 1.0E-6

!-----------------------------------------------------------------------
! Declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

REAL         ::  A_LAMBDA !- Following variables declared and
REAL         ::  A_PHI   !- initialised are expressions or values
REAL         ::  ARG     !- of the rotated coords.
REAL         ::  COS_PHI_POLE
REAL         ::  E_LAMBDA !- used in the calculation
REAL         ::  E_PHI
REAL         ::  LAMBDA_ZERO
REAL         ::  PI_OVER_180
REAL         ::  RECIP_PI_OVER_180
REAL         ::  RPOLE_LAT !- Latitude of rotated pole from rpole
REAL         ::  RPOLE_LON !- Longitude of rotated pole from rpole
REAL         ::  SIN_PHI_POLE
REAL         ::  TERM1
REAL         ::  TERM2

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

PI_OVER_180=PI/180.0
RECIP_PI_OVER_180=180.0/PI
RPOLE_LAT=RPOLE(1)
RPOLE_LON=RPOLE(2)
LAMBDA_ZERO=RPOLE_LON+180.
SIN_PHI_POLE=SIN(PI_OVER_180*RPOLE_LAT)
COS_PHI_POLE=COS(PI_OVER_180*RPOLE_LAT)

!----------------------------------------------------------------------
! Check latitude and longitude values are sensible.
!----------------------------------------------------------------------

IFLABEL1: &
IF (LAT <= 90.0 .AND. LAT >= -90.0 .AND. LON <= 180.0 .AND. &
    LON >= -180.0) THEN

!----------------------------------------------------------------------
! Scale Longitude to range -180 to +180 degrees
!----------------------------------------------------------------------

  A_LAMBDA=LON-LAMBDA_ZERO
  IF (A_LAMBDA  >  180.0) THEN
    A_LAMBDA=A_LAMBDA-360.
  END IF

  IF (A_LAMBDA <=  -180.0) THEN
    A_LAMBDA=A_LAMBDA+360.
  END IF

!----------------------------------------------------------------------
! Convert latitude and longitude to radians
!----------------------------------------------------------------------

  A_LAMBDA=PI_OVER_180*A_LAMBDA
  A_PHI=PI_OVER_180*LAT

!----------------------------------------------------------------------
! Calculate rotated latitude using equation 4.6 in UM documentation
!----------------------------------------------------------------------

  ARG=-COS_PHI_POLE*COS(A_LAMBDA)*COS(A_PHI)+ &
  SIN(A_PHI)*SIN_PHI_POLE

  ARG=MIN(ARG,1.0)
  ARG=MAX(ARG,-1.0)
  E_PHI=ASIN(ARG)
  RLAT=RECIP_PI_OVER_180*E_PHI

!----------------------------------------------------------------------
! Compute rotated longitude using equation 4.6 in UM documentation
!----------------------------------------------------------------------

  TERM1=(COS(A_PHI)*COS(A_LAMBDA)*SIN_PHI_POLE+ &
  SIN(A_PHI)*COS_PHI_POLE)
  TERM2=COS(E_PHI)
  IF (TERM2  <  SMALL) THEN
    E_LAMBDA=0.0
  ELSE
    ARG=TERM1/TERM2
    ARG=MIN(ARG,1.0)
    ARG=MAX(ARG,-1.0)
    E_LAMBDA=RECIP_PI_OVER_180*ACOS(ARG)
    E_LAMBDA=SIGN(E_LAMBDA,A_LAMBDA)
  END IF

!----------------------------------------------------------------------
! Scale Longitude to range 0 to 360 degs
!----------------------------------------------------------------------

  IF (E_LAMBDA >= 360.0) E_LAMBDA=E_LAMBDA-360.0
  IF (E_LAMBDA < 0.0) E_LAMBDA=E_LAMBDA+360.0

!----------------------------------------------------------------------
! We want the longitude in the range -180.0 to 180.0 degs so..
!----------------------------------------------------------------------

  IF (E_LAMBDA > 180.0) E_LAMBDA=E_LAMBDA-360.0

  RLON=E_LAMBDA

ELSE

!----------------------------------------------------------------------
! Bad lat/lon, rotated lat/lon = input lat/lon
!----------------------------------------------------------------------

  RLAT=LAT
  RLON=LON

END IF IFLABEL1

RETURN
END SUBROUTINE ROTAREA
