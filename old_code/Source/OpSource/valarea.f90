SUBROUTINE VALAREA(LAT,LONG,AREA,AREA_FLAG,LFLAG)

!-----------------------------------------------------------------------
!
! ROUTINE       : VALAREA
!
! PURPOSE       : To validate a lat long lies within a lat/long box
!
! DESCRIPTION   : The lat/long of the observation are compared against
!               : the users requested lat/long box. If the observation
!               : lat and/or long lie outside the requested area then
!               : the flag AREA_FLAG is set to .FALSE. If the lat and
!               : long of the observation lie within the box then
!               : AREA_FLAG is set to .TRUE.
!
! CALLED BY     : VALARR, TFMRET, SYNRET, UPRRET
!
! CALLS         : none
!
! ARGUMENTS     : LAT       - i/p, Real    Latitude from observation
!               : LONG      - i/p, Real    Longitude from observation
!               : AREA(5)   - i/p, Real    Area of users requested area
!               : AREA_FLAG - o/p, Logical True if lat/long in box
!               : LFLAG     - i/p, Logical True for diagnostics
!
!                           : AREA(1) - always set to zero in this case
!                           : AREA(2) - northernmost latitude
!                           : AREA(3) - westernmost longitude
!                           : AREA(4) - southernmost latitude
!                           : AREA(5) - easternmost longitude
!
! REVISION INFO:
!
! $Workfile: valarea.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 17/11/2010 09:58:48$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         17/11/2010 09:58:48    John Norton
!       Updated after doing rework for batch 13.
!  1    MetDB_Refresh 1.0         09/11/2010 11:35:01    John Norton     MetDB
!       Refresh created file  
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
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

 REAL,         INTENT(IN)    ::  LAT !- Latitude from observation
 REAL,         INTENT(IN)    ::  LONG !- Longitude from observation
 REAL,         INTENT(IN)    ::  AREA(5) !- Users requested Lat/Long box
 LOGICAL,      INTENT(OUT)   ::  AREA_FLAG !- Result flag from lat/long check
 LOGICAL,      INTENT(IN)    ::  LFLAG !- Diagnostic testing flag

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! Parameter statements
!-----------------------------------------------------------------------

 REAL,        PARAMETER ::  TOL = 0.000001 !- Tolerance used when checking
                                           !- observation position.

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Data initialisation and saved variables.
!-----------------------------------------------------------------------

 IF (LFLAG) THEN
   WRITE(*,*)'In VALAREA: Lat,Long     : ',LAT,LONG
   WRITE(*,*)'In VALAREA: Lat/Long Box : ',AREA
 END IF

!-----------------------------------------------------------------------
! Check ob is on the globe.
!-----------------------------------------------------------------------

IFLABEL1: &
 IF (ABS(LAT) > (90.0+TOL).OR.ABS(LONG) > (180.0+TOL)) THEN
   IF (LFLAG) WRITE(*,*)'In VALAREA: Bad observation lat/lon'
   AREA_FLAG = .FALSE.

!-----------------------------------------------------------------------
! Compare latitude of observation with latitude range of user's box.
!-----------------------------------------------------------------------

!               Ob. too far N   or  Ob. too far S
 ELSE IF (LAT > (AREA(2)+TOL) .OR. LAT < (AREA(4)-TOL)) THEN
   AREA_FLAG = .FALSE.

!-----------------------------------------------------------------------
! Compare longitude of observation with longitude range of user's box.
!-----------------------------------------------------------------------

!                                  (1)  AREA doesn't cross the dateline
!                West < or = East
 ELSE IF (AREA(3) <= AREA(5)) THEN
   AREA_FLAG = LONG >= (AREA(3)-TOL) .AND. &
               LONG <= (AREA(5)+TOL)

!                                        (2)  AREA crosses the dateline
 ELSE
   AREA_FLAG = LONG >= (AREA(3)-TOL) .OR.  &
               LONG <= (AREA(5)+TOL)
 END IF IFLABEL1

!-----------------------------------------------------------------------
! Printout if diagnostics requested.
!-----------------------------------------------------------------------

 IF (LFLAG) THEN
   IF (AREA_FLAG) THEN
     WRITE(*,*) 'In VALAREA: Ob is in requested area'
   ELSE
     WRITE(*,*) 'In VALAREA: Ob is not in requested area'
   END IF
 END IF

 RETURN
END SUBROUTINE VALAREA
