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
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/valarea.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.2  2003/03/31 08:53:03  usmdb
! Add a tolerance to the observation position check to allow for
! observations on the edge of a user area that could be rejected
! due to rounding errors (generated by BUFR retrieval on T3E) S.Cox.
!
! Revision 2.1  2002/01/15  15:17:05  15:17:05  usmdb
! 2.1.  21 January 2002.  Brian Barwell.  Change 163/01.
! Modify lat/long checks to allow for dateline crossing.
! Also checks for first call to subroutine.
!
! Revision 2.0  2001/01/08  11:59:25  11:59:25  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  16:30:56  16:30:56  uspm (Pat McCormack)
! Correct position of continuation character in definition of HEAD
!
! Revision 1.1  1997/08/04 13:55:32  uspm
! Initial revision
!
! Created       : 9th May 1997 : J.Lewthwaite
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

!-----------------------------------------------------------------------
! Parameter statements
!-----------------------------------------------------------------------

 REAL       TOL             !- Tolerance used when checking   !2.2
 PARAMETER (TOL=0.000001)   !- observation position.          !2.2

!-----------------------------------------------------------------------
! Declare variables
!-----------------------------------------------------------------------

 REAL    AREA(5)           !- Users requested Lat/Long box
 REAL    LAT               !- Latitude from observation
 REAL    LONG              !- Longitude from observation

 LOGICAL AREA_FLAG         !- Result flag from lat/long check
 LOGICAL FIRST             !- .TRUE. for first call to VALAREA
 LOGICAL LFLAG             !- Diagnostic testing flag

 CHARACTER HEAD*132        !- Revision information

!-----------------------------------------------------------------------
! Data initialisation and saved variables.
!-----------------------------------------------------------------------

 DATA FIRST /.TRUE./                                          !2.1
 SAVE FIRST                                                   !2.1

 IF (FIRST) THEN                                              !2.1
   HEAD='$RCSfile: valarea.f,v $ ' //&
       &'$Date: 26/01/2010 10:18:13$ $Revision: 1$'
   FIRST = .FALSE.                                            !2.1
 END IF                                                       !2.1

 IF (LFLAG) THEN
   WRITE(*,*)'In VALAREA: Lat,Long     : ',LAT,LONG
   WRITE(*,*)'In VALAREA: Lat/Long Box : ',AREA
 ENDIF

!-----------------------------------------------------------------------
! Check ob is on the globe.
!-----------------------------------------------------------------------

 IF (ABS(LAT).GT.(90.0+TOL).OR.ABS(LONG).GT.(180.0+TOL)) THEN !2.2
   IF (LFLAG) WRITE(*,*)'In VALAREA: Bad observation lat/lon'
   AREA_FLAG = .FALSE.

!-----------------------------------------------------------------------
! Compare latitude of observation with latitude range of user's box.
!-----------------------------------------------------------------------

!               Ob. too far N   or  Ob. too far S                   !2.1
 ELSEIF (LAT.GT.(AREA(2)+TOL) .OR. LAT.LT.(AREA(4)-TOL)) THEN !2.2
   AREA_FLAG = .FALSE.

!-----------------------------------------------------------------------
! Compare longitude of observation with longitude range of user's box.
!-----------------------------------------------------------------------

!                                  (1)  AREA doesn't cross the dateline
!                West < or = East                                   !2.1
 ELSE IF (AREA(3).LE.AREA(5)) THEN                            !2.1
   AREA_FLAG = LONG.GE.(AREA(3)-TOL) .AND.&                   !2.2
              &LONG.LE.(AREA(5)+TOL)                          !2.2

!                                        (2)  AREA crosses the dateline
 ELSE                                                         !2.1
   AREA_FLAG = LONG.GE.(AREA(3)-TOL) .OR.&                    !2.2
              &LONG.LE.(AREA(5)+TOL)                          !2.2
 END IF

!-----------------------------------------------------------------------
! Printout if diagnostics requested.
!-----------------------------------------------------------------------

 IF (LFLAG) THEN                                              !2.1
   IF (AREA_FLAG) THEN                                        !2.1
     WRITE(*,*) 'In VALAREA: Ob is in requested area'         !2.1
   ELSE                                                       !2.1
     WRITE(*,*) 'In VALAREA: Ob is not in requested area'     !2.1
   ENDIF                                                      !2.1
 ENDIF                                                        !2.1

 RETURN
 END SUBROUTINE VALAREA
