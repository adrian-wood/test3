LOGICAL FUNCTION ARTMCK(DATIME,SYS_YEAR)

!-----------------------------------------------------------------------
!
! PROGRAM       : FUNCTION ARTMCK
!
! PURPOSE       : To check that an AIREP report has a date/time within
!                 the accepted range of +-1 Year and MONTH, DAY, HOUR
!                 and Mins are within the normal maximum range ie.
!                 1-12 for Months etc
!
! CALLED BY     : AIRENC
!
! ARGUMENTS     : 1 DATIME - INTEGER ARRAY(5) Y,M,D,H,Mins - Input
!                 2 SYS_YEAR INTEGER Current System Year   - Input
!                 Returns  .TRUE. if Date/Time valid
!
! REVISION INFO :
!
! $Workfile: artmck.f90$ $Folder: OpSource$
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
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Interfaces (none)

IMPLICIT NONE

! Arguments

INTEGER,INTENT(IN)  :: DATIME(5)
INTEGER,INTENT(IN)  :: SYS_YEAR

ARTMCK=.FALSE.

IF (DATIME(1) >= (SYS_YEAR-1)) THEN
  IF (DATIME(1) <= (SYS_YEAR+1)) THEN
    IF (DATIME(2) >= 1.AND.DATIME(2) <= 12) THEN
      IF (DATIME(3) >= 1.AND.DATIME(3) <= 31) THEN
        IF (DATIME(4) >= 0.AND.DATIME(4) <= 23) THEN
          IF (DATIME(5) >= 0.AND.DATIME(5) <= 59) THEN
            ARTMCK=.TRUE.
          END IF
        END IF
      END IF
    END IF
  END IF
END IF

RETURN
END FUNCTION ARTMCK
