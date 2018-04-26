INTEGER FUNCTION CENTURY (TWOFIG_YEAR)

IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : CENTURY
!
! PURPOSE       : To return "1900" or "2000" for 20th- and 21st-
!                 century dates respectively.
!
! DESCRIPTION   : Uses arbitrary figure of "80" to decide to which
!                 century a 2-figure year belongs.  This was chosen
!                 in 1997 to allow for MDB-archive retrieval up to
!                 5 years before.
!
! CALLED BY     : Many date-handling modules.
!
! CALLS         : None
!
! FUNCTIONS     : None
!
! PARAMETERS    : 2-figure year number.
!
! REVISION INFO :
!
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         22/11/2010 09:31:34    Sheila Needham
!       Renamed from .F90 because it has no pre-processing.
! $
!
! INTRODUCED  : July 1997 by Jim Arnott (CC3)
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

INTEGER,INTENT(IN) ::  TWOFIG_YEAR  ! AS SUPPLIED BY USER ROUTINE..

IF (TWOFIG_YEAR  >  80) THEN

  CENTURY = 1900
ELSE
  CENTURY = 2000
END IF

RETURN
END FUNCTION CENTURY
