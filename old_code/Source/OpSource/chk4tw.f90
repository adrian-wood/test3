SUBROUTINE CHK4TW(IDESC,IREPL,NDES,WANTEMP,WANTWND,USRLEVT, &
USRLEVW,LFLAG)

!-----------------------------------------------------------------------
!
! subroutine    : CHK4TW
!
!               : ANSI standard except for IMPLICIT NONE and '!' used
!               : for comments
!
! purpose       : To look through the users requested upper-air elements
!               : to see if they want temperatures and/or winds, and if
!               : so how many levels they have allowed for.
!
! description   : This routine uses the element numbers as defined in
!               : the data dictionary for temperature, dew-point, wind
!               : direction and wind speed.
!
! data type(s)  : TEMP, PILOT, DROPSOND
!
! called by     : UPRRET
!
! sub calls     : None
!
! arguments     : IDESC    (ip)  : array of element numbers requested
!               : IREPL    (ip)  : levels corresponding to IDESC
!               : NDES     (ip)  : no of elements requested
!               : WANTEMP  (op)  : true if temperatures wanted
!               : WANTWND  (op)  : True if winds wanted
!               : USRLEVT  (op)  : max no of temp levels allowed for
!               : USRLEVW  (op)  : max no of wind levels allowed for
!               : LFLAG    (ip)  : true for diagnostics
!
!Y2K  26.06.1997  CHK4TW is Year 2000 compliant.
!
! revision info :
!
! $Workfile: chk4tw.f90$ $Folder: OpSource$
!
! $Revision: 1$ $Date: 22/11/2010 09:32:42$
!
! $Log:
!  1    MetDB_Refresh 1.0         22/11/2010 09:32:42    Sheila Needham
!       Renamed from .F90 because it has no pre-processing
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

IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare argument types - variables used as dimensions first
!-----------------------------------------------------------------------

INTEGER,INTENT(IN)   ::  NDES

INTEGER,INTENT(IN)   ::  IDESC(NDES)
INTEGER,INTENT(IN)   ::  IREPL(NDES)
INTEGER,INTENT(OUT)  ::  USRLEVT
INTEGER,INTENT(OUT)  ::  USRLEVW
LOGICAL,INTENT(IN)   ::  LFLAG
LOGICAL,INTENT(OUT)  ::  WANTEMP
LOGICAL,INTENT(OUT)  ::  WANTWND

!-----------------------------------------------------------------------
! Declare local variables
!-----------------------------------------------------------------------

INTEGER  ::   J


!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------


WANTEMP=.FALSE.
WANTWND=.FALSE.
USRLEVT=0
USRLEVW=0

DO J=1,NDES
  IF(IDESC(J) == 24.OR.IDESC(J) == 25)THEN
    WANTEMP=.TRUE.
    IF(IREPL(J) > USRLEVT) USRLEVT=IREPL(J)
  ELSE IF(IDESC(J) == 26.OR.IDESC(J) == 27)THEN
    WANTWND=.TRUE.
    IF(IREPL(J) > USRLEVW) USRLEVW=IREPL(J)
  END IF
END DO

IF(LFLAG)THEN
  PRINT*,'IN CHK4TW: WANTEMP =',WANTEMP,' LEVELS=',USRLEVT
  PRINT*,'           WANTWND =',WANTWND,' LEVELS=',USRLEVW
END IF

RETURN
END SUBROUTINE CHK4TW
