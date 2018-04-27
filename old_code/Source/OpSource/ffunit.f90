SUBROUTINE FFUNIT(WMOBLK,WMOSTN,IW)

!-----------------------------------------------------------------------
!
! PROGRAM       : FFUNIT
!
! PURPOSE       : SET WIND REPORTING UNITS FOR REPORTS WITH NO YYGGIW
!
! DESCRIPTION   : ASSUME BLOCKS 11 (STATIONS>400), 12, 15 AND 20-39
!                 REPORT IN M/S, THE REST IN KNOTS.
!
! CALLED BY     : SYNEXP
!
! ARGUMENTS     : (1) WMO BLOCK NUMBER                            (I)
!                 (2) WMO STATION NUMBER                          (I)
!                 (3) IW SET TO 0 FOR M/S, 3 FOR KNOTS            (O)
!
!
! REVISION INFO :
!
!
! $Workfile: ffunit.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 04/01/2011 16:36:47$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         04/01/2011 16:36:47    Rosemary Lavery
!       Initial Import
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

! Interface Arguments

INTEGER, INTENT(IN)   :: WMOBLK      ! (A1)
INTEGER, INTENT(IN)   :: WMOSTN      ! (A2)
INTEGER, INTENT(OUT)  :: IW          ! (A3)

! Local Variables

!-------------------------------------------------------------------

IF(WMOBLK == 11) THEN
  IF(WMOSTN < 400) THEN    ! AUSTRIA: KNOTS
    IW=3
  ELSE                     ! CZECH REPUBLIC & SLOVAKIA: M/S
    IW=0
  END IF
ELSE IF(WMOBLK == 12) THEN ! POLAND: M/S
  IW=0
ELSE IF(WMOBLK == 15) THEN ! ROMANIA: M/S
  IW=0
ELSE IF(WMOBLK < 20) THEN
  IW=3
ELSE IF(WMOBLK > 39) THEN
  IW=3
ELSE
  IW=0                     ! 'FORMER USSR': M/S
END IF

RETURN
END SUBROUTINE FFUNIT
