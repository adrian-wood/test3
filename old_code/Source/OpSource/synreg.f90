SUBROUTINE SYNREG(BLOCK,STNNUM,WMOREG)

!-----------------------------------------------------------------------
!
! PROGRAM       : SYNREG
!
! PURPOSE       : TO RETURN WMO REGION GIVEN WMO BLOCK AND STATION
!                 NUMBER.
!
! CALLED BY     : SYNEXP
!
! CALLS         : NONE
!
! ARGUMENTS     : (1)  WMO BLOCK
!                 (2)  WMO STATION NUMBER
!                 (3)  WMO REGION
!                           ANTARCTICA = REGION 0
!                           TOO HIGH   = -32768
!                           TOO LOW    = -32768
!
! REVISION INFO :
!
!
! $Workfile: synreg.f90$ $Folder: OpSource$
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
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE METDB_COM_mod, ONLY : MISSIN   ! missing data value

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(IN)  :: BLOCK    ! (A1)
INTEGER, INTENT(IN)  :: STNNUM   ! (A2)
INTEGER, INTENT(OUT) :: WMOREG   ! (A3)

! Local Variables

INTEGER  :: ISTN       ! WMO station id

! ---------------------------------------------------------------------

ISTN=BLOCK*1000+STNNUM

IFBLOCK1: &
IF(ISTN.LT.00000)THEN
   WMOREG = MISSIN
ELSE IF(ISTN.LT.20000)THEN
   WMOREG = 6
ELSE IF(ISTN.LT.33000)THEN
  IF(ISTN.LT.20100)THEN
     WMOREG = 2
  ELSE IF(ISTN.LT.20200)THEN
     WMOREG = 6
  ELSE IF(ISTN.LT.22000)THEN
     WMOREG = 2
  ELSE IF(ISTN.LT.23000)THEN
     WMOREG = 6
  ELSE IF(ISTN.LT.26000)THEN
     WMOREG = 2
  ELSE IF(ISTN.LT.28000)THEN
     WMOREG = 6
  ELSE
     WMOREG = 2
  END IF
ELSE IF(ISTN.LT.60000)THEN
  IF(ISTN.LT.35000)THEN
     WMOREG = 6
  ELSE IF(ISTN.LT.37000)THEN
     WMOREG = 2
  ELSE IF(ISTN.LT.38000)THEN
     WMOREG = 6
  ELSE IF(ISTN.LT.40000)THEN
     WMOREG = 2
  ELSE IF(ISTN.LT.40350)THEN
     WMOREG = 6
  ELSE IF(ISTN.LT.48600)THEN
     WMOREG = 2
  ELSE IF(ISTN.LT.48800)THEN
     WMOREG = 5
  ELSE
     WMOREG = 2
  END IF
ELSE IF(ISTN.LT.70000)THEN
   WMOREG = 1
ELSE IF(ISTN.LT.80000)THEN
   WMOREG = 4
ELSE IF(ISTN.LT.89000)THEN
  IF(ISTN.EQ.88963.OR.ISTN.EQ.88968)THEN
     WMOREG = 0
  ELSE
     WMOREG = 3
  END IF
ELSE IF(ISTN.LT.90000)THEN
   WMOREG = 0
ELSE IF(ISTN.LT.99000)THEN
   WMOREG = 5
ELSE
   WMOREG = MISSIN
END IF IFBLOCK1

RETURN
END SUBROUTINE SYNREG
