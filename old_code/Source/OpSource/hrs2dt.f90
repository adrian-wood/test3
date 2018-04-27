SUBROUTINE HRS2DT(IY,IM,ID,IH,ICNTH)

!----------------------------------------------------------------------
!
! SUBROUTINE    : HRS2DT
!
! PURPOSE       : CONVERTS A CENTURY HOUR TO YMDH FORMAT
!
! DESCRIPTION   :
!
! CALLS         : DATE31   IN ZPDATE
!
! ARGUMENTS     : INPUT
!                 -----
!                 IY      INTEGER  YEAR
!                 IM      INTEGER  MONTH
!                 ID      INTEGER  DAY
!                 IH      INTEGER  HOUR
!                 ICNTH   INTEGER  CENTURY HOUR
!
! REVISION INFO:
!
! $Workfile: hrs2dt.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 18/11/2010 11:45:43$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         18/11/2010 11:45:43    John Norton     After
!       doing merge batch 9 changes.
!  3    MetDB_Refresh 1.2         22/10/2010 14:11:14    Brian Barwell   USE
!       zpdate.mod added.
!  2    MetDB_Refresh 1.1         19/10/2010 14:33:03    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!  1    MetDB_Refresh 1.0         12/10/2010 16:23:36    Brian Barwell
!       Initial f77 version before porting to f90/95.
! $
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!----------------------------------------------------------------------
USE zpdate_mod

IMPLICIT NONE

INTEGER IY,IM,ID,IH,ICNTH,ICNTD

! CONVERT TO CENTURY DAY

IH=MOD(ICNTH,24)
ICNTD=(ICNTH-IH)/24+1
CALL DATE13(ICNTD,ID,IM,IY)

RETURN
END SUBROUTINE HRS2DT
