SUBROUTINE UPREPID(Byte17,RprtId)

!-----------------------------------------------------------------------
!
! subroutine    : UPRREPID
!
!               : ANSI standard except for IMPLICIT NONE and '!' used
!               : for comments
!
! purpose       : MetDB Upper-Air retrieval. Used to set the RPRT_IDNY
!               : to put in the user's array.
!
! data type(s)  : TEMP, PILOT, DROPSOND
!
! called by     : UPRRET
!
! sub calls     : None
!
! arguments     :
!
! Byte17        : character (ip) : index entry byte 17
! RprtId        : integer   (op) : report id
!
!Y2K  26.06.1997  UPREPID is Year 2000 compliant.
!
! REVISION INFO:
!
! $Workfile: uprepid.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 17/11/2010 09:58:48$
!
! change record :
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

CHARACTER(1), INTENT(IN)    ::  Byte17 !- current index entry
INTEGER,      INTENT(OUT)   ::  RprtId  !- report identity

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  IByte17                !- byte 17 of index entry
INTEGER      ::  Mdi = -9999999         !- missing data indicator

LOGICAL      ::  DropSonde              !- TRUE if it's a DROPSOND
LOGICAL      ::  Fixed                  !- TRUE if fixed report
LOGICAL      ::  OnLand                 !- TRUE if on land

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

IByte17   = ICHAR(Byte17)
DropSonde = (MOD(IByte17/8,2) == 1 .AND. MOD(IByte17/4,2) == 0)
Fixed     = (MOD(IByte17/2,2) == 0)
OnLand    = (MOD(IByte17/1,2) == 0)

IFLABEL1: &
IF (DropSonde) THEN
  RprtId = 0
ELSE
  IF (Fixed .AND. OnLand) THEN
    RprtId = 1
  ELSE IF ((.NOT.Fixed) .AND. OnLand) THEN
    RprtId = 2
  ELSE IF (.NOT.OnLand) THEN
    RprtId = 3
  ELSE
    RprtId = Mdi
  END IF
END IF IFLABEL1

RETURN
END SUBROUTINE UPREPID
