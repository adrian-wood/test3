FUNCTION CHAR2 (I4)

!----------------------------------------------------------------------
!
! FUNCTION     : CHAR2
!
! PURPOSE      : To convert an INTEGER*4 to a CHARACTER*2.
!
! USAGE        : C2 = CHAR2(I4)
!
! ARGUMENTS    : I4 - INTEGER*4 input to be converted to a CHARACTER*2.
!
! EXAMPLE      : If 'I4' is 1045, 'CHAR2(I4)' would be '0415' in hex.
!
! CALLED BY    : Various storage routines.
!
! CALLS        : INT2CH
!
! HISTORY      : Original version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Workfile: char2.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 28/01/2011 15:51:49$
!
! CHANGE RECORD:
!
! $Log:
!  3    MetDB_Refresh 1.2         28/01/2011 15:51:49    Alison Weir
!       Function CHAR changed to INT2CH
!  2    MetDB_Refresh 1.1         25/01/2011 10:28:26    Alison Weir     Ported
!        to f95 - BUFRDAT2
!  1    MetDB_Refresh 1.0         25/01/2011 09:53:08    Alison Weir
!       Initial f77 version
! $
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!----------------------------------------------------------------------

! Use statements:
USE int2ch_mod

IMPLICIT NONE

! Function arguments:
INTEGER, INTENT(IN)   ::  I4  ! INTEGER to be converted to CHARACTER*2.

! Function result:
CHARACTER(LEN=2)      ::  CHAR2

CHAR2 = INT2CH(I4/256) // INT2CH(MOD(I4,256))

RETURN
END FUNCTION CHAR2
