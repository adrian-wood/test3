FUNCTION CHAR3 (I4)

!----------------------------------------------------------------------
!
! FUNCTION     : CHAR3
!
! PURPOSE      : To convert an INTEGER*4 to a CHARACTER*3.
!
! USAGE        : C3 = CHAR3(I4)
!
! ARGUMENTS    : I4 - INTEGER*4 input to be converted to a CHARACTER*3.
!
! EXAMPLE      : If 'I4' is 133653, 'CHAR3(I4)' would be '020A15' in
!                hex (133653 = 2*256**2 + 10*256 + 21).
!
! CALLED BY    : Various storage routines.
!
! CALLS        : INT2CH
!
! HISTORY      : Original version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Workfile: char3.f90$ $Folder: OpSource$
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
INTEGER, INTENT(IN) ::   I4 ! INTEGER to be converted to CHARACTER*3.

! Function result:
CHARACTER(LEN=3)    ::   CHAR3

CHAR3 = INT2CH(I4/65536) //          &
        INT2CH(MOD(I4/256,256)) //   &
        INT2CH(MOD(I4,256))

RETURN
END FUNCTION CHAR3
