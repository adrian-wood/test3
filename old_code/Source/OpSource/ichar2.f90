INTEGER FUNCTION ICHAR2 (C2)

!----------------------------------------------------------------------
!
! FUNCTION     : ICHAR2
!
! PURPOSE      : To convert a CHARACTER*2 string to an INTEGER.
!
! USAGE        : I4 = ICHAR2(C2)
!
! PARAMETERS   : C2 - CHARACTER*2 input to be converted to an INTEGER.
!
! EXAMPLE      : If 'C2' is 0415 in hex, 'ICHAR2(C2)' would be 1045.
!
! HISTORY      : Original F77 version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Workfile: ichar2.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 19/10/2010 14:33:25$
!
! CHANGE RECORD:
!
! $Log:
!  2    MetDB_Refresh 1.1         19/10/2010 14:33:25    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!  1    MetDB_Refresh 1.0         12/10/2010 16:26:11    Brian Barwell
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

IMPLICIT NONE
!                                                             Variables
CHARACTER*2 C2   ! 2 characters to convert to integer

!                                       Convert 2 characters to integer

ICHAR2 = 256*ICHAR(C2(1:1)) + ICHAR(C2(2:2))
!                                                                Return
RETURN
END FUNCTION ICHAR2
