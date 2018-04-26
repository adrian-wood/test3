INTEGER FUNCTION ICHAR3 (C3)

!----------------------------------------------------------------------
!
! FUNCTION     : ICHAR3
!
! PURPOSE      : To convert a CHARACTER*3 string to an INTEGER.
!
! USAGE        : I4 = ICHAR3(C3)
!
! PARAMETERS   : C3 - CHARACTER*3 input to be converted to an INTEGER.
!
! EXAMPLE      : If 'C3' is 020A15 in hex, 'ICHAR3(C3)' would be 133653
!                (= 2*256**2 + 10*256 + 21).
!
! HISTORY      : Original F77 version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Workfile: ichar3.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 19/10/2010 14:34:21$
!
! CHANGE RECORD:
!
! $Log:
!  2    MetDB_Refresh 1.1         19/10/2010 14:34:21    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!  1    MetDB_Refresh 1.0         13/10/2010 11:47:33    Brian Barwell
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
CHARACTER*3 C3   ! 3 characters to convert to integer

!                                       Convert 3 characters to integer

ICHAR3 = 65536*ICHAR(C3(1:1)) + 256*ICHAR(C3(2:2)) + ICHAR(C3(3:3))
!                                                                Return
RETURN
END FUNCTION ICHAR3
