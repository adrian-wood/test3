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
! $Workfile: ichar3.f90$ $Folder: source$
! $Revision: 1$ $Date: 25/09/2012 09:53:18$
!
! CHANGE RECORD:
!
! $Log:
!  1    MOODS      1.0         25/09/2012 09:53:18    Paul Barnham    Copied
!       from MetDB project, revision 2.
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
