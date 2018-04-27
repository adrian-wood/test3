      INTEGER FUNCTION ICHAR3 (C3)
!
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
! CALLED BY    : Various storage routines.
!
! HISTORY      : Original version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:55$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ichar3.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:55    Sheila Needham  
! $
! Revision 2.0  2001/04/23 13:19:41  usmdb
! Initial Revision
!
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!----------------------------------------------------------------------
!
      IMPLICIT NONE
!                                                             Variables
      LOGICAL FIRST      ! .TRUE. if first call to subroutine
      CHARACTER*3   C3   ! 3 characters to convert to integer
      CHARACTER*132 HEAD ! Revision information
!
!                                              DATA and SAVE statements
      DATA FIRST /.TRUE./
      SAVE FIRST
!                               Revision information  (first call only)
      IF (FIRST) THEN
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/ichar3.F,v $
     &   '//'$Date: 30/01/2006 20:22:55$ $Revision: 1$'
         FIRST = .FALSE.
      END IF
!                                       Convert 3 characters to integer
      ICHAR3 = 65536*ICHAR(C3(1:1)) +
     &           256*ICHAR(C3(2:2)) +
     &               ICHAR(C3(3:3))
!                                                                Return
      RETURN
      END
