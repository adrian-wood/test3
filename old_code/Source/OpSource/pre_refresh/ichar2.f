      INTEGER FUNCTION ICHAR2 (C2)
!
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
! CALLED BY    : Various storage routines.
!
! HISTORY      : Original version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:54$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ichar2.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:54    Sheila Needham  
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
      CHARACTER*2   C2   ! 2 characters to convert to integer
      CHARACTER*132 HEAD ! Revision information
!
!                                              DATA and SAVE statements
      DATA FIRST /.TRUE./
      SAVE FIRST
!                               Revision information  (first call only)
      IF (FIRST) THEN
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/ichar2.F,v $
     &   '//'$Date: 30/01/2006 20:22:54$ $Revision: 1$'
         FIRST = .FALSE.
      END IF
!                                       Convert 2 characters to integer
      ICHAR2 = 256*ICHAR(C2(1:1)) +
     &             ICHAR(C2(2:2))
!                                                                Return
      RETURN
      END
