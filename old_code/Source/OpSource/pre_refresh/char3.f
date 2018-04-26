      CHARACTER*3 FUNCTION CHAR3 (I4)
!
!----------------------------------------------------------------------
!
! FUNCTION     : CHAR3
!
! PURPOSE      : To convert an INTEGER*4 to a CHARACTER*3.
!
! USAGE        : C3 = CHAR3(I4)
!
! PARAMETERS   : I4 - INTEGER*4 input to be converted to a CHARACTER*3.
!
! EXAMPLE      : If 'I4' is 133653, 'CHAR3(I4)' would be '020A15' in
!                hex (133653 = 2*256**2 + 10*256 + 21).
!
! CALLED BY    : Various storage routines.
!
! HISTORY      : Original version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:40$
! $Source: /home/us0400/mdb/op/lib/source/RCS/char3.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:40    Sheila Needham  
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
!
      INTEGER I4         ! INTEGER to be converted to CHARACTER*3.
      LOGICAL FIRST      ! .TRUE. if first call to subroutine
      CHARACTER*132 HEAD ! Revision information
!
!                                              DATA and SAVE statements
      DATA FIRST /.TRUE./
      SAVE FIRST
!                               Revision information  (first call only)
      IF (FIRST) THEN
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/char3.F,v $
     &   '//'$Date: 30/01/2006 20:21:40$ $Revision: 1$'
         FIRST = .FALSE.
      END IF
!                                       Convert integer to 3 characters
      CHAR3 = CHAR(I4/65536) //
     &        CHAR(MOD(I4/256,256)) //
     &        CHAR(MOD(I4,256))
!                                                                Return
      RETURN
      END
