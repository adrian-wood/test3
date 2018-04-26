      CHARACTER*2 FUNCTION CHAR2 (I4)
!
!----------------------------------------------------------------------
!
! FUNCTION     : CHAR2
!
! PURPOSE      : To convert an INTEGER*4 to a CHARACTER*2.
!
! USAGE        : C2 = CHAR2(I4)
!
! PARAMETERS   : I4 - INTEGER*4 input to be converted to a CHARACTER*2.
!
! EXAMPLE      : If 'I4' is 1045, 'CHAR2(I4)' would be '0415' in hex.
!
! CALLED BY    : Various storage routines.
!
! HISTORY      : Original version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:39$
! $Source: /home/us0400/mdb/op/lib/source/RCS/char2.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:39    Sheila Needham  
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
      INTEGER I4         ! INTEGER to be converted to CHARACTER*2.
      LOGICAL FIRST      ! .TRUE. if first call to subroutine
      CHARACTER*132 HEAD ! Revision information
!
!                                              DATA and SAVE statements
      DATA FIRST /.TRUE./
      SAVE FIRST
!                               Revision information  (first call only)
      IF (FIRST) THEN
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/char2.F,v $
     &   '//'$Date: 30/01/2006 20:21:39$ $Revision: 1$'
         FIRST = .FALSE.
      END IF
!                                       Convert integer to 2 characters
!
      CHAR2 = CHAR(I4/256) // CHAR(MOD(I4,256))
!                                                                Return
      RETURN
      END
