!-----------------------------------------------------------------------
!
! Function      : LENGTH
!
! Purpose       : Returns length of string excluding trailing blanks
!
! Description   : Accepts a string and uses intrinsic function LEN to
!                 update the occupied length until the last non-blank
!                 character has been reached. (From Comm Suite source)
!
! Data Type(s)  : Generic (utility)
!
! Called by     : CARTSUB
!
! Calls         : N/A
!
! I/O           : N/A
!
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 11/05/2011 16:39:19$
! $Workfile: coleng.f$
!
! CHANGE RECORD :
! $Log:
!  2    MetDB_Refresh 1.1         11/05/2011 16:39:19    Rosemary Lavery std
!       header added 
!  1    MetDB_Refresh 1.0         11/05/2011 16:01:08    Rosemary Lavery Add
!       archiving for ZFS
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------


      FUNCTION LENGTH(CWORD)

!     Arguments (in)
      CHARACTER*(*) CWORD

      ILEN=LEN(CWORD)
      LENGTH=0

      DO I=1,ILEN
         IF (CWORD(I:I).EQ.' ')GOTO 401
         LENGTH=I
  401    CONTINUE
      END DO

      RETURN
      END
