
      INTEGER FUNCTION LENSTR(CWORD)
!
!-----------------------------------------------------------------------
!
! ROUTINE     : LENSTR
!
! PURPOSE     : Finds the length of a character string upto the last
!               non-blank character.
!
! DESCRIPTION : Uses intrinsic function LEN to determine the total
!               length of the character string as declared and then
!               counts through, upto the last non-blank character.
!
! CALLED BY   : GARCSUB
!
! CALLS       : -
!
! FILES USED  : -
!
! HISTORY     : 17 Jul 2009  Taken from Commercial Suite module
!                            MCS.MLIB.FORT(COLENG) written by CK Ormonde
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 25/08/2009 10:59:49$      
! $Author: Sheila Needham$
! $Folder: OpSource$
! $Workfile: LENSTR.f$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         25/08/2009 10:59:49    Sheila Needham
!       Changed revision delimiter
!  1    Met_DB_Project 1.0         20/07/2009 14:09:21    Rosemary Lavery New
!       for migration to MASS-R of GRIB archiving
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2009 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!
!
!  1.0       ????
!  2.0     27/08/1998  Scanned for dates in code (Y2K compliance)
!

      IMPLICIT NONE

!     Arguments In:
      CHARACTER*(*) CWORD             ! String to be measured

!     Local scalars:
      INTEGER ILEN                    ! Total length as declared
      INTEGER I                       ! Loop counter

      ILEN=LEN(CWORD)
      LENSTR=0

      DO I=1,ILEN
         IF (CWORD(I:I).EQ.' ') GOTO 401
         LENSTR=I
  401 CONTINUE
      END DO

      RETURN
      END
