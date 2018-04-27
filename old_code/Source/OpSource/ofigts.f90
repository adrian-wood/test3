LOGICAL FUNCTION OFIGTS(CSTR,ISTART,IEND)

!-----------------------------------------------------------------------
!
! PROGRAM       : OFIGTS
!
! PURPOSE       : TO SEE IF A CHARACTER STRING CONSISTS OF FIGURES
!
! CALLED BY     : BATHY, TESAC, AMDARX & ANY OTHER FORTRAN EXPANSION
!
! ARGUMENTS     : (1) CSTR   - STRING TO BE CHECKED
!                 (2) ISTART - LOCATION OF START OF STRING REQUIRED
!                 (3) IEND   - END OF STRING
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 26/11/2010 11:28:21$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ofigts.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         26/11/2010 11:28:21    Brian Barwell   All
!       ntents changed from INOUT to IN.
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
! Use statements:
!None

IMPLICIT NONE

! Function arguments:

CHARACTER(*), INTENT(IN) ::  CSTR
INTEGER,      INTENT(IN) ::  ISTART
INTEGER,      INTENT(IN) ::  IEND

! Function result:
!<declare the type returned by the Function>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  ILOOP

!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

OFIGTS=.TRUE.
DO ILOOP=ISTART,IEND
     IF ((CSTR(ILOOP:ILOOP) < '0').OR. &
         (CSTR(ILOOP:ILOOP) > '9')) OFIGTS=.FALSE.
END DO
RETURN
END FUNCTION OFIGTS
