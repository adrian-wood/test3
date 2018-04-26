LOGICAL FUNCTION VALDDY (DAY,MONTH,YEAR)

!-----------------------------------------------------------------------
!
! PROGRAM       : VALDDY
!
! PURPOSE       : this subroutine checks the date input for validity
!
! DESCRIPTION   : A date is passed to the subroutine.  This is
!                 checked for validity and is TRUE for valid day and
!                 FALSE for invalid. Routine expires 2399 so write a
!                 replacement before then! Only valid from 1753
!
!
! CALLS         : MNTHDS
!
! CALLED BY     : OBHOUR
!
! ARGUMENTS     : 1. DAY    - INPUT DAY
!                 2. MONTH  - INPUT MONTH
!                 3. YEAR   - INPUT YEAR
!
! REVISION INFO :
!
! $Workfile: valddy.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 14/12/2010 10:51:40$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         14/12/2010 10:51:40    Alison Weir
!       Initial version MDBSTOR batch 5
! $
!
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
USE zpdate_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN) :: DAY    ! A01
INTEGER,          INTENT(IN) :: MONTH  ! A02
INTEGER,          INTENT(IN) :: YEAR   ! A03

! Local declarations: none

! Check that within valid year range
! Check that within valid month range
! and check that within valid day range

IF ((YEAR  >=  1753) .AND.  &
    (YEAR  <=  2399) .AND.  &
    (MONTH  >=  1) .AND.    &
    (MONTH  <=  12)   .AND. &
    (DAY  >=  1) .AND.      &
    (DAY  <=  MNTHDS(MONTH,YEAR))) THEN
! Valid date
        VALDDY=.TRUE.
ELSE
        VALDDY=.FALSE.
END IF

END FUNCTION VALDDY
