REAL FUNCTION TEMPEXP(REPORT)  ! Temperature in Kelvin.

!-----------------------------------------------------------------------
!
! PROGRAM       : TEMPEXP  IN TFMRET
!
! PURPOSE       : EXPAND TEMPERATURE DATA FROM AN NCM REPORT INTO
!                 A REAL VALUE.
!
! DESCRIPTION   : CONVERTS A 4 CHARACTER DATA STRING INTO A REAL VALUE
!                 AND CONVERTS THE VALUE TO A TEMPERATURE IN KELVIN.
!
! DATA TYPE(S)  : NCM, ESAWS
!
! CALLED BY     : NCMEXP, ENHEXP
!
! CALLS         : NONE
!
! ARGUMENTS     : (1) REPORT
!                 (2) TEMPEXP
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         26/11/2010 10:03:10    Brian Barwell   Intent
!        for REPORT changed from INOUT to IN.
!  2    MetDB_Refresh 1.1         18/11/2010 15:09:25    John Norton     Merge
!       batch 20 changes and minor porting issues fixed.
!  1    MetDB_Refresh 1.0         04/11/2010 13:25:18    John Norton     MetDB
!       Refresh batch 8 before reviewing.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! None

IMPLICIT NONE

! Function arguments:

CHARACTER(4), INTENT(IN) :: REPORT ! Data passed for conversion.

! Local declarations:

LOGICAL      ::  CHECK         ! Set if all characters in the data
                               ! string are numeric.

INTEGER      ::  POS           ! Position in data string
                               ! being checked.
INTEGER      ::  SIGN          ! Qualifier for positive or
                               ! negative temperature.

REAL         ::  TEMP          ! Real value of character data.

!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! Initialise variables

CHECK=.TRUE.
POS=1

! Check numeric content of data string.

DO POS=1,4
  IF (REPORT(POS:POS)  <  '0' .OR. REPORT(POS:POS)  >  '9') THEN
    CHECK=.FALSE.
  END IF
END DO

! Convert value to temperature in Kelvin checking for negative or
! positive values.

IF (CHECK) THEN
  READ (REPORT,'(I1,F3.1)') SIGN,TEMP
  IF (SIGN  ==  1) THEN
    TEMP=-TEMP
  END IF
  TEMPEXP=TEMP+273.1                                         !a
ELSE
  TEMPEXP=-9999999
END IF

RETURN
END FUNCTION TEMPEXP
