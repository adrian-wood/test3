MODULE tempexp_mod
  INTERFACE
    REAL FUNCTION TEMPEXP(REPORT)  ! Temperature in Kelvin.

    IMPLICIT NONE

! Function arguments:

    CHARACTER(4), INTENT(IN) :: REPORT ! Data passed for conversion.

    END FUNCTION TEMPEXP
  END INTERFACE
END MODULE tempexp_mod
