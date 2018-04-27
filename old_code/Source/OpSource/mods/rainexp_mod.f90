MODULE rainexp_mod
  INTERFACE
    REAL FUNCTION RAINEXP(REPORT) ! Rainfall value.

    IMPLICIT NONE

! Function arguments:

    CHARACTER(*), INTENT(IN) ::  REPORT ! Character data.

    END FUNCTION RAINEXP
  END INTERFACE
END MODULE rainexp_mod
