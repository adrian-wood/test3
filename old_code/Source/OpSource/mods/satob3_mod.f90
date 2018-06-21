MODULE SATOB3_mod
  INTERFACE
    SUBROUTINE SATOB3(POINT,KNOTS,DCVALS,NOBS,BULL)

    IMPLICIT NONE

    INTEGER, INTENT(INOUT)            :: POINT
    LOGICAL, INTENT(IN)               :: KNOTS
    REAL, INTENT(OUT)                 :: DCVALS(:)
    INTEGER, INTENT(IN)               :: NOBS
    CHARACTER (LEN=*), INTENT(INOUT)  :: BULL

    END SUBROUTINE SATOB3
  END INTERFACE
END MODULE SATOB3_mod