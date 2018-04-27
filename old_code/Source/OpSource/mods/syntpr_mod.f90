MODULE syntpr_mod
  INTERFACE
    SUBROUTINE SYNTPR(REGION,HOUR,TFLAG,TPER)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN)    :: REGION  !a01
    INTEGER,          INTENT(IN)    :: HOUR    !a02
    INTEGER,          INTENT(IN)    :: TFLAG   !a03
    REAL,             INTENT(OUT)   :: TPER    !a04

    END SUBROUTINE SYNTPR
  END INTERFACE
END MODULE syntpr_mod
