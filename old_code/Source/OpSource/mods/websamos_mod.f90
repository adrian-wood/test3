MODULE websamos_mod
  INTERFACE
    SUBROUTINE WEBSAMOS(CREP,RPRTEND,WRITEHEADER)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: CREP        !a01- input report text
    INTEGER,          INTENT(IN)    :: RPRTEND     !a02- length of input report text
    LOGICAL,          INTENT(IN)    :: WRITEHEADER !a03- TRUE to output header

    END SUBROUTINE WEBSAMOS
  END INTERFACE
END MODULE websamos_mod
