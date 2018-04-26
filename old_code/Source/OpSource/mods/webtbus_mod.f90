MODULE webtbus_mod
  INTERFACE
    SUBROUTINE WEBTBUS(CREP,RPRTEND,WRITEHEADER,CREP2)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: CREP        !a01- input report text
    INTEGER,          INTENT(IN)    :: RPRTEND     !a02- length of input report text
    LOGICAL,          INTENT(IN)    :: WRITEHEADER !a03- TRUE to output header
    CHARACTER(LEN=*), INTENT(INOUT) :: CREP2       !a04- string to hold line of text

    END SUBROUTINE WEBTBUS
  END INTERFACE
END MODULE webtbus_mod
