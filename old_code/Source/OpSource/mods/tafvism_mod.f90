MODULE tafvism_mod
  INTERFACE
    SUBROUTINE TAFVISM(GROUP,VVVV)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(INOUT) ::  GROUP
    REAL,         INTENT(INOUT) ::  VVVV

    END SUBROUTINE TAFVISM
  END INTERFACE
END MODULE tafvism_mod