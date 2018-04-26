MODULE char3_mod
  INTERFACE
    FUNCTION CHAR3 (I4)

    IMPLICIT NONE

! Function arguments:
    INTEGER, INTENT(IN) ::   I4 ! INTEGER to be converted to CHARACTER*3.

! Function result:
    CHARACTER(LEN=3)    ::   CHAR3

    END FUNCTION CHAR3
  END INTERFACE
END MODULE char3_mod
