MODULE char2_mod
  INTERFACE
    FUNCTION CHAR2 (I4)

    IMPLICIT NONE

! Function arguments:
    INTEGER, INTENT(IN)   ::  I4  ! INTEGER to be converted to CHARACTER*2.

! Function result:
    CHARACTER(LEN=2)      ::  CHAR2

    END FUNCTION CHAR2
  END INTERFACE
END MODULE char2_mod
