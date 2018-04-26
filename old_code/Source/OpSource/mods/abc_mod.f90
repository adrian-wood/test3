MODULE abc_mod
  INTERFACE
    SUBROUTINE ABC (CHAR_ARR, IPOS, NSIZE)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN)    :: NSIZE       !a03 SIZE OF IPOS AND CHAR ARRAYS
    CHARACTER(LEN=8), INTENT(INOUT) :: CHAR_ARR(*) !a01 CHARACTER ARRAY TO BE SORTED
    INTEGER,          INTENT(INOUT) :: IPOS(*)     !a02 POS OF ELEMENTS BEFORE SORTING

    END SUBROUTINE ABC
  END INTERFACE
END MODULE abc_mod
