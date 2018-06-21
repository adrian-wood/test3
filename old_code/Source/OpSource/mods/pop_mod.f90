MODULE pop_mod
  INTERFACE
    SUBROUTINE POP(SP, ELEM, STACK, IERR)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,      INTENT(INOUT) ::  SP
    CHARACTER(1), INTENT(OUT)   ::  ELEM
    CHARACTER(1), INTENT(IN)    ::  STACK(:)
    INTEGER,      INTENT(OUT)   ::  IERR

    END SUBROUTINE POP
  END INTERFACE
END MODULE pop_mod