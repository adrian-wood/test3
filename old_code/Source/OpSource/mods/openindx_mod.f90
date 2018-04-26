MODULE openindx_mod
  INTERFACE
    SUBROUTINE OPENINDX (MEMBER, ICODE)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(8), INTENT(IN)        ::  MEMBER ! Element index member name
    INTEGER, INTENT(OUT)            ::  ICODE ! Return code

    END SUBROUTINE OPENINDX
  END INTERFACE
END MODULE openindx_mod
