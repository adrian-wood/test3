MODULE IDMTCH_MOD
  INTERFACE
    SUBROUTINE IDMTCH (ENTRY, ID, IDLIST, LFLAG, RC)

    IMPLICIT NONE
!                                Subroutine arguments

    CHARACTER(*), INTENT(IN)  :: ENTRY     ! Index entry
    CHARACTER(9), INTENT(OUT) :: ID        ! Identifier from entry
    CHARACTER(9), INTENT(IN)  :: IDLIST(:) ! Required identifiers
    LOGICAL,      INTENT(IN)  :: LFLAG     ! Flag for extra diagnostics
    INTEGER,      INTENT(OUT) :: RC        ! Return code (0 or 1)

    END SUBROUTINE IDMTCH
  END INTERFACE
END MODULE IDMTCH_MOD
