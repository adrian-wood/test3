MODULE IDMAKE_MOD
  INTERFACE
    SUBROUTINE IDMAKE (VALUES, CSTR, NOBS, NSKIP, IDNDX)

    IMPLICIT NONE
!                                Subroutine arguments

    INTEGER,          INTENT(IN)  :: NOBS  ! No. of obs in message
    REAL,             INTENT(IN)  :: VALUES(NOBS) ! 'Values' for IDs
    CHARACTER(LEN=*), INTENT(IN)  :: CSTR  ! Characters from decoder
    INTEGER,          INTENT(IN)  :: NSKIP ! No. of ID chars. to skip
    CHARACTER(LEN=8), INTENT(OUT) :: IDNDX ! Identifier for index entry

    END SUBROUTINE IDMAKE
  END INTERFACE
END MODULE IDMAKE_MOD
