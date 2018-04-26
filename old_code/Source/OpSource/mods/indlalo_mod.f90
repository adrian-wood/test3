MODULE indlalo_mod
  INTERFACE
    SUBROUTINE INDLALO(ENTRY,RLAT,RLONG)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=23), INTENT(INOUT) :: ENTRY  !a1
    REAL,              INTENT(IN)    :: RLAT   !a2
    REAL,              INTENT(IN)    :: RLONG  !a3

    END SUBROUTINE INDLALO
  END INTERFACE
END MODULE indlalo_mod
