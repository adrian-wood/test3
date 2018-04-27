MODULE uadups_mod
  INTERFACE
    SUBROUTINE UADUPS(INDEKS,NTRIES,IDENT,ENTRY,IND)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,           INTENT(IN)    :: NTRIES         !a2
    CHARACTER(LEN=23), INTENT(INOUT) :: INDEKS(NTRIES) !a1
    CHARACTER(LEN=10), INTENT(IN)    :: IDENT          !a3
    CHARACTER(LEN=23), INTENT(INOUT) :: ENTRY          !a4
    INTEGER,           INTENT(OUT)   :: IND            !a5

    END SUBROUTINE UADUPS
  END INTERFACE
END MODULE uadups_mod
