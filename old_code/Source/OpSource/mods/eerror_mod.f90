MODULE eerror_mod
  INTERFACE
    SUBROUTINE EERROR(REP,IRC)

    IMPLICIT NONE

! Subroutine arguments:
    CHARACTER(LEN=*), INTENT(INOUT) :: REP  !A01
    INTEGER,          INTENT(OUT)   :: IRC  !A02

    END SUBROUTINE EERROR
  END INTERFACE
END MODULE eerror_mod
