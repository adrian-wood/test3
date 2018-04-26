MODULE wanttime_mod
  INTERFACE
    SUBROUTINE WANTTIME (ENTRY, NVER, NXTIM1, NTIM1, NTIM2, KEEP)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)        ::  ENTRY ! Index entry
    INTEGER, INTENT(IN)             ::  NVER ! Index entry version number (1 or 2)
    INTEGER, INTENT(IN)             ::  NXTIM1 ! Century minutes for start of index period
    INTEGER, INTENT(IN)             ::  NTIM1 ! Century minutes for user's start date/time
    INTEGER, INTENT(IN)             ::  NTIM2 ! Century minutes for user's end date/time
    INTEGER, INTENT(OUT)            ::  KEEP ! 'Keep' flag from time window checking

    END SUBROUTINE WANTTIME
  END INTERFACE
END MODULE wanttime_mod
