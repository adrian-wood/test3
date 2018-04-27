MODULE icobrv_mod
  INTERFACE
    SUBROUTINE ICOBRV (ICAO, POSN, HT, IRC)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=4), INTENT(IN)    :: ICAO    !a1 Target ICAO identifier !2
    REAL,             INTENT(OUT)   :: POSN(2) !a2 Lat & Long of target station !2
    INTEGER,          INTENT(OUT)   :: HT      !a3 Height of target station !2
    INTEGER,          INTENT(OUT)   :: IRC     !a4 Return code (0 or 8)

    END SUBROUTINE ICOBRV
  END INTERFACE
END MODULE icobrv_mod
