MODULE icolod_mod
  INTERFACE
    SUBROUTINE ICOLOD (ICAOLIST, DEGLAT, DEGLON, HT, HP, NUMICAO)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=4), INTENT(OUT)   :: ICAOLIST(*) !a1 List of ICAO identifiers
    REAL,             INTENT(OUT)   :: DEGLAT(*)   !a2 Latitudes of stations
    REAL,             INTENT(OUT)   :: DEGLON(*)   !a3 Longitudes of stations
    INTEGER,          INTENT(OUT)   :: HT(*)       !a4 Station heights
    INTEGER,          INTENT(OUT)   :: HP(*)       !a5 Station barometer heights
    INTEGER,          INTENT(INOUT) :: NUMICAO     !a6 Number of ICAO identifiers found

    END SUBROUTINE ICOLOD
  END INTERFACE
END MODULE icolod_mod
