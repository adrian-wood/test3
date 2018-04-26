MODULE rotarea_mod
  INTERFACE
    SUBROUTINE ROTAREA(LAT,LON,RLAT,RLON,RPOLE)

    IMPLICIT NONE

! Subroutine arguments:

    REAL,         INTENT(IN)  ::  LAT !- Latitude to be rotated
    REAL,         INTENT(IN)  ::  LON !- Longitude to be rotated
    REAL,         INTENT(OUT) ::  RLAT !- Rotated latitude
    REAL,         INTENT(OUT) ::  RLON !- Rotated longitude
    REAL,         INTENT(IN)  ::  RPOLE(2)

    END SUBROUTINE ROTAREA
  END INTERFACE
END MODULE rotarea_mod
