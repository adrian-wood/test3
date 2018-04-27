MODULE staray_mod
  INTERFACE
    SUBROUTINE STARAY (LISTWMO, DEGLAT, DEGLON, HTP, HTSTN, &
                       NTYPE, NUMSTNS, LOOKWMO, NPOS, NUMWMO)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,      INTENT(OUT)   :: LISTWMO(*) !a01 ARRAY OF ALL WMO STATION NUMBERS
    REAL,         INTENT(OUT)   :: DEGLAT(*) !a02 LATITUDES OF STATIONS
    REAL,         INTENT(OUT)   :: DEGLON(*) !a03 LONGITUDES OF STATIONS
    REAL,         INTENT(OUT)   :: HTP(*) !a04 PRESSURE SENSOR HEIGHTS OF STATIONS
    REAL,         INTENT(OUT)   :: HTSTN(*) !a05 SURFACE HEIGHTS OF STATIONS
    INTEGER,      INTENT(OUT)   :: NTYPE(*) !a06 STATION TYPES (SEE COMMENTS AT START)
    INTEGER,      INTENT(INOUT) :: NUMSTNS !a07 TOTAL NUMBER IF STATIONS IN LIST
    INTEGER,      INTENT(OUT)   :: LOOKWMO(*) !a08 ARRAY OF DIFFERENT WMO STATION NUMBERS
    INTEGER,      INTENT(OUT)   :: NPOS(*) !a09 LOCATION IN LISTWMO OF EACH WMO NO.
    INTEGER,      INTENT(OUT)   :: NUMWMO !a10 NUMBER OF DIFFERENT STATIONS IN LIST

    END SUBROUTINE STARAY
  END INTERFACE
END MODULE staray_mod
