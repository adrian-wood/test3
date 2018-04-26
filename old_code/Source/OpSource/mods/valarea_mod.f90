MODULE valarea_mod
  INTERFACE
    SUBROUTINE VALAREA(LAT,LONG,AREA,AREA_FLAG,LFLAG)

    IMPLICIT NONE

! Subroutine arguments:

     REAL,         INTENT(IN)    ::  LAT !- Latitude from observation
     REAL,         INTENT(IN)    ::  LONG !- Longitude from observation
     REAL,         INTENT(IN)    ::  AREA(5) !- Users requested Lat/Long box
     LOGICAL,      INTENT(OUT)   ::  AREA_FLAG !- Result flag from lat/long check
     LOGICAL,      INTENT(IN)    ::  LFLAG !- Diagnostic testing flag

    END SUBROUTINE VALAREA
  END INTERFACE
END MODULE valarea_mod
