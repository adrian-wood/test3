MODULE synfiv_mod
  INTERFACE
    SUBROUTINE SYNFIV(REPORT,POINT,NGRPS,IG, &
        IR,IE,EEE,SUN2,SUN1,RNET,GLSO,DISO,LW,SW,SWNET,SDIR, &
        RNET24,GLSO24,DISO24,LW24,SW24,SWNET24,SDIR24,PCHG24)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: REPORT  !a01
    INTEGER,          INTENT(INOUT) :: POINT   !a02 to first character of a group
    INTEGER,          INTENT(IN)    :: NGRPS   !a03 number of groups in section
    INTEGER,          INTENT(INOUT) :: IG      !a04 number of current group in section
    INTEGER,          INTENT(IN)    :: IR      !a05 rainfall indicator
    REAL,             INTENT(INOUT) :: IE      !a06 Type of instrumentation for evaporation (iE)
    REAL,             INTENT(INOUT) :: EEE     !a07 Evaporation or evapotranspiration (EEE)
    REAL,             INTENT(INOUT) :: SUN2    !a08 24 hour Sunshine amount (SSS)
    REAL,             INTENT(INOUT) :: SUN1    !a09 1 hour Sunshine amount (SS)
    REAL,             INTENT(INOUT) :: RNET    !a10 1 hour Net radiation
    REAL,             INTENT(INOUT) :: GLSO    !a11 1 hour global radiation
    REAL,             INTENT(INOUT) :: DISO    !a12 1 hour diffuse radiation
    REAL,             INTENT(INOUT) :: LW      !a13 1 hour Longwave radiation
    REAL,             INTENT(INOUT) :: SW      !a14 1 hour shortwave radiation
    REAL,             INTENT(INOUT) :: SWNET   !a15 1 hour net shortwave radiation
    REAL,             INTENT(INOUT) :: SDIR    !a16 1 hour direct solar radiation
    REAL,             INTENT(INOUT) :: RNET24  !a17 24 hour Net radiation
    REAL,             INTENT(INOUT) :: GLSO24  !a18 24 hour global radiation
    REAL,             INTENT(INOUT) :: DISO24  !a19 24 hour diffuse radiation
    REAL,             INTENT(INOUT) :: LW24    !a20 24 hour Longwave radiation
    REAL,             INTENT(INOUT) :: SW24    !a21 24 hour shortwave radiation
    REAL,             INTENT(INOUT) :: SWNET24 !a22 24 hour net shortwave radiation
    REAL,             INTENT(INOUT) :: SDIR24  !a23 24 hour direct solar radiation
    REAL,             INTENT(INOUT) :: PCHG24  !a24 24 hour Pressure change amount (P24P24P24)

    END SUBROUTINE SYNFIV
  END INTERFACE
END MODULE synfiv_mod
