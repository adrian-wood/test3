MODULE synxp3_mod
  INTERFACE
    SUBROUTINE SYNXP3(REPORT,POINT,NGRPS,SYNTAX, &
       IWMOB,IWMOR,WNDKTS,WPER,IR,IDATIM,TPER1,TPER2,TGTG,VERTV, &
       A3,HHH,MAXTTT,MINTTT,E,SSS, &
       T1GUST,T2GUST,VGUST, T1GUS,T2GUS,VGUS, &
       T1MEAN,T2MEAN,VMEAN,ICL,GROUP8,VSIF,RRR3,TR3,R24,IE,EEE, &
       SUN2,SUN1,RNET,GLSO,DISO,LW,SW,RNET24,GLSO24, &
       DISO24,LW24,SW24,PCHG24,SEASTE,SEAVIS,VV, &
       LRAD,LSEA)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: REPORT     !a01
    INTEGER,          INTENT(INOUT) :: POINT      !a02
    INTEGER,          INTENT(IN)    :: NGRPS      !a03
    LOGICAL,          INTENT(INOUT) :: SYNTAX     !a04
    INTEGER,          INTENT(IN)    :: IWMOB      !a05
    INTEGER,          INTENT(IN)    :: IWMOR      !a06
    LOGICAL,          INTENT(IN)    :: WNDKTS     !a07
    REAL,             INTENT(INOUT) :: WPER       !a08 Past weather period
    INTEGER,          INTENT(IN)    :: IR         !a09
    INTEGER,          INTENT(IN)    :: IDATIM(*)  !a10
    REAL,             INTENT(INOUT) :: TPER1      !a11 Max temp time period
    REAL,             INTENT(INOUT) :: TPER2      !a12 Min temp time period
    REAL,             INTENT(INOUT) :: TGTG       !a13 Grass minimum temperature (TgTg)
    REAL,             INTENT(INOUT) :: VERTV      !a14 Vertical visibilty
    REAL,             INTENT(INOUT) :: A3         !a15 Standard isobaric surface (a3)
    REAL,             INTENT(INOUT) :: HHH        !a16 Geopotential height (hhh)
    REAL,             INTENT(INOUT) :: MAXTTT     !a17 Max Air Temperature (TxTxTx)
    REAL,             INTENT(INOUT) :: MINTTT     !a18 Min Air Temperature (TnTnTn)
    REAL,             INTENT(INOUT) :: E          !a19 State of Ground (E/E') '
    REAL,             INTENT(INOUT) :: SSS        !a20 Snow depth (sss)
    REAL,             INTENT(INOUT) :: T1GUST     !a21 Start gust period of gust 911 group
    REAL,             INTENT(INOUT) :: T2GUST     !a22 End gust period of gust 911 group
    REAL,             INTENT(INOUT) :: VGUST      !a23 911 group gust value (ff/fff)
    REAL,             INTENT(INOUT) :: T1GUS      !a24 Start gust period of gust 910 group
    REAL,             INTENT(INOUT) :: T2GUS      !a25 End gust period of gust 910 group
    REAL,             INTENT(INOUT) :: VGUS       !a26 910 group gust value (ff/fff)
    REAL,             INTENT(INOUT) :: T1MEAN     !a27 Start mean period of 912 group
    REAL,             INTENT(INOUT) :: T2MEAN     !a28 End mean period of 912 group
    REAL,             INTENT(INOUT) :: VMEAN      !a29 912 group mean value (ff/fff)
    INTEGER,          INTENT(INOUT) :: ICL        !a30
    REAL,             INTENT(INOUT) :: GROUP8(12) !a31 "8" Cloud group elements(4*NsChshs)
    REAL,             INTENT(INOUT) :: VSIF(4)    !a32 Significance of cloud layers
    REAL,             INTENT(INOUT) :: RRR3       !a33 Section 3 Precipitation amount (RRR)
    REAL,             INTENT(INOUT) :: TR3        !a34 Section 3 type of precipitation (tR)
    REAL,             INTENT(INOUT) :: R24        !a35 24 hour Precipitation amount (R24R24R24R24)
    REAL,             INTENT(INOUT) :: IE         !a36 Type of instrumentation for evaporation (iE)
    REAL,             INTENT(INOUT) :: EEE        !a37 Evaporation or evapotranspiration (EEE)
    REAL,             INTENT(INOUT) :: SUN2       !a38 24 hour Sunshine amount (SSS)
    REAL,             INTENT(INOUT) :: SUN1       !a39 1 hour Sunshine amount (SS)
    REAL,             INTENT(INOUT) :: RNET       !a40 1 hour Net radiation
    REAL,             INTENT(INOUT) :: GLSO       !a41 1 hour global radiation
    REAL,             INTENT(INOUT) :: DISO       !a42 1 hour diffuse radiation
    REAL,             INTENT(INOUT) :: LW         !a43 1 hour Longwave radiation
    REAL,             INTENT(INOUT) :: SW         !a44 1 hour shortwave radiation
    REAL,             INTENT(INOUT) :: RNET24     !a45 24 hour Net radiation
    REAL,             INTENT(INOUT) :: GLSO24     !a46 24 hour global radiation
    REAL,             INTENT(INOUT) :: DISO24     !a47 24 hour diffuse radiation
    REAL,             INTENT(INOUT) :: LW24       !a48 24 hour Longwave radiation
    REAL,             INTENT(INOUT) :: SW24       !a49 24 hour shortwave radiation
    REAL,             INTENT(INOUT) :: PCHG24     !a50 24 hour Pressure change amount (P24P24P24)
    REAL,             INTENT(INOUT) :: SEASTE     !a51 State of sea 924 group (S)
    REAL,             INTENT(INOUT) :: SEAVIS     !a52 Horiz vis towards sea 924 group (Vs)
    REAL,             INTENT(INOUT) :: VV         !a53 Visibility (VV)
    LOGICAL,          INTENT(INOUT) :: LRAD       !a54
    LOGICAL,          INTENT(INOUT) :: LSEA       !a55

    END SUBROUTINE SYNXP3
  END INTERFACE
END MODULE synxp3_mod
