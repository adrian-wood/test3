MODULE YSYNELM_mod

  REAL :: ATEND      !a0x Pressure tendency amount (a)
  REAL :: A3         !a0x Standard isobaric surface (a3)
  REAL :: CH         !a0x Type High Cloud (CH)
  REAL :: CL         !a0x Type Low Cloud (CL)
  REAL :: CM         !a0x Type Medium Cloud (CM)
  REAL :: DD         !a0x Wind direction (dd)
  REAL :: DISO       !a0x 1 hour diffuse radiation
  REAL :: DISO24     !a0x 24 hour diffuse radiation
  REAL :: E          !a0x State of Ground (E/E') '
  REAL :: EEE        !a0x Evaporation or evapotranspiration (EEE)
  REAL :: FF         !a0x Wind speed (ff/fff)
  REAL :: GLSO       !a0x 1 hour global radiation
  REAL :: GLSO24     !a0x 24 hour global radiation
  REAL :: GROUP8(12) !a0x "8" Cloud group elements(4*NsChshs)
  REAL :: HHH        !a0x Geopotential height (hhh)
  REAL :: HL         !a0x Height of base of (lowest) cloud (h)
  REAL :: HOUR       !a0x Hour (GG)
  REAL :: IE         !a0x Type of instrumentation for evaporation (iE)
  REAL :: IW         !a0x Wind Speed Units indicator (iw)
  REAL :: LAT        !a0x Latitude (LaLaLa or Station Master)
  REAL :: LON        !a0x Longitude (LoLoLoLo or Station Master)
  REAL :: LW         !a0x 1 hour Longwave radiation
  REAL :: LW24       !a0x 24 hour Longwave radiation
  REAL :: MAXTTT     !a0x Max Air Temperature (TxTxTx)
  REAL :: MINS       !a0x Minutes (gg)
  REAL :: MINTTT     !a0x Min Air Temperature (TnTnTn)
  REAL :: MTCL(4,2)  !a0x Mountain Clouds (2*NØCØHØHØCt)
  REAL :: NH         !a0x Amount of low cloud (Nh)
  REAL :: NT         !a0x Total cloud amount (N)
  REAL :: PCHANG     !a0x 3 hour Pressure change amount(ppp)
  REAL :: PCHG24     !a0x 24 hour Pressure change amount (P24P24P24)
  REAL :: PPPP       !a0x MSL pressure (PPPP)
  REAL :: P0P0P0     !a0x Station level Pressure (P0P0P0P0)
  REAL :: RNET       !a0x 1 hour Net radiation
  REAL :: RNET24     !a0x 24 hour Net radiation
  REAL :: RRR        !a0x Section 1 Precipitation amount (RRR)
  REAL :: RRR3       !a0x Section 3 Precipitation amount (RRR)
  REAL :: R24        !a0x 24 hour Precipitation amount (R24R24R24R24)
  REAL :: SDIR       !a0x 1 hour direct solar radiation
  REAL :: SW         !a0x 1 hour shortwave radiation
  REAL :: SWNET      !a0x 1 hour net shortwave radiation
  REAL :: SDIR24     !a0x 24 hour direct solar radiation
  REAL :: SEASTE     !a0x State of sea 924 group (S)
  REAL :: SEAVIS     !a0x Horiz vis towards sea 924 group (Vs)
  REAL :: SSS        !a0x Snow depth (sss)
  REAL :: STNHT      !a0x Station height
  REAL :: STNTYP     !a0x Station type (ix)
  REAL :: SUN1       !a0x 1 hour Sunshine amount (SS)
  REAL :: SUN2       !a0x 24 hour Sunshine amount (SSS)
  REAL :: SWELWV(6)  !a0x Swell Waves (2*dwdwPwPwHwHw)
  REAL :: SWNET24    !a0x 24 hour net shortwave radiation
  REAL :: SW24       !a0x 24 hour shortwave radiation
  REAL :: TDTDTD     !a0x Dew point temperature (TdTdTd)
  REAL :: TGTG       !a0x Grass minimum temperature (TgTg)
  REAL :: TPER1      !a0x Max temp time period
  REAL :: TPER2      !a0x Min temp time period
  REAL :: TR         !a0x Section 1 type of precipitation (tR)
  REAL :: TR3        !a0x Section 3 type of precipitation (tR)
  REAL :: TTT        !a0x Air temperature (TTT)
  REAL :: TWTWTW     !a0x Sea temperature(TwTwTw)
  REAL :: TWTYPE     !a0x SST measurement method (ss)
  REAL :: T1GUS      !a0x Start gust period of gust 910 group
  REAL :: T2GUS      !a0x End gust period of gust 910 group
  REAL :: T1GUST     !a0x Start gust period of gust 911 group
  REAL :: T2GUST     !a0x End gust period of gust 911 group
  REAL :: T1MEAN     !a0x Start mean period of gust 912 group
  REAL :: T2MEAN     !a0x End mean period of gust 912 group
  REAL :: UUU        !a0x Relative humidity (UUU)
  REAL :: VERTV      !a0x Vertical visibilty
  REAL :: VGUS       !a0x 910 group gust value (ff/fff)
  REAL :: VGUST      !a0x 911 group gust value (ff/fff)
  REAL :: VMEAN      !a0x 912 group mean value (ff/fff)
  REAL :: VSIF(4)    !a0x Significance of cloud layers
  REAL :: VV         !a0x Visibility (VV)
  REAL :: WMOBLK     !a0x WMO Block Number (II)
  REAL :: WMOREG     !a0x WMO Region
  REAL :: WMOSTA     !a0x WMO Station Number (iii)
  REAL :: WPER       !a0x Past weather period
  REAL :: WVHT       !a0x Wave height (HwaHwa)
  REAL :: WVMS       !a0x Wave measurement method
  REAL :: WVPER      !a0x Wave period (PwaPwa)
  REAL :: WW         !a0x Present Weather (ww)
  REAL :: W1         !a0x Past Weather 1 (W1)
  REAL :: W2         !a0x Past Weather 2 (W2)

END MODULE YSYNELM_mod
