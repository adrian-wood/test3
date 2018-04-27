SUBROUTINE SYNIT(                                              &
        WMOBLK,WMOSTA,WMOREG, LAT,LON, STNHT,STNTYP,           &
        DD,FF, TTT,TDTDTD,UUU, VV,VERTV, WW,WPER,W1,W2,        &
        NT, TPER1,MAXTTT,TPER2,MINTTT, E, TR,RRR,TR3,RRR3,R24, &
        T1GUST,T2GUST,VGUST, T1MEAN,T2MEAN,VMEAN,              &
        PPPP,P0P0P0, A3,HHH, SSS,TGTG, ATEND,PCHANG,PCHG24,    &
        IE,EEE, SUN2,SUN1, RNET,GLSO,DISO,LW,SW,SWNET,SDIR,    &
        RNET24,GLSO24,DISO24,LW24,SW24,SWNET24,SDIR24,         &
        SEAVIS,SEASTE,TWTYPE,TWTWTW,                           &
        DS,VS,TBTYPE,TBTBTB,WVMS,WVHT,WVPER,SWELWV,            &
        ISICE,ESES,RS,CI,SI,BI,DI,ZI,                          &
        CL,CM,CH,NH,HL, VSIF,GROUP8, MTCL, IW,HOUR,MINS,       &
        T1GUS,T2GUS,VGUS)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNIT
!
! PURPOSE       : TO INITIALISE ALL ELEMENT VARIABLES TO MISSING
!
! DATA TYPE(S)  : LNDSYN
!
! CALLED BY     : SYNEXP
!
! ARGUMENTS     : ALL ELEMENT VARIABLES
!
! REVISION INFO :
!
!
! $Workfile: synit.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 04/01/2011 16:36:47$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         04/01/2011 16:36:47    Rosemary Lavery
!       Initial Import
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE METDB_COM_mod, ONLY : RMISS   ! missing data value

IMPLICIT NONE

REAL, INTENT(OUT)  :: WMOBLK    !a0x WMO Block Number (II)
REAL, INTENT(OUT)  :: WMOSTA    !a0x WMO Station Number (iii)
REAL, INTENT(OUT)  :: WMOREG    !a0x WMO Region
REAL, INTENT(OUT)  :: LAT       !a0x Latitude (LaLaLa or Station Master)
REAL, INTENT(OUT)  :: LON       !a0x Longitude (LoLoLoLo or Station Master)
REAL, INTENT(OUT)  :: STNHT     !a0x Station height
REAL, INTENT(OUT)  :: STNTYP    !a0x Station type (ix)
REAL, INTENT(OUT)  :: DD        !a0x Wind direction (dd)
REAL, INTENT(OUT)  :: FF        !a0x Wind speed (ff/fff)
REAL, INTENT(OUT)  :: TTT       !a0x Air temperature (TTT)
REAL, INTENT(OUT)  :: TDTDTD    !a0x Dew point temperature (TdTdTd)
REAL, INTENT(OUT)  :: UUU       !a0x Relative humidity (UUU)
REAL, INTENT(OUT)  :: VV        !a0x Visibility (VV)
REAL, INTENT(OUT)  :: VERTV     !a0x Vertical visibilty
REAL, INTENT(OUT)  :: WW        !a0x Present Weather (ww)
REAL, INTENT(OUT)  :: WPER      !a0x Past weather period
REAL, INTENT(OUT)  :: W1        !a0x Past Weather 1 (W1)
REAL, INTENT(OUT)  :: W2        !a0x Past Weather 2 (W2)
REAL, INTENT(OUT)  :: NT        !a0x Total cloud amount (N)
REAL, INTENT(OUT)  :: TPER1     !a0x Max temp time period
REAL, INTENT(OUT)  :: MAXTTT    !a0x Max Air Temperature (TxTxTx)
REAL, INTENT(OUT)  :: TPER2     !a0x Min temp time period
REAL, INTENT(OUT)  :: MINTTT    !a0x Min Air Temperature (TnTnTn)
REAL, INTENT(OUT)  :: E         !a0x State of Ground (E/E') '
REAL, INTENT(OUT)  :: TR        !a0x Section 1 type of precipitation (tR)
REAL, INTENT(OUT)  :: RRR       !a0x Section 1 Precipitation amount (RRR)
REAL, INTENT(OUT)  :: TR3       !a0x Section 3 type of precipitation (tR)
REAL, INTENT(OUT)  :: RRR3      !a0x Section 3 Precipitation amount (RRR)
REAL, INTENT(OUT)  :: R24       !a0x 24 hour Precipitation amount (R24R24R24R24)
REAL, INTENT(OUT)  :: T1GUST    !a0x Start gust period of gust 911 group
REAL, INTENT(OUT)  :: T2GUST    !a0x End gust period of gust 911 group
REAL, INTENT(OUT)  :: VGUST     !a0x 911 group gust value (ff/fff)
REAL, INTENT(OUT)  :: T1MEAN    !a0x Start mean period of gust 912 group
REAL, INTENT(OUT)  :: T2MEAN    !a0x End mean period of gust 912 group
REAL, INTENT(OUT)  :: VMEAN     !a0x 912 group mean value (ff/fff)
REAL, INTENT(OUT)  :: PPPP      !a0x MSL pressure (PPPP)
REAL, INTENT(OUT)  :: P0P0P0    !a0x Station level Pressure (P0P0P0P0)
REAL, INTENT(OUT)  :: A3        !a0x Standard isobaric surface (a3)
REAL, INTENT(OUT)  :: HHH       !a0x Geopotential height (hhh)
REAL, INTENT(OUT)  :: SSS       !a0x Snow depth (sss)
REAL, INTENT(OUT)  :: TGTG      !a0x Grass minimum temperature (TgTg)
REAL, INTENT(OUT)  :: ATEND     !a0x Pressure tendency amount (a)
REAL, INTENT(OUT)  :: PCHANG    !a0x 3 hour Pressure change amount(ppp)
REAL, INTENT(OUT)  :: PCHG24    !a0x 24 hour Pressure change amount (P24P24P24)
REAL, INTENT(OUT)  :: IE        !a0x Type of instrumentation for evaporation (iE)
REAL, INTENT(OUT)  :: EEE       !a0x Evaporation or evapotranspiration (EEE)
REAL, INTENT(OUT)  :: SUN2      !a0x 24 hour Sunshine amount (SSS)
REAL, INTENT(OUT)  :: SUN1      !a0x 1 hour Sunshine amount (SS)
REAL, INTENT(OUT)  :: RNET      !a0x 1 hour Net radiation
REAL, INTENT(OUT)  :: GLSO      !a0x 1 hour global radiation
REAL, INTENT(OUT)  :: DISO      !a0x 1 hour diffuse radiation
REAL, INTENT(OUT)  :: LW        !a0x 1 hour Longwave radiation
REAL, INTENT(OUT)  :: SW        !a0x 1 hour shortwave radiation
REAL, INTENT(OUT)  :: SWNET     !a0x 1 hour net shortwave radiation
REAL, INTENT(OUT)  :: SDIR      !a0x 1 hour direct solar radiation
REAL, INTENT(OUT)  :: RNET24    !a0x 24 hour Net radiation
REAL, INTENT(OUT)  :: GLSO24    !a0x 24 hour global radiation
REAL, INTENT(OUT)  :: DISO24    !a0x 24 hour diffuse radiation
REAL, INTENT(OUT)  :: LW24      !a0x 24 hour Longwave radiation
REAL, INTENT(OUT)  :: SW24      !a0x 24 hour shortwave radiation
REAL, INTENT(OUT)  :: SWNET24   !a0x 24 hour net shortwave radiation
REAL, INTENT(OUT)  :: SDIR24    !a0x 24 hour direct solar radiation
REAL, INTENT(OUT)  :: SEAVIS    !a0x Horiz vis towards sea 924 group (Vs)
REAL, INTENT(OUT)  :: SEASTE    !a0x State of sea 924 group (S)
REAL, INTENT(OUT)  :: TWTYPE    !a0x SST measurement method (ss)
REAL, INTENT(OUT)  :: TWTWTW    !a0x Sea temperature(TwTwTw)
REAL, INTENT(OUT)  :: DS        !a0x Direction of movement (Ds)
REAL, INTENT(OUT)  :: VS        !a0x Speed of movement(vs)
REAL, INTENT(OUT)  :: TBTYPE    !a0x Wet-bulb temperature measurement method (sw)
REAL, INTENT(OUT)  :: TBTBTB    !a0x Wet-bulb temperature(TbTbTb)
REAL, INTENT(OUT)  :: WVMS      !a0x Wave measurement method
REAL, INTENT(OUT)  :: WVHT      !a0x Wave height (HwaHwa)
REAL, INTENT(OUT)  :: WVPER     !a0x Wave period (PwaPwa)
REAL, INTENT(OUT)  :: SWELWV(6) !a0x Swell Waves (2*dwdwPwPwHwHw)
REAL, INTENT(OUT)  :: ISICE     !a0x Cause of ice accretion on ship (Is)
REAL, INTENT(OUT)  :: ESES      !a0x Ice thickness on ship(EsEs)
REAL, INTENT(OUT)  :: RS        !a0x Rate of ice accretion on ship(Rs)
REAL, INTENT(OUT)  :: CI        !a0x Sea ice concentration(ci)
REAL, INTENT(OUT)  :: SI        !a0x Sea ice development(Si)
REAL, INTENT(OUT)  :: BI        !a0x Amount & type of sea ice(bi)
REAL, INTENT(OUT)  :: DI        !a0x Bearing of nearest sea ice(Di)
REAL, INTENT(OUT)  :: ZI        !a0x Sea ice situation (zi)
REAL, INTENT(OUT)  :: CL        !a0x Type Low Cloud (CL)
REAL, INTENT(OUT)  :: CM        !a0x Type Medium Cloud (CM)
REAL, INTENT(OUT)  :: CH        !a0x Type High Cloud (CH)
REAL, INTENT(OUT)  :: NH        !a0x Amount of low cloud (Nh)
REAL, INTENT(OUT)  :: HL        !a0x Height of base of (lowest) cloud (h)
REAL, INTENT(OUT)  :: VSIF(4)   !a0x Significance of cloud layers
REAL, INTENT(OUT)  :: GROUP8(12) !a0x "8" Cloud group elements(4*NsChshs)
REAL, INTENT(OUT)  :: MTCL(4,2) !a0x Mountain Clouds (2*NXCXHXHXCt)
REAL, INTENT(OUT)  :: IW        !a0x Wind Speed Units indicator (iw)
REAL, INTENT(OUT)  :: HOUR      !a0x Hour (GG)
REAL, INTENT(OUT)  :: MINS      !a0x Minutes (gg)
REAL, INTENT(OUT)  :: T1GUS     !a0x Start gust period of gust 910 group
REAL, INTENT(OUT)  :: T2GUS     !a0x End gust period of gust 910 group
REAL, INTENT(OUT)  :: VGUS      !a0x 910 group gust value (ff/fff)

! Local variables

INTEGER:: I
INTEGER:: J

! ---------------------------------------------------------------------

WMOBLK=RMISS    ! BLOCK
WMOSTA=RMISS    ! STATION NUMBER
WMOREG=RMISS    ! WMO REGION
STNTYP=RMISS    ! STATION TYPE CODE TBL 002002
LAT   =RMISS    ! LATITUDE
LON   =RMISS    ! LONGITUDE
STNHT =RMISS    ! STATION HEIGHT
STNTYP=RMISS    ! STATION TYPE
DD    =RMISS    ! WIND DIRECTION
FF    =RMISS    ! WIND SPEED
TTT   =RMISS    ! TEMPERATURE
TDTDTD=RMISS    ! DEWPOINT
UUU   =RMISS    ! RELATIVE HUMIDITY
VV    =RMISS    ! HORIZONTAL VIS
VERTV =RMISS    ! VERTICAL VISIBILITY
WW    =RMISS    ! PRESENT WEATHER
WPER  =RMISS    ! PAST WEATHER PERIOD
W1    =RMISS    ! PAST WEATHER 1
W2    =RMISS    ! PAST WEATHER 2
NT    =RMISS    ! TOTAL CLOUD AMOUNT
TPER1 =RMISS    ! MAX TEMP TIME PERIOD
MAXTTT=RMISS    ! MAX TEMPERATURE
TPER2 =RMISS    ! MIN TEMP TIME PERIOD
MINTTT=RMISS    ! MIN TEMPERATURE
E     =RMISS    ! STATE OF GROUND
TR    =RMISS    ! RAINFALL PERIOD
RRR   =RMISS    ! RAINFALL AMOUNT
TR3   =RMISS    ! RAINFALL TIME PERIOD ( FROM SECTION 3)
RRR3  =RMISS    ! RAINFALL AMOUNT (SECTION 3)
R24   =RMISS    ! 24 HOUR RAINFALL
T1GUST=RMISS    ! GUST PERIOD
T2GUST=RMISS    ! PERIOD OF GUST
T1GUS =RMISS    ! GUST PERIOD
T2GUS =RMISS    ! PERIOD OF GUST
VGUS  =RMISS    ! MAX WIND SPEED (GUST)
VGUST =RMISS    ! MAX WIND SPEED (GUST)
T1MEAN=RMISS    ! MAX WIND SPEED (GUST)
T2MEAN=RMISS    ! PERIOD OF MEAN WIND SPEED
VMEAN =RMISS    ! MAX MEAN WIND SPEED
PPPP  =RMISS    ! MEAN SEA LEVEL PRESSURE
P0P0P0=RMISS    ! STATION LEVEL PRESSURE
A3    =RMISS    ! PRESSURE LEVEL
HHH   =RMISS    ! GEOP. HT OF GIVEN PRESSURE LEVEL
SSS   =RMISS    ! SNOW DEPTH
TGTG  =RMISS    ! GRASS MINIMUM
ATEND =RMISS    ! PRESSURE TENDENCY
PCHANG=RMISS    ! LAST 3 HR PRESSURE CHANGE
PCHG24=RMISS    ! 24 HOUR PRESSURE CHANGE

! RADIATION ELEMENTS

IE    =RMISS    ! TYPE OF INSTR FOR EVAP.TRANS.
EEE   =RMISS    ! 24 HOUR EVAPOTRANSPIRATION
SUN2  =RMISS    ! DAILY HOURS OF SUNSHINE
SUN1  =RMISS    ! SUNSHINE AMOUNT IN LAST HOUR
RNET  =RMISS    ! LAST HOUR NET RADIATION
GLSO  =RMISS    ! LAST HOUR GLOBAL RADIATION
DISO  =RMISS    ! LAST HOUR DIFFUSE RADIATION
LW    =RMISS    ! LAST HOUR LONGWAVE RADIATION
SW    =RMISS    ! LAST HOUR SHORTWAVE RADIATION
SWNET =RMISS    ! LAST HOUR NET SHORTWAVE RADIATION
SDIR  =RMISS    ! LAST HOUR DIRECT SOLAR RADIATION
RNET24=RMISS    ! 24 HOUR NET RADIATION
GLSO24=RMISS    ! 24  HOUR GLOBAL RADIATION
DISO24=RMISS    ! 24 HOUR DIFFUSE RADIATION
LW24  =RMISS    ! 24 HOUR LONGWAVE RADIATION
SW24  =RMISS    ! 24 HOUR SHORTWAVE RADIATION
SWNET24=RMISS   ! 24 HOUR NET SHORTWAVE RADIATION
SDIR24=RMISS    ! 24 HOUR DIRECT SOLAR RADIATION

! SEA (OR COASTAL) ELEMENTS

SEAVIS=RMISS    ! HORIZ VIS TOWARDS SEA
SEASTE=RMISS    ! STATE OF SEA
DS    =RMISS    ! DIRECTION OF MOVEMENT
VS    =RMISS    ! SPEED OF MOVEMENT
TBTYPE=RMISS    ! WET-BULB TEMPERATURE MEASUREMENT METHOD
TBTBTB=RMISS    ! WET-BULB TEMPERATURE
TWTYPE=RMISS    ! SST MEASUREMENT METHOD
TWTWTW=RMISS    ! SEA TEMPERATURE
WVMS  =RMISS    ! WAVE MEASUREMENT METHOD
WVHT  =RMISS    ! WAVE HEIGHT
WVPER =RMISS    ! WAVE PERIOD

DO I=1,6
SWELWV(I)=RMISS
ENDDO

ISICE =RMISS    ! CAUSE OF ICE ACCRETION ON SHIP
ESES  =RMISS    ! ICE THICKNESS ON SHIP
RS    =RMISS    ! RATE OF ICE ACCRETION ON SHIP
CI    =RMISS    ! SEA ICE CONCENTRATION
SI    =RMISS    ! SEA ICE DEVELOPMENT
BI    =RMISS    ! AMOUNT & TYPE OF SEA ICE
DI    =RMISS    ! BEARING OF NEAREST SEA ICE
ZI    =RMISS    ! SEA ICE SITUATION

! CLOUD

CL    =RMISS    ! LOW CLOUD TYPE
CM    =RMISS    ! MEDIUM CLOUD TYPE
CH    =RMISS    ! HIGH CLOUD TYPE
NH    =RMISS    ! AMOUNT OF LOW (LOWEST) CLOUD
HL    =RMISS    ! HT OF BASE OF (LOWEST) CLOUD
IW    =RMISS    ! ORIGINAL WIND SPEED UNITS
HOUR  =RMISS    ! ACTUAL HOUR OF OBSERVATION
MINS  =RMISS    ! ACTUAL MINUTE OF OBSERVATION

! CLOUD 8-GROUPS

DO I=1,4
  VSIF(I)=RMISS
ENDDO

DO I=1,12
  GROUP8(I)=RMISS
ENDDO

! MOUNTAIN CLOUD

DO I=1,2
  DO J=1,4
    MTCL(J,I)=RMISS
  ENDDO
ENDDO

RETURN
END SUBROUTINE SYNIT
