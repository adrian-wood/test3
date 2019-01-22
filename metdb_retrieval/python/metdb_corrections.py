""" Contains corrections and additions to the metdb module that have not yet
    made it to the prodction version.
"""
import metdb


def local_updates():
    # Local corrections for errors in the python subtypes module
    metdb.subtypes.DTYPE_MAPS["RAINFALL"][u'SCND'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["AMDARS"][u'SCND'] = 'i4'

    metdb.subtypes.DTYPE_MAPS["METARS"][u'STTN_RPRT_TYPE'] = 'i4'
    metdb.subtypes.CREP_ELEMENTS["METARS"] = "MTR_RPT_TXT"
    metdb.subtypes.DTYPE_MAPS["METARS"][u'MTR_RPT_TXT'] = 'S500'

    metdb.subtypes.DTYPE_MAPS["SPECI"][u'STTN_RPRT_TYPE'] = 'i4'
    metdb.subtypes.CREP_ELEMENTS["SPECI"] = "MTR_RPT_TXT"
    metdb.subtypes.DTYPE_MAPS["SPECI"][u'MTR_RPT_TXT'] = 'S500'
    
    metdb.subtypes.DTYPE_MAPS["SONDE"][u'HUMDY_CRTN'] = 'f4'

    metdb.subtypes.DTYPE_MAPS["AIRQAL"][u'AQTY_OBSG_STTN_DMNT_EMSN_SRC'] = 'i4'

    metdb.subtypes.DTYPE_MAPS["AIRQALEU"] = {
        u'LNG_STTN_NME'                     : 'S32'     , # Long station name
        u'AQTY_OBSG_STTN_LCL_CD'            : 'S7'      , # Observing station local code
        u'ABSE_AQTY_OBSG_STTN_CD'           : 'S6'      , # Airbase observing station code
        u'GEMS_AQTY_OBSG_STTN_CD'           : 'S6'      , # GEMS Station Code
        u'AQTY_OBSG_STTN_DMNT_EMSN_SRC'     : 'i4'      , # Observing station dominant emission source
        u'AQTY_OBSG_STTN_AR_TYP'            : 'i4'      , # Observing station area type
        u'AQTY_OBSG_STTN_TRN_TYP'           : 'i4'      , # Observing station terrain type
        u'YEAR'                             : 'i4'      , # Year
        u'MNTH'                             : 'i4'      , # Month
        u'DAY'                              : 'i4'      , # Day
        u'HOUR'                             : 'i4'      , # Hour
        u'MINT'                             : 'i4'      , # Minute
        u'SCND'                             : 'i4'      , # Second
        u'LTTD'                             : 'f4'      , # Latitude (High Accuracy)
        u'LNGD'                             : 'f4'      , # Longtitude (High Accuracy)
        u'STTN_HGHT'                        : 'f4'      , # Station height
        u'SNSR_HGHT'                        : 'f4'      , # Height of sensor
        u'AVGNG_PERD'                       : 'i4'      , # Averaging time period
        u'CSNT_TYP'                         : 'i4'      , # Atmospheric constituent type
        u'CAS_RGSY_NMBR'                    : 'S88'     , # CAS Registry number
        u'PLTE_MTR_CHTZN'                   : 'i4'      , # Particulate matter characterization
        u'DCML_SCL_MASS_DNSTY'              : 'f4'      , # ?????
        u'MASS_DNSTY'                       : 'f4'      , # ?????
        u'QLTY_INFN'                        : 'f4'      , # ?????
        u'RCPT_YEAR'                        : 'i4'      , # Time of receipt year
        u'RCPT_MNTH'                        : 'i4'      , # Time of receipt month
        u'RCPT_DAY'                         : 'i4'      , # Time of receipt day
        u'RCPT_HOUR'                        : 'i4'      , # Time of receipt hour
        u'RCPT_MINT'                        : 'i4'      , # Time of receipt minute
    }

    # This is to overcome the problem of nested replications
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_SIG_WX_INSY_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_SIG_WX_INSY_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_SIG_WX_INSY_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_SIG_WX_DSC_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_SIG_WX_DSC_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_SIG_WX_DSC_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_SIG_WX_PHNM_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_SIG_WX_PHNM_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_SIG_WX_PHNM_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_CLD_AMT_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_CLD_AMT_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_CLD_AMT_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_CLD_TYPE_ID_1'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_CLD_TYPE_ID_2'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_CLD_TYPE_ID_3'] = 'i4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_CLD_BASE_HT_1'] = 'f4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_CLD_BASE_HT_2'] = 'f4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'CHG_CLD_BASE_HT_3'] = 'f4'
    metdb.subtypes.DTYPE_MAPS["TAFS"][u'TAF_RPT_TXT'] = 'S5000'

    metdb.subtypes.DTYPE_MAPS["BUOYB"] = {
             u'BLTN_TYPE': 'i4',    # 1, 2, 3 or 4 for old, moored, drifting or wave buoy sequence respectively
             u'WMO_REGN_NMBR': 'i4',    # WMO region number
             u'WMO_REGN_SUB_AREA': 'i4',    # WMO region sub-area
             u'BUOY_IDNY': 'i4',    # Buoy identifier
             u'STTN_NAME': 'S32',    # Station name or other identification
             u'STTN_RPRT_TYPE': 'i4',    # Type of station
             u'BUOY_TYPE': 'i4',    # Type of buoy
             u'BUOY_DATA_TYPE': 'i4',    # Type of data buoy
             u'YEAR': 'i4',    # year
             u'MNTH': 'i4',    # month
             u'DAY': 'i4',    # day
             u'HOUR': 'i4',    # hour
             u'MINT': 'i4',    # minute
             u'YEAR_OF_LAST_PSTN': 'i4',    # year of last known position
             u'MNTH_OF_LAST_PSTN': 'i4',    # month of last known position
             u'DAY_OF_LAST_PSTN': 'i4',    # day of last known position
             u'HOUR_OF_LAST_PSTN': 'i4',    # hour of last known position
             u'MINT_OF_LAST_PSTN': 'i4',    # minute  of last known position
             u'LTTD': 'f4',    # latitude
             u'LNGD': 'f4',    # longitude
             u'RCPT_YEAR': 'i4',    # time of receipt year
             u'RCPT_MNTH': 'i4',    # time of receipt month
             u'RCPT_DAY': 'i4',    # time of receipt day
             u'RCPT_HOUR': 'i4',    # time of receipt hour
             u'RCPT_MINT': 'i4',    # time of receipt minute
             u'BUOY_DRCTN': 'f4',    # Direction of motion of BUOY
             u'BUOY_SPED': 'f4',    # Speed of motion of BUOY
             u'BUOY_STLT_TRMSN_QLTY': 'i4',    # Buoy satellite transmission quality
             u'DATA_COLTN_SYSTM': 'i4',    # Data collection system
             u'QLTY_OF_PSTN': 'i4',    # Quality of buoy location
             u'QLTY_OF_PSTN_CLSS': 'i4',    # Location quality class
             u'PLTFM_BTRY_VLTG': 'f4',    # Platform battery voltage
             u'DROG_IDNY': 'i4',    # Drogue type
             u'LGRGN_DRFTR_DROG_STTS': 'i4',    # Lagrangian drifter drogue status
             u'DROG_DPTH': 'f4',    # Drogue depth
             u'LGRGN_DRFTR_SBMGC': 'i4',    # Lagrangian drifter submergence
             u'ICE_THCKS': 'f4',    # Ice thickness
             u'SEA_TMPR_PRCSN': 'f4',    # Precision of temperature observation
             u'DGTZN_INDTR_T': 'i4',    # Digitization indicator
             u'TMPR_LEVL_RPLTN_CONT': 'i4',    # Number of temperature levels
             u'DPTH_OF_TMPR_MESRT': 'f4',    # Depth below sea surface
             u'LEVL_SEA_TMPR_T': 'f4',    # Sea temperature
             u'DGTZN_INDTR': 'i4',    # Digitization indicator
             u'SALNY_DPTH_INST': 'i4',    # Method of salinity/depth measurement
             u'TMPR_SALNY_LEVL_RPLTN_CONT': 'i4',    # No. of temp./salinity levels
             u'DPTH_OF_TMPR_SALNY_MESRT': 'f4',    # Depth below sea surface
             u'LEVL_SEA_TMPR': 'f4',    # Sea temperature
             u'LEVL_SEA_SALNY': 'f4',    # Sea salinity
             u'DRTN_TIME_CRNT_MESRT': 'i4',    # Duration of current measurement
             u'CRNT_LEVL_RPLTN_CONT': 'i4',    # Number of current levels
             u'DPTH_OF_CRNT_MESRT': 'f4',    # Depth of current measurement
             u'LEVL_CRNT_DRCTN': 'f4',    # Direction of current
             u'LEVL_CRNT_SPED': 'f4',    # Speed of current
             u'STTN_PESR': 'f4',    # Station pressure
             u'MSL_PESR': 'f4',    # Mean sea level pressure
             u'TMPR_HUMDY_SNSR_HGHT_WATR': 'f4',    # Height of temperature & humidity above water surface
             u'SRFC_AIR_TMPR': 'f4',    # Surface air temperature
             u'SRFC_DEW_PONT_TMPR': 'f4',    # Surface dew point temperature
             u'SRFC_RLTV_HUMDY': 'f4',    # Surface relative humidity
             u'WIND_SNSR_HGHT_WATR': 'f4',    # Height of wind sensor above water surface
             u'ANMTR_TYPE': 'i4',    # Type of anemometer
             u'SRFC_WIND_PERD': 'f4',    # Surface wind period
             u'SRFC_WIND_DRCTN': 'f4',    # Surface wind direction
             u'SRFC_WIND_SPED': 'f4',    # Surface wind speed
             u'SRFC_GUST_PERD': 'f4',    # Surface gust period
             u'MXMM_SRFC_GUST_SPED': 'f4',    # Maximum surface gust speed
             u'SEA_SRFC_TMPR_PRCSN': 'f4',    # Precision of temperature observation
             u'SEA_SRFC_TMPR_DPTH': 'f4',    # Actual depth of sea SST observation
             u'SEA_SRFC_TMPR': 'f4',    # Sea surface temperature
             u'SRFC_SALNY_DPTH_INST': 'i4',    # Method of salinity/depth measurement
             u'SEA_SRFC_SALNY': 'f4',    # Sea surface salinity
             u'SRFC_TYPE': 'i4',    # Surface type
             u'HRZL_VSBLY': 'f4',    # Horizontal visibility
             u'PRCTN_PERD': 'f4',    # Precipitation period
             u'PRCTN_AMNT': 'f4',    # Precipitation amount
             u'RADTN_PERD': 'f4',    # Radiation observation period
             u'LONG_WAVE_RADTN': 'f4',    # Long-wave radiation
             u'SHRT_WAVE_RADTN': 'f4',    # Short-wave radiation
             u'NET_RADTN': 'f4',    # Net radiation
             u'GLOBL_SOLR_RADTN': 'f4',  # Global solar radiation
             u'DIFS_SOLR_RADTN': 'f4',  # Diffuse solar radiation
             u'DRCT_SOLR_RADTN': 'f4',  # Direct solar radiation
             u'DRTN_WAVE_RCRD': 'f4',  # Duration of wave record
             u'MXMM_WAVE_HGHT': 'f4',  # Maximum wave height
             u'SGNT_WAVE_HGHT': 'f4',  # Significant wave height
             u'MEAN_WAVE_PERD': 'f4',  # Mean wave period
             u'DRCTN_PEAK_WAVE': 'f4',  # Direction from which dominant waves are coming
             u'SPRD_PEAK_WAVE': 'f4',  # Directional spread of dominant waves
             u'PEAK_WAVE_PERD': 'f4',  # Spectral peak wave period
             u'DRTN_SPCL_WAVE_RCRD': 'f4',  # duration of spectral wave record
             u'MXMM_SPCL_WAVE_DNSY': 'f4',  # Maximum non-directional spectral wave density
             u'SPCL_BAND_RPLTN_CONT': 'i4',  # Number of wavebands
             u'BAND_CNTRL_FRQY': 'f4',  # Waveband central frequency
             u'SPCL_BAND_WDTH': 'f4',  # Spectral band width
             u'SPCL_WAVE_DNSY': 'f4',  # Spectral wave density
             u'MEAN_DRCTN_OF_WAVES': 'f4',  # Mean direction from which waves are coming
             u'PRNL_DRCTN_OF_WAVES': 'f4',  # Principal direction from which waves are coming
             u'FRST_NDMLD_POLR_CRDT': 'f4',  # 1st Normalized polar co-ordinate from Fourier coefficients
             u'SCND_NDMLD_POLR_CRDT': 'f4',   # 2nd Normalized polar co-ordinate from Fourier coefficients
             }

    metdb.subtypes.DTYPE_MAPS["ARGOB"] = {
             u'WMO_MRN_PLTM_IDNY': 'i4',  # WMO Marine Observing platform Extended identifier
             u'PLTM_MNFT_MODL': 'S32',  # Observing Platform Manufacturers Model
             u'PLTM_MNFT_SERL': 'S32',  # Observing Platform Manufacturers Serial No
             u'YEAR': 'i4',  # year
             u'MNTH': 'i4',  # month
             u'DAY': 'i4',  # day
             u'HOUR': 'i4',  # hour
             u'MINT': 'i4',  # minute
             u'LTTD': 'f4',  # latitude
             u'LNGD': 'f4',  # longitude
             u'RCPT_YEAR': 'i4',  # time of receipt year
             u'RCPT_MNTH': 'i4',  # time of receipt month
             u'RCPT_DAY': 'i4',  # time of receipt day
             u'RCPT_HOUR': 'i4',  # time of receipt hour
             u'RCPT_MINT': 'i4',  # time of receipt minute
             u'BUOY_TYPE': 'i4',  # Buoy Type
             u'BUOY_DATA_TYPE': 'i4',  # Type of data buoy
             u'DATA_COLTN_SYSTM': 'i4',  # Data collection system
             u'FLOT_CYCL': 'i4',  # Float cycle number
             u'PRFL_DRCTN': 'i4',  # Direction of profile
             u'SEA_TMPR_INST_TYPE': 'i4',  # Instrument type for water Temperature profile measurement
             u'GTSPP_POSN_QC': 'i4',  # GTSPP Position QC Flag
             u'LEVL_RPLTN_CONT': 'i4',  # No of levels
             u'SUB_SRFC_TMPR': 'f4',  # Subsurface sea temperature
             u'GTSPP_TMPR_QC': 'i4',  # GTSPP Temperature QC Flag
             u'SUB_SRFC_PESR': 'f4',  # Water Pressure
             u'GTSPP_PESR_QC': 'i4',  # GTSPP Water pressure QC Flag
             u'SALNY': 'f4',  # Salinity
             u'GTSPP_SALNY_QC': 'i4',  # GTSPP Salinity QC Flag
             }
