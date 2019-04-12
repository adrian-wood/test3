# Merge these code fragments with subtypes.py in the metdb module

CREP_ELEMENTS = {
         u'LNDSYB' : u''
                }
DTYPE_MAPS = {
    "LNDSYB":{ 
        u'WMO_BLCK_NMBR'                    : 'i4'      , # WMO block number
        u'WMO_STTN_NMBR'                    : 'i4'      , # WMO station number
        u'WMO_REGN_NMBR'                    : 'i4'      , # WMO region number
        u'WMO_STTN_NAME'                    : 'S40'     , # WMO station name
        u'CALL_SIGN'                        : 'S9'      , # Ship or Mobile station identifier
        u'SHIP_DRCTN'                       : 'f4'      , # Direction of motion of ship
        u'SHIP_SPED'                        : 'f4'      , # Speed of motion of ship
        u'STTN_RPRT_TYPE'                   : 'i4'      , # station type
        u'YEAR'                             : 'i4'      , # year
        u'MNTH'                             : 'i4'      , # month
        u'DAY'                              : 'i4'      , # day
        u'HOUR'                             : 'i4'      , # hour
        u'MINT'                             : 'i4'      , # minute
        u'RCPT_YEAR'                        : 'i4'      , # time of receipt year
        u'RCPT_MNTH'                        : 'i4'      , # time of receipt month
        u'RCPT_DAY'                         : 'i4'      , # time of receipt day
        u'RCPT_HOUR'                        : 'i4'      , # time of receipt hour
        u'RCPT_MINT'                        : 'i4'      , # time of receipt minute
        u'LTTD'                             : 'f4'      , # Latitude
        u'LNGD'                             : 'f4'      , # Longitude
        u'STTN_HGHT'                        : 'f4'      , # Station height above sea level
        u'STTE_IDNY'                        : 'i4'      , # State identifier
        u'NTNL_STTN_NMBR'                   : 'f4'      , # National station number
        u'SRFC_TYPE'                        : 'i4'      , # Surface type
        u'DATA_SLCTN'                       : 'i4'      , # Data selection value (1 land 2 sea 3 mobile)
        u'CORTN_NMBR'                       : 'i4'      , # Bulletin correction number
        u'TIME_UNIT'                        : 'i4'      , # Units flag for time periods (1 hours 2 minutes)
        u'PRST_WTHR_DTCG_SYTM'              : 'i4'      , # Present weather detection system
        u'SPLTY_PRST_WTHR_SNSR'             : 'i4'      , # Supplementary present weather sensor
        u'VSBLY_MESRT_SYTM'                 : 'i4'      , # Visibility measurement system
        u'CLOD_DTCN_SYTM'                   : 'i4'      , # Cloud detection system
        u'LGTNG_SNSR_TYPE'                  : 'i4'      , # Type of lightning detection sensor
        u'SKY_CNDN_ALGM'                    : 'i4'      , # Type of sky condition algorithm
        u'PRCTN_DTCN_CPBLY'                 : 'i4'      , # Precipitation detection capability
        u'WTHR_DTCN_CPBLY'                  : 'i4'      , # Other weather detection capability
        u'OBSRN_DTCN_CPBLY'                 : 'i4'      , # Obscuration detection capability
        u'LGTNG_DSCTN_CPBLY'                : 'i4'      , # Lightning discrimination capability
        u'PESR_SNSR_HGHT'                   : 'f4'      , # Barometer height above sea level
        u'STTN_ELVN_QLTY'                   : 'i4'      , # Station elevation quality mark
        u'STTN_PESR'                        : 'f4'      , # Station pressure
        u'MSL_PESR'                         : 'f4'      , # Mean sea level pressure
        u'STND_PESR_LEVL'                   : 'f4'      , # Standard pressure level
        u'GPTL_HGHT'                        : 'f4'      , # Geopotential height
        u'Q3HOUR_STTN_LEVL_PESR_DFFRC'      : 'f4'      , # 3-hour pressure change
        u'Q3HOUR_PESR_TNDY'                 : 'i4'      , # Characteristic of pressure tendency
        u'Q24HOUR_SRFC_PESR_DFFRC'          : 'f4'      , # 24-hour pressure change
        u'TMPR_SNSR_HGHT'                   : 'f4'      , # Height of temperature sensor above local ground
        u'TMPR_SNSR_HGHT_WATR'              : 'f4'      , # Height of temperature sensor above water surface
        u'SRFC_AIR_TMPR'                    : 'f4'      , # Surface air temperature
        u'WET_BULB_RCRDG_IDNY'              : 'i4'      , # Flag for presence of web-bulb measurement (1 available 0 not)
        u'WET_BULB_TMPR'                    : 'f4'      , # Surface wet bulb temperature
        u'SRFC_DEW_PONT_TMPR'               : 'f4'      , # Surface dew point temperature
        u'SRFC_RLTV_HUMDY'                  : 'f4'      , # Surface relative humidity
        u'SOIL_TMPR_FLAG'                   : 'i4'      , # Flag for presence of soil temps (1 available 0 not)
        u'DPTH_BELW_SRFC'                   : 'f4'      , # Depth below surface
        u'SOIL_TMPR'                        : 'f4'      , # Soil temperature
        u'VSBLY_FLAG'                       : 'i4'      , # Flag for presence of visibility (1 available 0 not)
        u'VSBLY_SNSR_HGHT'                  : 'f4'      , # Height of visibility sensor above local ground
        u'VSBLY_SNSR_HGHT_WATR'             : 'f4'      , # Height of visibility sensor above water surface
        u'HRZL_VSBLY'                       : 'f4'      , # Horizontal visibility
        u'VSBLY_ATRT'                       : 'i4'      , # Attribute of horizontal visibility
        u'ICE_FLAG'                         : 'i4'      , # Flag for presence of ice data (1 available 0 not)
        u'ICE_THKNS'                        : 'f4'      , # Thickness of ice deposit
        u'ICE_ACRTN_RATE'                   : 'i4'      , # Ice accretion rate
        u'ORGN_SHIP_ICE_ACRN'               : 'i4'      , # Cause of ice accretion
        u'CNTRN_SEA_ICE'                    : 'i4'      , # Sea ice concentration
        u'LAND_ORGN_ICE_AMNT'               : 'i4'      , # Amount and type of ice
        u'SEA_ICE_STTN'                     : 'i4'      , # Ice situation
        u'SEA_ICE_AGE'                      : 'i4'      , # Ice development
        u'SEA_ICE_EDGE_DRCTN'               : 'f4'      , # Bearing of ice edge
        u'TMPR_SALNY_RCRDG_IDNY'            : 'i4'      , # Sea temperature measurement method
        u'DPTH_SEA_TMPR_MESRT'              : 'f4'      , # Depth below sea surface
        u'SEA_SRFC_TMPR'                    : 'f4'      , # Sea surface temperature
        u'WAVE_DRCTN'                       : 'f4'      , # Direction of waves
        u'WAVE_PERD'                        : 'f4'      , # Period of waves
        u'WAVE_HGHT'                        : 'f4'      , # Height of waves
        u'WIND_WAVE_DRCTN'                  : 'f4'      , # Direction of wind waves
        u'WIND_WAVE_PERD'                   : 'f4'      , # Period of wind waves
        u'WIND_WAVE_HGHT'                   : 'f4'      , # Height of wind waves
        u'SWEL_WAVE_DRCTN'                  : 'f4'      , # Direction of swell waves
        u'SWEL_WAVE_PERD'                   : 'f4'      , # Period of swell waves
        u'SWEL_WAVE_HGHT'                   : 'f4'      , # Height of swell waves
        u'GRND_STTE_FLAG'                   : 'i4'      , # Flag for presence of ground state (1 available 0 not)
        u'GRND_STTE_MESRT_MTHD'             : 'i4'      , # Ground state measurement method
        u'GRND_STTE_IDNY'                   : 'i4'      , # State of ground
        u'SNOW_DPTH_MESRT_MTHD'             : 'i4'      , # Snow depth measurement method
        u'SNOW_DPTH'                        : 'f4'      , # Snow depth
        u'SEA_STTE_IDNY'                    : 'i4'      , # State of the sea
        u'SEA_VSBLY'                        : 'f4'      , # Seaward visibility
        u'CLOD_FLAG'                        : 'i4'      , # Flag for presence of total cloud (1 available 0 not)
        u'TOTL_CLOD_AMNT'                   : 'f4'      , # Total cloud amount
        u'TOTL_CLOD_VRTL_SGNC'              : 'i4'      , # Total cloud vertical significance
        u'LWST_CLOD_AMNT'                   : 'i4'      , # Amount of lowest cloud
        u'LWST_CLOD_BASE_HGHT'              : 'f4'      , # Base of lowest cloud
        u'LOW_CLOD_TYPE'                    : 'i4'      , # Low cloud type
        u'MEDM_CLOD_TYPE'                   : 'i4'      , # Medium cloud type
        u'HIGH_CLOD_TYPE'                   : 'i4'      , # High cloud type
        u'CLOD_RPLTN_CONT'                  : 'i4'      , # Number of cloud types
        u'CLOD_VRTL_SGNC'                   : 'i4'      , # Cloud vertical significance
        u'CLOD_AMNT'                        : 'i4'      , # Cloud amount
        u'CLOD_TYPE'                        : 'i4'      , # Cloud type
        u'CLOD_BASE_HGHT'                   : 'f4'      , # Cloud base height
        u'CLOD_BASE_ATRT'                   : 'i4'      , # Attribute of cloud base height
        u'BELW_STTN_CLOD_RPLTN_CONT'        : 'i4'      , # Number of cloud levels
        u'BELW_STTN_CLOD_VRTL_SGNC'         : 'i4'      , # Vertical significance
        u'BELW_STTN_CLOD_AMNT'              : 'i4'      , # Cloud amount
        u'BELW_STTN_CLOD_TYPE'              : 'i4'      , # Cloud type
        u'BELW_STTN_CLOD_TOP_HGHT'          : 'f4'      , # Cloud top height
        u'BELW_STTN_CLOD_TOP_TYPE'          : 'i4'      , # Cloud top description
        u'CLOD_DRFT_FLAG'                   : 'i4'      , # Flag for presence of cloud drift  (1 available 0 not)
        u'CLOD_DRFT_SGNC'                   : 'i4'      , # Vertical significance
        u'CLOD_DRCTN'                       : 'f4'      , # True direction from which clouds are moving
        u'CLOD_LCTN_FLAG'                   : 'i4'      , # Flag for presence of cloud direction  (1 available 0 not)
        u'CLOD_AZMH'                        : 'f4'      , # Bearing of cloud
        u'CLOD_ELVTN'                       : 'f4'      , # Elevation of cloud
        u'CLOD_LAYR_TYPE'                   : 'i4'      , # Cloud type
        u'WTHR_FLAG'                        : 'i4'      , # Flag for presence of present/past wx (1 available 0 not)
        u'CRNT_WTHR_TYPE'                   : 'i4'      , # Present weather
        u'PAST_WTHR_PERD'                   : 'f4'      , # Past weather period (1 hours 2 minutes)
        u'PRMY_PAST_WTHR_TYPE'              : 'i4'      , # Primary past weather type
        u'SCNY_PAST_WTHR_TYPE'              : 'i4'      , # Secondary past weather type
        u'Q24HOUR_PRCTN_AMNT'               : 'f4'      , # Total precipitation in past 24 hours
        u'PRCTN_FLAG'                       : 'i4'      , # Flag for presence of precipitation (1 available 0 not)
        u'PRCTN_SNSR_HGHT'                  : 'f4'      , # Height of precipitation sensor above local ground
        u'PRCTN_SNSR_HGHT_WATR'             : 'f4'      , # Height of precipitation sensor above water surface
        u'PRCTN_MESRT_MTHD'                 : 'i4'      , # Method of precipitation measurement
        u'LQID_WATR_MESRT_MTHD'             : 'i4'      , # Method of measurement of liquid content of precipitation
        u'PRCTN_PERD'                       : 'i4'      , # Precipitation period (1 hours 2 minutes)
        u'PRCTN_AMNT'                       : 'f4'      , # Total precipitation
        u'PRCTN_RATE_FLAG'                  : 'i4'      , # Flag for presence of precipitation  (1 available 0 not)
        u'PRCTN_RATE_PERD'                  : 'f4'      , # Precipitation measurement period
        u'PRCTN_RATE'                       : 'f4'      , # Intensity if precipitation
        u'PRCTN_ELMT_SIZE'                  : 'f4'      , # Size of precipitating element
        u'PRCTN_DRTN_FLAG'                  : 'i4'      , # Flag for presence of precipitation (1 available 0 not)
        u'PRCTN_TYPE'                       : 'i4'      , # Type of precipitation
        u'PRCTN_CHRTR'                      : 'i4'      , # Character of precipitation
        u'PRCTN_DRTN'                       : 'f4'      , # Duration of precipitation
        u'OTHR_WTHR'                        : 'i4'      , # Other weather phenomena
        u'WTHR_INTSY'                       : 'i4'      , # Intensity of phenomena
        u'OBSCN'                            : 'i4'      , # Obscuration
        u'OBSCN_CHRTR'                      : 'i4'      , # Character of obscuration
        u'NMBR_FLSHS'                       : 'i4'      , # No of flashes in last 10 minutes
        u'WIND_SNSR_HGHT'                   : 'f4'      , # Height of wind sensor above local ground
        u'WIND_SNSR_HGHT_WATR'              : 'f4'      , # Height of wind sensor above water surface
        u'SRFC_WIND_SPED_RCRDG_IDNY'        : 'i4'      , # Wind measurement instrument type
        u'SRFC_WIND_PERD'                   : 'f4'      , # Wind observation period (usually 10)
        u'SRFC_WIND_DRCTN'                  : 'f4'      , # Surface wind direction
        u'SRFC_WIND_SPED'                   : 'f4'      , # Surface wind speed
        u'SRFC_GUST_PERD'                   : 'f4'      , # Gust observation period
        u'MXMM_SRFC_GUST_DRCTN'             : 'f4'      , # Direction of maximum gust
        u'MXMM_SRFC_GUST_SPED'              : 'f4'      , # Maximum gust speed
        u'MXMM_MEAN_WIND_PERD'              : 'f4'      , # Mean wind observation period
        u'MXMM_MEAN_WIND_SPED'              : 'f4'      , # Maximum wind speed (10 minute mean)
        u'VRBL_WIND_DRCTN_ANTI'             : 'f4'      , # Extreme direction of variable wind anticlockwise
        u'VRBL_WIND_DRCTN_CLKW'             : 'f4'      , # Extreme direction of variable wind clockwise
        u'EXTM_TMPR_SNSR_HGHT'              : 'f4'      , # Height of temperature sensor above local ground
        u'EXTM_TMPR_SNSR_HGHT_WATR'         : 'f4'      , # Height of temperature sensor above water surface
        u'EXTM_SRFC_AIR_TMPR_PERD'          : 'f4'      , # Observing period
        u'MXMM_SRFC_AIR_TMPR'               : 'f4'      , # Maximum air temperature
        u'MNMM_SRFC_AIR_TMPR'               : 'f4'      , # Minimum air temperature
        u'EXTM_PERD_TMPR_SNSR_HGHT'         : 'f4'      , # Height of temperature sensor above local ground
        u'MXMM_SRFC_AIR_TMPR_STRT'          : 'f4'      , # Start of Tmax observation period
        u'MXMM_SRFC_AIR_TMPR_END'           : 'f4'      , # End of Tmax observation period
        u'MXMM_PERD_SRFC_AIR_TMPR'          : 'f4'      , # Maximum air temperature in period
        u'MNMM_SRFC_AIR_TMPR_STRT'          : 'f4'      , # Start of Tmin observation period
        u'MNMM_SRFC_AIR_TMPR_END'           : 'f4'      , # End of Tmin observation period
        u'MNMM_PERD_SRFC_AIR_TMPR'          : 'f4'      , # Minimum air temperature in period
        u'Q12HOUR_MNMM_GRND_TMPR'           : 'f4'      , # Minimum ground temperature in past 12 hours
        u'GRND_TMPR_SNSR_HGHT'              : 'f4'      , # Height of sensor above local ground
        u'EXTM_GRND_TMPR_PERD'              : 'f4'      , # Ground temperature observing period
        u'MNMM_GRND_TMPR'                   : 'f4'      , # Minimum ground temperature
        u'EVPTN_MESRT_MTHD'                 : 'i4'      , # Method of evaporation measurement
        u'EVPTN_PERD_STRT'                  : 'f4'      , # Start of evap observation period
        u'EVPTN_PERD_END'                   : 'f4'      , # End of evap observation period
        u'EVPTN_FLAG'                       : 'i4'      , # Flag for presence of evaporation (1 available 0 not)
        u'EVPTN_PERD'                       : 'i4'      , # Evaporation observation period (1 hours 2 minutes)
        u'EVPTN_INST_TYPE'                  : 'i4'      , # Type of evaporation instrumentation
        u'EVPTN_AMNT'                       : 'f4'      , # Evaporation/evapotranspiration
        u'SUN_FLAG'                         : 'i4'      , # Flag for presence of sunshine data (1 available 0 not)
        u'SUN_PERD'                         : 'i4'      , # Observation period (1 hours 2 minutes)
        u'SUN_AMNT'                         : 'f4'      , # Total sunshine
        u'RADTN_FLAG'                       : 'i4'      , # Flag for presence of radiation (1 available 0 not)
        u'RADTN_PERD'                       : 'i4'      , # Radiation observation period (1 hours 2 minutes)
        u'LONG_WAVE_RADTN'                  : 'f4'      , # Long-wave radiation
        u'SHRT_WAVE_RADTN'                  : 'f4'      , # Short-wave radiation
        u'NET_RADTN'                        : 'f4'      , # Net radiation
        u'GLOBL_SOLR_RADTN'                 : 'f4'      , # Global solar radiation
        u'DIFS_SOLR_RADTN'                  : 'f4'      , # Diffuse solar radiation
        u'DRCT_SOLR_RADTN'                  : 'f4'      , # Direct solar radiatiom
        u'TMPR_CHNG_FLAG'                   : 'i4'      , # Flag for presence of temperature change data (1 available 0 not)
        u'SRFC_AIR_TMPR_CHNG_STRT'          : 'f4'      , # Time displacement of start of period
        u'SRFC_AIR_TMPR_CHNG_END'           : 'f4'      , # Time displacement of end of period
        u'SRFC_AIR_TMPR_CHNG'               : 'f4'      , # Temperature change over period
        u'STATS_FLAG'                       : 'i4'      , # Flag for presence of first order statistics (1 available 0 not)
        u'STND_DVTN_PERD'                   : 'f4'      , # Time period for statistics
        u'PESR_STND_DVTN'                   : 'f4'      , # Pressure standard deviation
        u'WIND_DRCTN_STND_DVTN'             : 'f4'      , # Wind direction standard deviation
        u'WIND_SPED_STND_DVTN'              : 'f4'      , # Wind speed standard deviation
        u'TMPR_STND_DVTN'                   : 'f4'      , # Temperature standard deviation
        u'RLTV_HUMDY_STND_DVTN'             : 'f4'      , # Relative humidity standard deviation
        u'AWS_QLTY'                         : 'i4'      , # (AWS) Quality information
        u'AWS_STTS'                         : 'i4'      , # (AWS) Internal measurement status
        u'DOWN_STRM_WATR_LEVL'              : 'f4'      , # Downstream water level
        u'WATR_TMPR'                        : 'f4'      , # Water temperature
        u'Q1HOUR_PRCTN_AMNT'                : 'f4'      , # Precipitation in past hour
        u'MXMM_WATR_LEVL'                   : 'f4'      , # Maximum water level
    },
