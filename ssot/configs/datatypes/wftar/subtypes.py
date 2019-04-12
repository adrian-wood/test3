# Merge these code fragments with subtypes.py in the metdb module

CREP_ELEMENTS = {
         u'WFTAR' : u''
                }
DTYPE_MAPS = {
    "WFTAR":{ 
        u'SITE_NAME'                        : 'S32'     , # Site Name
        u'SSPA_IDNY'                        : 'S8'      , # SSPA ID
        u'LTTD'                             : 'f4'      , # Latitude
        u'LNGD'                             : 'f4'      , # Longitude
        u'STTN_HGHT'                        : 'f4'      , # Station Height
        u'YEAR'                             : 'i4'      , # year
        u'MNTH'                             : 'i4'      , # month
        u'DAY'                              : 'i4'      , # day
        u'HOUR'                             : 'i4'      , # hour
        u'MINT'                             : 'i4'      , # minute
        u'SCND'                             : 'f4'      , # second
        u'RPLTN_CONT'                       : 'i4'      , # Replication (Number of Site Observations)
        u'OBSVN_ID'                         : 'S8'      , # Observation ID
        u'STTN_BMTR_HGHT'                   : 'f4'      , # Pressure Sensor Height
        u'STTN_PESR'                        : 'f4'      , # Station Pressure
        u'STTN_HUMDY_SNSR_HGHT'             : 'f4'      , # Humidity Sensor Height
        u'RLTV_HUMDY'                       : 'f4'      , # Relative Humidity
        u'STTN_TMPR_SNSR_HGHT'              : 'f4'      , # Temperature Sensor Height
        u'AIR_TMPR'                         : 'f4'      , # Air Temperature
        u'STTN_WIND_SNSR_HGHT'              : 'f4'      , # Wind Sensor Height
        u'WIND_DRCTN'                       : 'f4'      , # Wind Direction
        u'WIND_SPED'                        : 'f4'      , # Wind Speed
        u'WIND_SPED_MIN'                    : 'f4'      , # Minimum Wind Speed
        u'WIND_SPED_MAX'                    : 'f4'      , # Maximum Wind Speed
        u'RCPT_YEAR'                        : 'i4'      , # time of receipt year
        u'RCPT_MNTH'                        : 'i4'      , # time of receipt month
        u'RCPT_DAY'                         : 'i4'      , # time of receipt day
        u'RCPT_HOUR'                        : 'i4'      , # time of receipt hour
        u'RCPT_MINT'                        : 'i4'      , # time of receipt minute
    },
