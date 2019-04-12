# Merge these code fragments with subtypes.py in the metdb module

CREP_ELEMENTS = {
         u'GMI' : u''
                }
DTYPE_MAPS = {
    "GMI":{ 
        u'STLT_IDNY'                        : 'i4'      , # Satellite Identifier
        u'STLT_INSTS'                       : 'i4'      , # Satellite Instruments
        u'STLT_LTTD'                        : 'f4'      , # Satellite Latitude (high accuracy)
        u'STLT_LNGD'                        : 'f4'      , # Satellite Longitude (high accuracy)
        u'HGHT'                             : 'f4'      , # Height or Altitude
        u'SPFT_ROLL'                        : 'f4'      , # Spacecraft Roll
        u'SPFT_PTCH'                        : 'f4'      , # Spacecraft Pitch
        u'SPFT_YAW'                         : 'f4'      , # Spacecraft Yaw
        u'SCAN_LINE_NMBR'                   : 'i4'      , # Scan Line Number
        u'NMBR_SCAN_LINES'                  : 'i4'      , # Number of Scan Lines
        u'YEAR'                             : 'i4'      , # year
        u'MNTH'                             : 'i4'      , # month
        u'DAY'                              : 'i4'      , # day
        u'HOUR'                             : 'i4'      , # hour
        u'MINT'                             : 'i4'      , # minute
        u'SCND'                             : 'f4'      , # second (microsecond accuracy)
        u'OBSVN_LTTD'                       : 'f4'      , # Observation Latitude (high accuracy)
        u'OBSVN_LNGD'                       : 'f4'      , # Observation Longitude (high accuracy)
        u'CHNL_NMBR'                        : 'i4'      , # Channel Number
        u'STLT_CHNL_CNTR_FRQY'              : 'f4'      , # Satellite Channel Centre Frequency
        u'ANTA_PLRZN'                       : 'i4'      , # Antenna Polarization
        u'GMI_QLTY_FLAG'                    : 'i4'      , # GMI Quality Flag
        u'STLT_ZNTH_ANGL'                   : 'f4'      , # Satellite Zenith Angle
        u'SUN_GLNT_ANGL'                    : 'f4'      , # Sun Glint Angle
        u'BRGTS_TMPR'                       : 'f4'      , # Brightness Temperature
        u'RCPT_YEAR'                        : 'i4'      , # time of receipt year
        u'RCPT_MNTH'                        : 'i4'      , # time of receipt month
        u'RCPT_DAY'                         : 'i4'      , # time of receipt day
        u'RCPT_HOUR'                        : 'i4'      , # time of receipt hour
        u'RCPT_MINT'                        : 'i4'      , # time of receipt minute
        u'DATA_SLCTN'                       : 'i4'      , # Channel Group: 1=low-frequency group, 2=high-frequency group
    },
