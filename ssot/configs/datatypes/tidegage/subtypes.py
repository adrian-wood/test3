# Merge these code fragments with subtypes.py in the metdb module

CREP_ELEMENTS = {
         u'TIDEGAGE' : u''
                }
DTYPE_MAPS = {
    "TIDEGAGE":{ 
        u'STTN_NAME'                        : 'S20'     , # Station Name
        u'YEAR'                             : 'i4'      , # year
        u'MNTH'                             : 'i4'      , # month
        u'DAY'                              : 'i4'      , # day
        u'HOUR'                             : 'i4'      , # hour
        u'MINT'                             : 'i4'      , # minute
        u'LTTD'                             : 'f4'      , # Latitude
        u'LNGD'                             : 'f4'      , # Longitude
        u'TIDE_ELVTN'                       : 'f4'      , # Tide Elevation
        u'RCPT_YEAR'                        : 'i4'      , # time of receipt year
        u'RCPT_MNTH'                        : 'i4'      , # time of receipt month
        u'RCPT_DAY'                         : 'i4'      , # time of receipt day
        u'RCPT_HOUR'                        : 'i4'      , # time of receipt hour
        u'RCPT_MINT'                        : 'i4'      , # time of receipt minute
    },
