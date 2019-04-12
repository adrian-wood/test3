# Merge these code fragments with subtypes.py in the metdb module

CREP_ELEMENTS = {
         u'OLCI' : u''
                }
DTYPE_MAPS = {
    "OLCI":{ 
        u'STLT_IDNY'                        : 'i4'      , # Satellite Identifier
        u'STLT_INSTS'                       : 'i4'      , # Satellite instruments
        u'STTN_ACQSTN'                      : 'S20'     , # Station acquisition
        u'SFTWR_ID'                         : 'S14'     , # Software Id & version no.
        u'YEAR'                             : 'i4'      , # Year
        u'MNTH'                             : 'i4'      , # Month
        u'DAY'                              : 'i4'      , # Day
        u'HOUR'                             : 'i4'      , # Hour
        u'MINT'                             : 'i4'      , # Minute
        u'SCND'                             : 'i4'      , # Second
        u'RCPT_YEAR'                        : 'i4'      , # Time of receipt year
        u'RCPT_MNTH'                        : 'i4'      , # Time of receipt month
        u'RCPT_DAY'                         : 'i4'      , # Time of receipt day
        u'RCPT_HOUR'                        : 'i4'      , # Time of receipt hour
        u'RCPT_MINT'                        : 'i4'      , # Time of receipt minute
        u'LTTD'                             : 'f4'      , # Latitude
        u'LNGD'                             : 'f4'      , # Longitude
        u'ORBT_NMBR'                        : 'i4'      , # Orbit number
        u'SOLR_ZNTH_ANGL'                   : 'f4'      , # Solar zenith angle
        u'SOLR_AZMH'                        : 'f4'      , # Solar azimuth
        u'VWNG_ZNTH_ANGL'                   : 'f4'      , # Viewing zenith angle
        u'VWNG_AZMH_ANGL'                   : 'f4'      , # Viewing azimuth
        u'CLOD_TOP_PESR'                    : 'f4'      , # Cloud top pressure
        u'CLOD_OPTCL_THKNS'                 : 'i4'      , # Cloud optical thickness
        u'MSL_PESR'                         : 'f4'      , # Mean sea level pressure
        u'TOTL_WATR_VAPR'                   : 'f4'      , # Total column water vapour
    },
