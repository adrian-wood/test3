# Merge these code fragments with subtypes.py in the metdb module

CREP_ELEMENTS = {
         u'KMAWINDS' : u''
                }
DTYPE_MAPS = {
    "KMAWINDS":{ 
        u'STLT_IDNY'                        : 'i4'      , # Satellite Identifier
        u'YEAR'                             : 'i4'      , # year
        u'MNTH'                             : 'i4'      , # month
        u'DAY'                              : 'i4'      , # day
        u'HOUR'                             : 'i4'      , # hour
        u'MINT'                             : 'i4'      , # minute
        u'SCND'                             : 'i4'      , # second
        u'LTTD'                             : 'f4'      , # Latitude
        u'LNGD'                             : 'f4'      , # Longitude
        u'RCPT_YEAR'                        : 'i4'      , # time of receipt year
        u'RCPT_MNTH'                        : 'i4'      , # time of receipt month
        u'RCPT_DAY'                         : 'i4'      , # time of receipt day
        u'RCPT_HOUR'                        : 'i4'      , # time of receipt hour
        u'RCPT_MINT'                        : 'i4'      , # time of receipt minute
        u'DATA_SLCTN'                       : 'i4'      , # 1 for IR cloud drift winds, 2 for winds derived from visible channels or 3 for water vapour winds
        u'ORGNG_GNTRG_CNTR'                 : 'i4'      , # Originating Centre
        u'STLT_CLAS'                        : 'i4'      , # Satellite Classification
        u'NADR_SGMT_SIZE_X'                 : 'f4'      , # Segment Size at Nadir (x-dir)
        u'NADR_SGMT_SIZE_Y'                 : 'f4'      , # Segment Size at Nadir (y-dir)
        u'STLT_INST'                        : 'i4'      , # Satellite Processing Instruments
        u'CLOD_MOTN_MTHD'                   : 'i4'      , # Cloud motion computational method
        u'LEVL_PESR'                        : 'f4'      , # Cloud Top Pressure
        u'LEVL_WIND_DRCTN'                  : 'f4'      , # Cloud Top Wind Direction
        u'LEVL_WIND_SPED'                   : 'f4'      , # Cloud Top Wind Speed
        u'CNTR_FRQY'                        : 'f4'      , # Channel Central Frequency
        u'BAND_WIDH'                        : 'f4'      , # Channel Band Width
        u'LEVL_AIR_TMPR'                    : 'f4'      , # Coldest cluster temperature
        u'HGHT_ASGT_MTHD'                   : 'i4'      , # Height assignment method
        u'TRCR_CRLN_MTHD'                   : 'i4'      , # Tracer correlation method
        u'SRFC_TYPE'                        : 'i4'      , # Surface type
        u'STLT_ZNTH_ANGL'                   : 'f4'      , # Satellite Zenith Angle
        u'FRST_GUES_INFMN_ORGN'             : 'i4'      , # Origin of 1st guess information
        u'CMPT_COLTN_STRT_YEAR'             : 'i4'      , # Component Collection Start Year
        u'CMPT_COLTN_STRT_MNTH'             : 'i4'      , # Component Collection Start Month
        u'CMPT_COLTN_STRT_DAY'              : 'i4'      , # Component Collection Start Day
        u'CMPT_COLTN_STRT_HOUR'             : 'i4'      , # Component Collection Start Hour
        u'CMPT_COLTN_PERD'                  : 'i4'      , # Component Collection Period
        u'CMPT_VCTR_STRT_HOUR'              : 'i4'      , # Component Vector Start Hour
        u'CMPT_VCTR_STRT_MINT'              : 'i4'      , # Component Vector Start Minute
        u'CMPT_VCTR_STRT_SCND'              : 'i4'      , # Component Vector Start Second
        u'CMPT_VCTR_END_HOUR'               : 'i4'      , # Component Vector End Hour
        u'CMPT_VCTR_END_MINT'               : 'i4'      , # Component Vector End Minute
        u'CMPT_VCTR_END_SCND'               : 'i4'      , # Component Vector End Second
        u'CMPT_VCTR_WIND_DRCTN'             : 'f4'      , # Component Vector Wind Direction
        u'CMPT_VCTR_WIND_SPED'              : 'f4'      , # Component Vector Wind Speed
        u'ALTV_HGHT_ASGT_MTHD'              : 'i4'      , # Height assignment method
        u'ALTV_LEVL_PESR'                   : 'f4'      , # Pressure
        u'ALTV_LEVL_AIR_TMPR'               : 'f4'      , # Temperature
        u'PRCT_CNFC1'                       : 'f4'      , # Percent Confidence
        u'QLTY_CNTL1'                       : 'i4'      , # Manual-auto quality control
        u'NMNL_CNFC_THRD1'                  : 'f4'      , # Nominal confidence threshold
        u'PRCT_CNFC2'                       : 'f4'      , # Percent Confidence
        u'QLTY_CNTL2'                       : 'i4'      , # Manual-auto quality control
        u'NMNL_CNFC_THRD2'                  : 'f4'      , # Nominal confidence threshold
    },
