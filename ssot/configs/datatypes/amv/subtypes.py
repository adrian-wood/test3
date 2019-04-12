# Merge these code fragments with subtypes.py in the metdb module

CREP_ELEMENTS = {
         u'AMV' : u''
                }
DTYPE_MAPS = {
    "AMV":{ 
        u'ORGNG_GNTRG_CNTR'                 : 'i4'      , # Identification of originating/generating centre
        u'ORGNG_SUBCNTR'                    : 'i4'      , # Identification of originating/generating sub-centre
        u'SFTWR_ID_VER'                     : 'S12'     , # Software identification and version number
        u'DTBS_IDNY'                        : 'i4'      , # Database identification
        u'STLT_IDNY'                        : 'i4'      , # Satellite Identifier
        u'STLT_CHNL_CNTR_FRQY'              : 'f4'      , # Satellite Channel Centre Frequency
        u'PLTM_MOTN_DRCTN'                  : 'f4'      , # Direction of motion of moving observing platform
        u'CROSS_TRCK_RSLN'                  : 'f4'      , # Cross-track resolution
        u'LONG_TRCK_RSLN'                   : 'f4'      , # Along-track resolution
        u'NADR_SGMT_SIZE_X'                 : 'f4'      , # Segment Size at Nadir in x-direction (target box size)
        u'NADR_SGMT_SIZE_Y'                 : 'f4'      , # Segment Size at Nadir in y-direction (target box size)
        u'WIND_PRCSG_MTHD'                  : 'i4'      , # Wind Processing Method
        u'TRCR_CRLN_MTHD'                   : 'i4'      , # Tracer Correlation Method
        u'STLT_DRVD_WIND_CMPT_MTHD'         : 'i4'      , # Satellite Derived Wind Computation Method
        u'SRFC_TYPE'                        : 'i4'      , # Land/Sea Qualifier
        u'DAY_NGHT_QLFR'                    : 'i4'      , # Day/Night Qualifier
        u'GRID_PONT_IDNY'                   : 'i4'      , # Grid point identifier
        u'LTTD'                             : 'f4'      , # Latitude (high accuracy)
        u'LNGD'                             : 'f4'      , # Longitude (high accuracy)
        u'YEAR'                             : 'i4'      , # year
        u'MNTH'                             : 'i4'      , # month
        u'DAY'                              : 'i4'      , # day
        u'HOUR'                             : 'i4'      , # hour
        u'MINT'                             : 'i4'      , # minute
        u'SCND'                             : 'i4'      , # second
        u'TIME_PERD'                        : 'i4'      , # Long Time Period or Displacement
        u'WIND_DRCTN'                       : 'f4'      , # Wind Direction
        u'WIND_SPED'                        : 'f4'      , # Wind Speed
        u'WIND_U'                           : 'f4'      , # Wind u-component
        u'WIND_V'                           : 'f4'      , # Wind v-component
        u'HGHT_ASGT_MTHD'                   : 'i4'      , # Extended height assignment method
        u'LEVL_PESR'                        : 'f4'      , # Pressure
        u'LEVL_AIR_TMPR'                    : 'f4'      , # Temperature
        u'AGL_CLOD_TOP_HGHT'                : 'f4'      , # Height of Top Cloud
        u'STLT_ZNTH_ANGL'                   : 'f4'      , # Satellite Zenith Angle
        u'OBSVN_NMBR'                       : 'i4'      , # Observation Sequence Number
        u'ALTV_RPLTN_CONT'                  : 'i4'      , # 
        u'ALTV_HGHT_ASGT_MTHD'              : 'i4'      , # Extended height assignment method
        u'ALTV_LEVL_PESR'                   : 'f4'      , # Pressure
        u'ALTV_LEVL_AIR_TMPR'               : 'f4'      , # Temperature
        u'ALTV_CLOD_TOP_HGHT'               : 'f4'      , # Height of Top Cloud
        u'IMGE_RPLTN_CONT'                  : 'i4'      , # 
        u'IMGE_TIME_DSPLT'                  : 'f4'      , # Long Time Period or Displacement
        u'IMGE_STLT_CLAS'                   : 'i4'      , # Satellite Classification
        u'IMGE_STLT_IDNY'                   : 'i4'      , # Satellite Identifier
        u'IMGE_STLT_INST'                   : 'i4'      , # Satellite Instruments
        u'IMGE_CHNL_NMBR'                   : 'i4'      , # Channel Number
        u'IMGE_CNTR_FRQY'                   : 'f4'      , # Satellite Channel Centre Frequency
        u'IMGE_ORBT_NMBR'                   : 'i4'      , # Orbit Number
        u'IMGE_STLT_ZNTH_ANGL'              : 'f4'      , # Satellite Zenith Angle
        u'IMGE_AZMH_ANGL'                   : 'f4'      , # Bearing or Azimuth
        u'IMGE_HGHT_ASGT_MTHD'              : 'i4'      , # Extended height assignment method
        u'IMGE_PESR'                        : 'f4'      , # Pressure
        u'IMGE_AIR_TMPR'                    : 'f4'      , # Temperature
        u'IMGE_CLOD_TOP_HGHT'               : 'f4'      , # Height of Top Cloud
        u'CMPT_RPLTN_CONT'                  : 'i4'      , # Number of intermediate wind vectors retrieved
        u'CMPT_VCTR_STRT_DSPLT'             : 'f4'      , # Difference between the image timestamp and the nominal validity time reported before (start of vector)
        u'CMPT_VCTR_TIME_DFRC'              : 'f4'      , # Time Difference between the two images
        u'CMPT_VCTR_LTTD'                   : 'f4'      , # latitude of the derived intermediate wind vector
        u'CMPT_VCTR_LNGD'                   : 'f4'      , # longitude of the derived intermediate wind vector
        u'CMPT_VCTR_U_WIND'                 : 'f4'      , # u-component of the derived intermediate wind vector
        u'CMPT_VCTR_V_WIND'                 : 'f4'      , # v-component of the derived intermediate wind vector
        u'CMPT_VCTR_TRCK_CRLTN'             : 'i4'      , # Tracking correlation of vector
        u'CMPT_CFCNT_VRTN'                  : 'i4'      , # Coefficient of variation
        u'FRST_ORDR_STAT_RPLTN_CONT'        : 'i4'      , # Number of statistics provided
        u'FRST_ORDR_STAT_TYPE'              : 'i4'      , # Type of statistic
        u'U_WIND_STAT'                      : 'f4'      , # u-component
        u'V_WIND_STAT'                      : 'f4'      , # v-component
        u'AXIS_ERRR_RPLTN_CONT'             : 'i4'      , # Number of uncertainties reported
        u'X_AXIS_ERRR'                      : 'f4'      , # x-axis error ellipse major component
        u'Y_AXIS_ERRR'                      : 'f4'      , # y-axis error ellipse major component
        u'X_AXIS_ANGL_ERRR'                 : 'f4'      , # Angle of x-axis in error ellipse
        u'FRST_GUES_ORGNG_CNTR'             : 'i4'      , # Generating centre of the forecast data
        u'FRST_GUES_U_WIND'                 : 'f4'      , # first guess u-component of the model wind vector
        u'FRST_GUES_V_WIND'                 : 'f4'      , # first guess v-component of the model wind vector
        u'FRST_GUES_PESR'                   : 'f4'      , # pressure associated with first guess winds
        u'FRCT_U_WIND'                      : 'f4'      , # forecast u-component of the model wind vector
        u'FRCT_V_WIND'                      : 'f4'      , # forecast v-component of the model wind vector
        u'FRCT_PESR'                        : 'f4'      , # pressure associated with forecast winds
        u'BEST_FIT_LEVL_U_WIND'             : 'f4'      , # u-component of forecast wind at the level of best fit
        u'BEST_FIT_LEVL_V_WIND'             : 'f4'      , # v-component of forecast wind at the level of best fit
        u'BEST_FIT_LEVL_PESR'               : 'f4'      , # pressure at the level of best fit
        u'PRCT_CNFC_RPLTN_CONT'             : 'i4'      , # 
        u'GNTRG_APLCTN'                     : 'i4'      , # Generating Application (in 3 10 067)
        u'STND_GNTRG_APLCTN'                : 'i4'      , # Generating Application (in 3 10 077)
        u'PRCT_CNFC'                        : 'f4'      , # Percent Confidence
        u'U_WIND_UNCRT'                     : 'f4'      , # Uncertainty of u-component
        u'V_WIND_UNCRT'                     : 'f4'      , # Uncertainty of v-component
        u'PESR_UNCRT'                       : 'f4'      , # Uncertainty of pressure
        u'AMV_QLTY_FLAG'                    : 'i4'      , # AMV Quality Flag
        u'SGMT_CLOD_AMNT'                   : 'f4'      , # Cloud amount computed over segment
        u'SGMT_CLOD_TYPE'                   : 'i4'      , # Dominant cloud type over segment
        u'SGMT_CLOD_PHS'                    : 'i4'      , # Dominant cloud phase over segment
        u'CLOD_FRST_ORDR_RPLTN_CONT'        : 'i4'      , # 
        u'CLOD_FRST_ORDR_STAT_TYPE'         : 'i4'      , # Type of statistic
        u'CLOD_TOP_PESR_STAT'               : 'f4'      , # Pressure at the top of cloud
        u'CLOD_TOP_TMPR_UNCRT'              : 'f4'      , # Uncertainty of temperature associated with cloud top
        u'CLOD_TOP_PESR_UNCRT'              : 'f4'      , # Uncertainty of pressure associated with cloud top
        u'OPTML_ESTMN_COST'                 : 'i4'      , # Optimal estimation cost
        u'CLOD_TOP_PESR'                    : 'f4'      , # Pressure at top of cloud
        u'CLOD_TOP_HGHT'                    : 'f4'      , # Height of top of cloud
        u'CLOD_OPTCL_THKNS'                 : 'f4'      , # Cloud Optical Thickness
        u'ICE_LQID_WATR_PATH'               : 'f4'      , # Ice/liquid water path
        u'CLOD_PRTCL_SIZE'                  : 'f4'      , # Cloud Particle Size
        u'CLOD_EMSY'                        : 'f4'      , # Cloud Emissivity
        u'RCPT_YEAR'                        : 'i4'      , # time of receipt year
        u'RCPT_MNTH'                        : 'i4'      , # time of receipt month
        u'RCPT_DAY'                         : 'i4'      , # time of receipt day
        u'RCPT_HOUR'                        : 'i4'      , # time of receipt hour
        u'RCPT_MINT'                        : 'i4'      , # time of receipt minute
    },
