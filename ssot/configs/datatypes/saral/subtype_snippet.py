# Use these code snippets in your python program to update the metdb module



metdb.subtypes.DTYPE_MAPS["SARAL"] = {
           
             u'YEAR' : 'i4'  ,# year
           
             u'MNTH' : 'i4'  ,# month
           
             u'DAY' : 'i4'  ,# day
           
             u'HOUR' : 'i4'  ,# hour
           
             u'MINT' : 'i4'  ,# minute
           
             u'SCND' : 'i4'  ,# second
           
             u'LTTD' : 'f4'  ,# Latitude
           
             u'LNGD' : 'f4'  ,# Longitude
           
             u'RCPT_YEAR' : 'i4'  ,# time of receipt year
           
             u'RCPT_MNTH' : 'i4'  ,# time of receipt month
           
             u'RCPT_DAY' : 'i4'  ,# time of receipt day
           
             u'RCPT_HOUR' : 'i4'  ,# time of receipt hour
           
             u'RCPT_MINT' : 'i4'  ,# time of receipt minute
           
             u'STLT_IDNY' : 'i4'  ,# Satellite Identifier
           
             u'STLT_INSTS' : 'i4'  ,# Satellite Instruments
           
             u'STTN_ACQSTN' : 'S20'  ,# Acquisition Station Identifier
           
             u'SFTWR_ID' : 'S12'  ,# Software Identification
           
             u'STLT_CYCL_NMBR' : 'i4'  ,# Satellite Cycle Number
           
             u'ORBT_NMBR' : 'i4'  ,# Orbit Number
           
             u'NMRCL_MODL_IDNY' : 'S16'  ,# Numerical Model Identifier
           
             u'SRFC_TYPE' : 'i4'  ,# Surface type
           
             u'RDMR_SNSD_SRFC_TYPE' : 'i4'  ,# Radiometer Sensed Surface type
           
             u'INTPN_FLAG' : 'i4'  ,# Interpolation Flag
           
             u'ERRR_ESMT_NVGTR_ORBT' : 'i4'  ,# 3-D Error Estimate of Navigator Orbit
           
             u'ALTR_DATA_QLTY_FLAG' : 'i4'  ,# Altimeter Data Quality Flag
           
             u'ALTR_CORTN_QLTY_FLAG' : 'i4'  ,# Altimeter Correction Quality Flag
           
             u'TRAIL_EDGE_VARN_FLAG' : 'i4'  ,# Trailing Edge Variation Flag
           
             u'ICE_PRSNC' : 'i4'  ,# Ice Presence Indicator
           
             u'MET_MAP_AVLBY' : 'i4'  ,# Meteorological Map Availability
           
             u'DRNL_TIDE_INTPN_FLAG' : 'i4'  ,# Interpolation Flag for Mean Diurnal Tide
           
             u'CHNL_FRQY' : 'f4'  ,# Channel Central Frequency
           
             u'KA_OCN_RNGE' : 'f4'  ,# Ka Band Ocean Range
           
             u'KA_RMS_OCN_RNGE' : 'f4'  ,# RMS Ka Band Ocean Range
           
             u'KA_NMBR_OCN_RNGE' : 'i4'  ,# No. of Valid Points for Ka Band Ocean Range
           
             u'KA_INSTL_CORTN' : 'f4'  ,# Ka Band Net Instrumental Correction
           
             u'KA_SEA_STTE_BIAS_CORTN' : 'f4'  ,# Ka Band Sea State Bias Correction
           
             u'KA_SGFT_WAVE_HGHT' : 'f4'  ,# Ka Band Significant Wave Height
           
             u'KA_RMS_SGFT_WAVE_HGHT' : 'f4'  ,# RMS Ka Band Significant Wave Height
           
             u'KA_NMBR_SGFT_WAVE_HGHT' : 'i4'  ,# No. of Valid Points for Ka Band Significant Wave Height
           
             u'KA_CORTN_SGFT_WAVE_HGHT' : 'f4'  ,# Ka Band Net Instrumental Correction for Significant Wave Height
           
             u'KA_CORTD_OCN_BCKR_CFNT' : 'f4'  ,# Ka Band Net Corrected Ocean Backscatter Coefficient
           
             u'KA_STD_CORTD_OCN_BCKR_CFNT' : 'f4'  ,# SD of Ka Band Net Corrected Ocean Backscatter Coefficient
           
             u'KA_NMBR_CORTD_OCN_BCKR_CFNT' : 'i4'  ,# No. of Valid Points for Ka Band Backscatter
           
             u'KA_INSTL_CORTN_AGC' : 'f4'  ,# Ka Band Net Instrumental Correction for AGC
           
             u'KA_SGMA0_ATNTN_CORTN' : 'f4'  ,# Ka Band Attenuation Correction on Sigma-0
           
             u'KA_ATMC_GAIN_CNTL' : 'f4'  ,# Ka Band Automatic Gain Control
           
             u'KA_RMS_ATMC_GAIN_CNTL' : 'f4'  ,# RMS of Ka Band Automatic Gain Control
           
             u'KA_NMBR_ATMC_GAIN_CNTL' : 'f4'  ,# No. of Valid Points for Ka Band Automatic Gain Control
           
             u'CHNL_FRQY1' : 'f4'  ,# Channel Central Frequency
           
             u'BRGTS_TMPR1' : 'f4'  ,# Brightness Temperature
           
             u'CHNL_FRQY2' : 'f4'  ,# Channel Central Frequency
           
             u'BRGTS_TMPR2' : 'f4'  ,# Brightness Temperature
           
             u'RDMR_WATR_VAPR_CONT' : 'f4'  ,# Radiometer Water Vapour Content
           
             u'TOTL_ACCM_PRCTN' : 'f4'  ,# Total Accumulated Precipitation
           
             u'WIND_HGHT' : 'f4'  ,# Height for Wind Speed Data
           
             u'ALTR_WIND_SPED' : 'f4'  ,# Wind Speed from Altimeter
           
             u'MODL_WIND_HGHT' : 'f4'  ,# Height for Model Wind Data
           
             u'MODL_WIND_U' : 'f4'  ,# U Component of Model Wind Vector
           
             u'MODL_WIND_V' : 'f4'  ,# V Component of Model Wind Vector
           
             u'DYNC_TPGY' : 'f4'  ,# Mean Dynamic Topography
           
             u'ALTD_COG' : 'f4'  ,# Altitude of COG above Reference Ellipsoid
           
             u'ALTD_RATE' : 'f4'  ,# Instantaneous Altitude Rate
           
             u'SQ_OFF_NADR_ANGL_PLTM' : 'f4'  ,# Squared-Off Nadir Angle of the Satellite from Platform Data
           
             u'SQ_OFF_NADR_ANGL_WVFM' : 'f4'  ,# Squared-Off Nadir Angle of the Satellite from Waveform Data
           
             u'CHNL_FRQY3' : 'f4'  ,# Channel Central Frequency
           
             u'INSRC_CORTN_MODL_KA' : 'f4'  ,# Model Ionospheric Correction on Ka Band
           
             u'MODL_DRY_TPRSC_CORTN' : 'f4'  ,# Model Dry Tropospheric Correction
           
             u'MODL_WET_TPRSC_CORTN' : 'f4'  ,# Model Wet Tropospheric Correction
           
             u'RDMR_WET_TRPHC_CORTN' : 'f4'  ,# Radiometer Wet Tropospheric Correction
           
             u'SEA_SRFC_HGHT' : 'f4'  ,# Mean Sea Surface Height
           
             u'GOID_HGHT' : 'f4'  ,# Geoid's Height
           
             u'OCN_DPTH_LAND_ELVTN' : 'f4'  ,# Ocean Depth / Land Elevation
           
             u'SOLD_ERTH_TIDE_HGHT' : 'f4'  ,# Solid Earth Tide Height
           
             u'GCNRC_OCN_TIDE_HGHT1' : 'f4'  ,# Geocentric Ocean Tide Height Solution 1
           
             u'GCNRC_OCN_TIDE_HGHT2' : 'f4'  ,# Geocentric Ocean Tide Height Solution 2
           
             u'LDNG_TDE_HGHT1' : 'f4'  ,# Loading Tide Height Geocentric Ocean Tide Solution 1
           
             u'LDNG_TDE_HGHT2' : 'f4'  ,# Loading Tide Height Geocentric Ocean Tide Solution 2
           
             u'TECH_INIT_PERTB' : 'i4'  ,# Technique for Making Up Initial Perturbations
           
             u'NON_EQLBM_LONG_PERD_TIDE_HGHT' : 'f4'  ,# Non-Equilibrium Long Period Tide Height
           
             u'GCNRC_POLE_TIDE_HGHT' : 'f4'  ,# Geocentric Pole Tide Height
           
             u'INVTD_BMTR_CORTN' : 'f4'  ,# Inverted Barometer Correction
           
             u'FLCTN_SEA_SRFC_TPGY_CORTN' : 'f4'  ,# High frequency fluctuations of the Sea Surface Topography Correction
           
             u'SEA_SRFC_HGHT_ANMY' : 'f4'  ,# Sea Surface Height Anomaly
           
             }

