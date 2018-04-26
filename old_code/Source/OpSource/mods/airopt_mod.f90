MODULE AIROPT_mod
 INTERFACE
  SUBROUTINE AIROPT(REPLEN,REPORT,POINT,TIMEDD,TIMEHH,TIMEMM,LAT,&
  LAT2,LONG,LONG2,OPT_TEMP,OPT_WIND,OPT_WNDS,OPT_TURB,OPT_ICE,&
  MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,SIGN,MATCH,NFT,NFTBCN,DECERR)
   INTEGER,INTENT(IN)      ::    REPLEN   !Report Length
   INTEGER,INTENT(IN)      ::    POINT    !Position within report
   INTEGER,INTENT(INOUT)   ::    DECERR   !Decode error flag
   INTEGER,INTENT(IN)      ::    NFTBCN
   INTEGER,INTENT(IN)      ::    NFT
   INTEGER,INTENT(INOUT)   ::    MATCH
   REAL,INTENT(INOUT)      ::    MID_MM
   REAL,INTENT(INOUT)      ::    MID_MTH
   REAL,INTENT(INOUT)      ::    MID_YY
   REAL,INTENT(INOUT)      ::    MID_DD
   REAL,INTENT(INOUT)      ::    MID_HH
   REAL,INTENT(IN)         ::    TIMEDD   !Day of current report
   REAL,INTENT(IN)         ::    TIMEHH   !Hour of current report
   REAL,INTENT(IN)         ::    TIMEMM   !Mins of current report
   REAL,INTENT(IN)         ::    LAT
   REAL,INTENT(IN)         ::    LONG
   REAL,INTENT(INOUT)      ::    LAT2     !Second Latitude
   REAL,INTENT(INOUT)      ::    LONG2    !Second Longitude
   REAL,INTENT(INOUT)      ::    OPT_TEMP !Decoded optional temp group
   REAL,INTENT(INOUT)      ::    OPT_WIND !Dec opt wind direction
   REAL,INTENT(INOUT)      ::    OPT_WNDS !Decoded opt wind speed group
   REAL,INTENT(INOUT)      ::    OPT_TURB !Decoded option turb group
   REAL,INTENT(INOUT)      ::    OPT_ICE  !Decoded optional ice group
   CHARACTER(LEN=*)        ::    REPORT   !Airep report
   CHARACTER(LEN=8)        ::    SIGN
  END SUBROUTINE AIROPT
 END INTERFACE
END MODULE airopt_mod
