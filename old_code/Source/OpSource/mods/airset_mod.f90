MODULE airset_mod
 INTERFACE
  SUBROUTINE AIRSET(LAT,LONG,LAT2,LONG2,MATCH,TIMEYY,TIMEMNTH,&
  TIMEDD,TIMEHH,TIMEMM,LEVEL,TEMP,WINDD,WINDS,OPT_TEMP,OPT_WIND,&
  OPT_WNDS,OPT_TURB,OPT_ICE,MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,&
  ARYFLG,AIR_ARAY1,AIR_ARAY2,INDX_ARAY1,INDX_ARAY2)
   INTEGER,INTENT(IN)    ::    MATCH
   INTEGER,INTENT(OUT)   ::    ARYFLG
   REAL,INTENT(IN)       ::    LAT
   REAL,INTENT(IN)       ::    LONG
   REAL,INTENT(IN)       ::    LAT2
   REAL,INTENT(IN)       ::    LONG2
   REAL,INTENT(IN)       ::    TIMEDD
   REAL,INTENT(IN)       ::    TIMEHH
   REAL,INTENT(IN)       ::    TIMEMM
   REAL,INTENT(IN)       ::    TIMEMNTH
   REAL,INTENT(IN)       ::    TIMEYY
   REAL,INTENT(IN)       ::    LEVEL
   REAL,INTENT(IN)       ::    TEMP
   REAL,INTENT(IN)       ::    WINDD
   REAL,INTENT(IN)       ::    WINDS
   REAL,INTENT(IN)       ::    OPT_TEMP
   REAL,INTENT(IN)       ::    OPT_WIND
   REAL,INTENT(IN)       ::    OPT_WNDS
   REAL,INTENT(IN)       ::    OPT_TURB
   REAL,INTENT(IN)       ::    OPT_ICE
   REAL,INTENT(OUT)      ::    AIR_ARAY1(18)
   REAL,INTENT(OUT)      ::    AIR_ARAY2(18)
   REAL,INTENT(OUT)      ::    INDX_ARAY1(8)
   REAL,INTENT(OUT)      ::    INDX_ARAY2(8)
   REAL,INTENT(IN)       ::    MID_YY
   REAL,INTENT(IN)       ::    MID_MTH
   REAL,INTENT(IN)       ::    MID_DD
   REAL,INTENT(IN)       ::    MID_HH
   REAL,INTENT(IN)       ::    MID_MM
  END SUBROUTINE AIRSET
 END INTERFACE
END MODULE airset_mod