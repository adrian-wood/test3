MODULE airelm_mod
 INTERFACE
  SUBROUTINE AIRELM(REPLEN,REPORT,POINT,TEMP,WINDD,WINDS,&
  WIND_FLAG,TEMP_FLAG)
   INTEGER,INTENT(IN)          ::    REPLEN    !Len of airep rep decoded
   INTEGER,INTENT(INOUT)       ::    POINT     !Position within report
   INTEGER,INTENT(IN)          ::    WIND_FLAG !control flag
   INTEGER,INTENT(INOUT)       ::    TEMP_FLAG !control flag
   CHARACTER(LEN=*),INTENT(IN) ::    REPORT    !Airep report
   REAL,INTENT(OUT)            ::    TEMP      !Decoded temperature
   REAL,INTENT(OUT)            ::    WINDD     !Decoded wind direction
   REAL,INTENT(OUT)            ::    WINDS     !Decoded wind speed
  END SUBROUTINE AIRELM
 END INTERFACE
END MODULE airelm_mod
