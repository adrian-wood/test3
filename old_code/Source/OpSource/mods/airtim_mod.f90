MODULE airtim_mod
 INTERFACE
  SUBROUTINE AIRTIM(REPLEN,REPORT,POINT,TIMEDD,TIMEYY,TIMEMNTH,&
  TIMEHH,TIMEMM,YYGGGG,DECERR)
   INTEGER,INTENT(IN)          ::   REPLEN   !report length
   INTEGER,INTENT(INOUT)       ::   POINT
   INTEGER,INTENT(INOUT)       ::   DECERR   !decode error flag
   REAL,INTENT(OUT)            ::   TIMEDD   !extract day-time
   REAL,INTENT(OUT)            ::   TIMEHH   !extract hour-time
   REAL,INTENT(OUT)            ::   TIMEMM   !extract minute-time
   REAL,INTENT(OUT)            ::   TIMEYY   !Year
   REAL,INTENT(OUT)            ::   TIMEMNTH !Month
   CHARACTER(LEN=*),INTENT(IN) ::   REPORT   !size passed on from airarp
   CHARACTER(LEN=6),INTENT(IN) ::   YYGGGG   !day/time from bulletin
  END SUBROUTINE AIRTIM
 END INTERFACE
END MODULE airtim_mod
