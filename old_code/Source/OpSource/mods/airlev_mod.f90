MODULE airlev_mod
 INTERFACE
  SUBROUTINE AIRLEV(REPLEN,REPORT,POINT,LEVEL,DECERR)
   CHARACTER(LEN=*),INTENT(IN)  ::   REPORT     !size passed from airarp
   INTEGER,INTENT(IN)           ::   REPLEN     !report length
   INTEGER,INTENT(INOUT)        ::   POINT      !pos in group in report
   INTEGER,INTENT(OUT)          ::   DECERR     !decode error flag
   REAL,INTENT(OUT)             ::   LEVEL      !exp fl level (feet>met
  END SUBROUTINE AIRLEV
 END INTERFACE
END MODULE airlev_mod
