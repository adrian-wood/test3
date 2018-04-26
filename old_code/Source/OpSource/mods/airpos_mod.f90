MODULE airpos_mod
 INTERFACE
  SUBROUTINE AIRPOS(REPLEN,REPORT,POINT,LAT,LONG,BEAC_NAME,&
   NFTBCN,DECERR)
   CHARACTER(LEN=*),INTENT(IN)  ::  REPORT      !size passed from airarp
   CHARACTER(LEN=8),INTENT(OUT) ::  BEAC_NAME   !beacon name
   INTEGER,INTENT(IN)           ::  REPLEN      !report length
   INTEGER,INTENT(INOUT)        ::  POINT       !pos in group in report
   INTEGER,INTENT(IN)           ::  NFTBCN      !Ft no. for Beacons list
   INTEGER,INTENT(OUT)          ::  DECERR      !error flag
   REAL,INTENT(OUT)             ::  LAT         !expanded lat
   REAL,INTENT(OUT)             ::  LONG        !expanded long
  END SUBROUTINE AIRPOS
 END INTERFACE
END MODULE airpos_mod
