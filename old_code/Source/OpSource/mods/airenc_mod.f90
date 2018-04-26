MODULE airenc_mod
 INTERFACE
  SUBROUTINE AIRENC(AIR_ARAY1,AIR_ARAY2,ARYFLG,INDX_ARAY1,&
  INDX_ARAY2,SIGN,BEAC_NAME,REPORT,REPLEN,&
  SPAN,CCCC,NFT,TOR,TTAAII)
   INTEGER,INTENT(IN)   ::     TOR(5)    !time or reciept
   INTEGER,INTENT(IN)   ::     ARYFLG
   INTEGER,INTENT(IN)   ::     REPLEN    !length of character report
   INTEGER,INTENT(IN)   ::     SPAN      !time span of rep in bull
   INTEGER,INTENT(IN)   ::     NFT !data set unit number for MDB storage
   REAL,INTENT(IN)      ::     AIR_ARAY1(18) !data array
   REAL,INTENT(IN)      ::     AIR_ARAY2(18) !elem array with mid point
   REAL,INTENT(IN)      ::     INDX_ARAY1(8)
   REAL,INTENT(IN)      ::     INDX_ARAY2(8)
   CHARACTER(LEN=*),INTENT(IN) ::     REPORT !Raw character report
   CHARACTER(LEN=*),INTENT(IN) ::     CCCC
   CHARACTER(LEN=*),INTENT(IN) ::     TTAAII
   CHARACTER(LEN=8),INTENT(IN) ::     SIGN   !aircraft callsign
   CHARACTER(LEN=8),INTENT(IN) ::     BEAC_NAME
  END SUBROUTINE AIRENC
 END INTERFACE
END MODULE airenc_mod
