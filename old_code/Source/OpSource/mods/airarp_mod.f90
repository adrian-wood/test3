MODULE airarp_mod
 INTERFACE
  SUBROUTINE AIRARP(REPLEN,REPORT,TAILNO,YYGGGG,CCCC,&
  TTAAII,NFTAIR,NFTBCN,SPAN)
   CHARACTER(LEN=4),INTENT(IN)      ::  CCCC
   CHARACTER(LEN=6),INTENT(IN)      ::  TTAAII
   CHARACTER(LEN=120),INTENT(INOUT) ::  REPORT !len of bull of airep
   CHARACTER(LEN=*),INTENT(IN)      ::  TAILNO !tail number
   CHARACTER(LEN=6),INTENT(IN)      ::  YYGGGG
!
   INTEGER,INTENT(IN)       ::  NFTAIR    !FT no. of storage dataset
   INTEGER,INTENT(IN)       ::  NFTBCN    !FT no. for Beacons list ds
   INTEGER,INTENT(INOUT)    ::  REPLEN    !len of rep being decoded
   INTEGER,INTENT(IN)       ::  SPAN      !time span of obs in bull
  END SUBROUTINE AIRARP
 END INTERFACE
END MODULE airarp_mod
