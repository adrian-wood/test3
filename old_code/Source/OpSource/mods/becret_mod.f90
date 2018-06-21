MODULE becret_mod
 INTERFACE
  SUBROUTINE BECRET (NFTBCN, BEACON, DEGLAT, DEGLON, NUMSTNS)
   INTEGER,INTENT(IN)           ::    NFTBCN
   INTEGER,INTENT(INOUT)        ::    NUMSTNS
   REAL,INTENT(OUT)             ::    DEGLAT(:)
   REAL,INTENT(OUT)             ::    DEGLON(:)
   CHARACTER(LEN=8),INTENT(OUT) ::    BEACON(:)
  END SUBROUTINE BECRET
 END INTERFACE
END MODULE becret_mod