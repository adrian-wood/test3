MODULE read4k_mod
 INTERFACE
  SUBROUTINE READ4K (NFT, BUFREC, NFIRST, KODE)
   INTEGER,          INTENT(INOUT)   ::   KODE
   INTEGER,          INTENT(INOUT)   ::   NFIRST
   INTEGER,          INTENT(IN)      ::   NFT
   CHARACTER(LEN=*), INTENT(INOUT)   ::   BUFREC
  END SUBROUTINE READ4K
 END INTERFACE
END MODULE READ4K_MOD
