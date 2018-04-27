MODULE amdar_mod
  INTERFACE
    SUBROUTINE AMDAR(BULL,LBUF,NFTAMD) 
      IMPLICIT NONE  
      CHARACTER(LEN=*), INTENT(INOUT)     :: BULL     ! BULLETIN (STARTING WITH TTAAII),  a1
      LOGICAL, INTENT(IN)                 :: LBUF     ! BUFR FOUND FLAG,                  a2
      INTEGER, INTENT(IN)                 :: NFTAMD   ! FT number for storage data set,   a3
    END SUBROUTINE AMDAR
  END INTERFACE
END MODULE amdar_mod
