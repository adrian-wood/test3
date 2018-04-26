MODULE amdbul_mod
  INTERFACE
    SUBROUTINE AMDBUL (BULL, LBUF, NFTAMD)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN) :: BULL   ! GTS AMDAR bulletin
      LOGICAL,          INTENT(IN) :: LBUF   ! BUFR bulletin flag
      INTEGER,          INTENT(IN) :: NFTAMD ! Storage data set unit no.
    END SUBROUTINE AMDBUL
  END INTERFACE
END MODULE amdbul_mod
