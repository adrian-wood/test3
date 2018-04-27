MODULE airbul_mod
  INTERFACE
    SUBROUTINE AIRBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,NFTAIR, &
                        NFTBCN,BULL)
      IMPLICIT NONE
      INTEGER, INTENT (INOUT)                 :: POINT     !pointer to end of bulletin heading
      INTEGER, INTENT (IN)                    :: BULEND    !pointer to end of bulletin
      CHARACTER(LEN=6), INTENT(IN)            :: TTAAII    !bulletin id header
      CHARACTER(LEN=4), INTENT(IN)            :: CCCC      !collecting centre
      CHARACTER(LEN=6), INTENT(IN)            :: YYGGGG    !date/time of bulletin
      INTEGER, INTENT (IN)                    :: NFTAIR    !FT no for airep storage
      INTEGER, INTENT (IN)                    :: NFTBCN    !FT no for beacons dataset
      CHARACTER(LEN=*), INTENT(IN)            :: BULL      !bulletin report
    END SUBROUTINE AIRBUL
  END INTERFACE
END MODULE airbul_mod
