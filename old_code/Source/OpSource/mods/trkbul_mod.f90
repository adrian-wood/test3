MODULE trkbul_mod
  INTERFACE
    SUBROUTINE TRKBUL(BULL,L,TTAAII,CCCC,YYGGGG,CORN,IFT)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT)        :: BULL       ! bulletin,                    a1
      INTEGER, INTENT(INOUT)                 :: L          ! length of bulletin,          a2
      CHARACTER(LEN=6), INTENT(IN)           :: TTAAII     ! bulletin header,             a3
      CHARACTER(LEN=4), INTENT(IN)           :: CCCC       ! collecting centre,           a4
      CHARACTER(LEN=6), INTENT(IN)           :: YYGGGG     ! date/time of bulletin,       a5
      CHARACTER(LEN=2), INTENT(IN)           :: CORN       ! report correction number,    a6
      INTEGER, INTENT(IN)                    :: IFT        ! FT number of storage dataset,a7
    END SUBROUTINE TRKBUL
  END INTERFACE
END MODULE trkbul_mod
