MODULE tafbul_mod
  INTERFACE
    SUBROUTINE TAFBUL(POINT,BEND,TTAAII,CCCC,YYGGGG, &
                      AMDBUL,AMDNUB,CORBUL,CORNUB,NFT,OERR,BULL)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(INOUT) :: POINT  ! A01 Current position in bul.
    INTEGER,          INTENT(INOUT) :: BEND   ! A02 End of bulletin.
    CHARACTER(LEN=*), INTENT(IN)    :: TTAAII ! A03 Bulletin identifier.
    CHARACTER(LEN=*), INTENT(IN)    :: CCCC   ! A04 Originating centre.
    CHARACTER(LEN=*), INTENT(IN)    :: YYGGGG ! A05 Bulletin date/time.
    LOGICAL,          INTENT(INOUT) :: AMDBUL ! A06 Bulletin amend flag.
    CHARACTER(LEN=*), INTENT(INOUT) :: AMDNUB ! A07 Bulletin amend number.
    LOGICAL,          INTENT(INOUT) :: CORBUL ! A08 Bulletin correction flg.
    CHARACTER(LEN=*), INTENT(INOUT) :: CORNUB ! A09 Bulletin correction no.
    INTEGER,          INTENT(IN)    :: NFT    ! A10 File allocation number.
    LOGICAL,          INTENT(OUT)   :: OERR   ! A11 Error flag.
    CHARACTER(LEN=*), INTENT(INOUT) :: BULL   ! A12 Bulletin of reports.

    END SUBROUTINE TAFBUL
  END INTERFACE
END MODULE tafbul_mod
