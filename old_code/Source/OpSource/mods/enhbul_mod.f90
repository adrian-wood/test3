MODULE enhbul_mod
  INTERFACE
    SUBROUTINE ENHBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,OCOR, &
                      CORNUM,MIMJ,NFTENH,BULL)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(INOUT) ::    POINT     !A01
    INTEGER,          INTENT(INOUT) ::    BULEND    !A02
    CHARACTER(LEN=*), INTENT(IN)    ::    TTAAII    !A03
    CHARACTER(LEN=*), INTENT(IN)    ::    CCCC      !A04
    CHARACTER(LEN=*), INTENT(IN)    ::    YYGGGG    !A05
    LOGICAL,          INTENT(IN)    ::    OCOR      !A06
    CHARACTER(LEN=*), INTENT(IN)    ::    CORNUM    !A07
    CHARACTER(LEN=7), INTENT(IN)    ::    MIMJ      !A08
    INTEGER,          INTENT(IN)    ::    NFTENH    !A09
    CHARACTER(LEN=*), INTENT(INOUT) ::    BULL      !A10

    END SUBROUTINE ENHBUL
  END INTERFACE
END MODULE enhbul_mod
