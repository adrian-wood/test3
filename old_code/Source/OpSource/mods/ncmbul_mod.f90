MODULE ncmbul_mod
  INTERFACE
    SUBROUTINE NCMBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG, &
                      OCOR,CORNUM,MIMJ,NFTNCM,BULL)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(INOUT) :: POINT  !a01 start of report in bulletin
    INTEGER,          INTENT(INOUT) :: BULEND !a02 end of bulletin
    CHARACTER(LEN=*), INTENT(IN)    :: TTAAII !a03
    CHARACTER(LEN=*), INTENT(IN)    :: CCCC   !a04
    CHARACTER(LEN=*), INTENT(INOUT) :: YYGGGG !a05
    LOGICAL,          INTENT(IN)    :: OCOR   !a06
    CHARACTER(LEN=*), INTENT(IN)    :: CORNUM !a07
    CHARACTER(LEN=7), INTENT(IN)    :: MIMJ   !a08
    INTEGER,          INTENT(IN)    :: NFTNCM !a09
    CHARACTER(LEN=*), INTENT(INOUT) :: BULL   !a10

    END SUBROUTINE NCMBUL
  END INTERFACE
END MODULE ncmbul_mod
