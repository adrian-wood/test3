MODULE tessc5_mod
  INTERFACE
    SUBROUTINE TESSC5(REPORT,EXPARR,IDFLG,ID)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    ::    REPORT        !a01
    REAL,             INTENT(INOUT) ::    EXPARR(0:*)   !a02
    LOGICAL,          INTENT(OUT)   ::    IDFLG         !a03
    CHARACTER(LEN=9), INTENT(OUT)   ::    ID            !a04

    END SUBROUTINE TESSC5
  END INTERFACE
END MODULE tessc5_mod
