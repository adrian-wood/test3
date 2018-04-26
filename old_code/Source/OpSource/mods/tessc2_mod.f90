MODULE tessc2_mod
  INTERFACE
    SUBROUTINE TESSC2(REPORT,POS,EXPARR,POS1A,ENDVALS)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    ::  REPORT        !a01
    INTEGER,          INTENT(INOUT) ::  POS           !a02
    REAL,             INTENT(INOUT) ::  EXPARR(0:*)   !a03
    INTEGER,          INTENT(OUT)   ::  POS1A         !a04
    REAL,             INTENT(OUT)   ::  ENDVALS(3)    !a05

    END SUBROUTINE TESSC2
  END INTERFACE
END MODULE tessc2_mod
