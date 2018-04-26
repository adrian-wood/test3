MODULE tessc4_mod
  INTERFACE
    SUBROUTINE TESSC4(REPORT,POS,EXPARR,ARRPOS3,ENDVALS)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    ::  REPORT       !a01
    INTEGER,          INTENT(INOUT) ::  POS          !a02
    REAL,             INTENT(INOUT) ::  EXPARR(0:*)  !a03
    INTEGER,          INTENT(IN)    ::  ARRPOS3      !a04
    REAL,             INTENT(INOUT) ::  ENDVALS(3)   !a05

    END SUBROUTINE TESSC4
  END INTERFACE
END MODULE tessc4_mod
