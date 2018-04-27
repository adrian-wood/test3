MODULE tessc3_mod
  INTERFACE
    SUBROUTINE TESSC3(REPORT,POS,EXPARR,POS1A,POS2A)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    ::  REPORT       !a01
    INTEGER,          INTENT(INOUT) ::  POS          !a02
    REAL,             INTENT(INOUT) ::  EXPARR(0:*)  !a03
    INTEGER,          INTENT(IN)    ::  POS1A        !a04
    INTEGER,          INTENT(OUT)   ::  POS2A        !a05

    END SUBROUTINE TESSC3
  END INTERFACE
END MODULE tessc3_mod
