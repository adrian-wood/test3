MODULE tessc1_mod
  INTERFACE
    SUBROUTINE TESSC1(REPORT,POS,EXPARR,DATIME,IERR)
    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    ::  REPORT      !a01
    INTEGER,          INTENT(OUT)   ::  POS         !a02
    REAL,             INTENT(INOUT) ::  EXPARR(0:*) !a03
    INTEGER,          INTENT(OUT)   ::  DATIME(5)   !a04
    INTEGER,          INTENT(OUT)   ::  IERR        !a05

    END SUBROUTINE TESSC1
  END INTERFACE
END MODULE tessc1_mod
