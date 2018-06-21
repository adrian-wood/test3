MODULE mtrexp_mod
  INTERFACE
    SUBROUTINE MTREXP(REPORT,REPLEN,REXP,CEXP,IFAIL)              !2.0

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(INOUT) ::  REPORT ! WHOLE METAR REPORT
    INTEGER,      INTENT(INOUT) ::  REPLEN ! METAR REPORT LENGTH
    REAL,         INTENT(INOUT) ::  REXP(:) ! EXPANDED DATA (NUMERIC)
    CHARACTER(*), INTENT(INOUT) ::  CEXP ! EXPANDED DATA (CHARACTER)
    INTEGER,      INTENT(INOUT) ::  IFAIL ! ERROR STATUS FLAG

    END SUBROUTINE MTREXP
  END INTERFACE
END MODULE mtrexp_mod