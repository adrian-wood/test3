MODULE tafexp_mod
  INTERFACE
    SUBROUTINE TAFEXP(REPORT,REPLEN,REXP,NAMD,FAIL)          !2.0,9

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(INOUT) ::  REPORT ! The report to be expanded/d    ecoded.
    INTEGER,      INTENT(INOUT) ::  REPLEN ! Total length of the report.
    REAL,         INTENT(INOUT) ::  REXP(:) ! Expansion array.
    INTEGER,      INTENT(INOUT) ::  NAMD ! Number of amendment   !9
    INTEGER,      INTENT(INOUT) ::  FAIL ! Error value returned to MDB.

    END SUBROUTINE TAFEXP
  END INTERFACE
END MODULE tafexp_mod
