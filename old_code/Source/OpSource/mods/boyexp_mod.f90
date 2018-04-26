MODULE boyexp_mod
  INTERFACE
    SUBROUTINE BOYEXP(REPORT,RLEN,DATIME,ARRAY,DROGFLG)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)  ::  REPORT    !A1
    INTEGER,          INTENT(IN)  ::  RLEN      !A2 length of report
    INTEGER,          INTENT(OUT) ::  DATIME(5) !A3 report date/time
    REAL,             INTENT(OUT) ::  ARRAY(0:600) !A4 output array
    LOGICAL,          INTENT(OUT) ::  DROGFLG   !A5 set if there's a profile

    END SUBROUTINE BOYEXP
  END INTERFACE
END MODULE boyexp_mod
