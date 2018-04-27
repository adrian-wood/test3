MODULE tafcnl_mod
  INTERFACE
    SUBROUTINE TAFCNL(REPORT,REPLEN,C_RC)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)    ::  REPORT ! Report
    INTEGER,      INTENT(IN)    ::  REPLEN ! Report Length
    INTEGER,      INTENT(OUT)   ::  C_RC ! Return code 1 = TAF cancelled
                                         ! 2= TAF VALID

    END SUBROUTINE TAFCNL
  END INTERFACE
END MODULE tafcnl_mod
