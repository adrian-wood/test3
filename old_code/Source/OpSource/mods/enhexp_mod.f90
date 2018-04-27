MODULE enhexp_mod
  INTERFACE
    SUBROUTINE ENHEXP(REPORT,REPLEN,POS,EXPARR,STNID,OBDATE,NUM_VALS, &
                      SYNTAX)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=4096), INTENT(IN)        ::  REPORT      !A01
    INTEGER,             INTENT(IN)        ::  REPLEN      !A02
    INTEGER,             INTENT(INOUT)     ::  POS         !A03
    REAL,                INTENT(OUT)       ::  EXPARR(30)  !A04
    CHARACTER(LEN=5),    INTENT(IN)        ::  STNID       !A05
    INTEGER,             INTENT(IN)        ::  OBDATE(5)   !A06
    INTEGER,             INTENT(OUT)       ::  NUM_VALS    !A07
    LOGICAL,             INTENT(INOUT)     ::  SYNTAX      !A08

    END SUBROUTINE ENHEXP
  END INTERFACE
END MODULE enhexp_mod
