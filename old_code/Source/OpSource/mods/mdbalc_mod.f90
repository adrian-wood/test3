MODULE MDBALC_MOD
INTERFACE
SUBROUTINE MDBALC (CTYPE, IDATA, CDSN, IFAIL, CERR, LTEST,  &
                         RNAME, MSTREAM)

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)    :: CTYPE      ! Type of data required
INTEGER,           INTENT(INOUT) :: IDATA(5)   ! Data set name information
CHARACTER (LEN=*), INTENT(IN)    :: CDSN       ! Storage data set to be opened
INTEGER,           INTENT(OUT)   :: IFAIL      ! Return code
CHARACTER (LEN=*), INTENT(OUT)   :: CERR       ! Error message text
LOGICAL,           INTENT(IN)    :: LTEST      ! TRUE if diagnostics wanted
CHARACTER (LEN=*), INTENT(IN)    :: RNAME      ! DSN of retrieval table
CHARACTER (LEN=3), INTENT(IN)    :: MSTREAM    ! MASS stream (minus MDB prfx)


END SUBROUTINE MDBALC
END INTERFACE
END MODULE MDBALC_MOD
