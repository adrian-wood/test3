MODULE NCMEXP_MOD
INTERFACE
SUBROUTINE NCMEXP(REPORT,REPLEN,REXP)

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)    ::  REPORT    ! character report to be expanded
INTEGER,           INTENT(IN)    ::  REPLEN    ! the report length
REAL,              INTENT(INOUT) ::  REXP(:)   ! array of expanded values

END SUBROUTINE NCMEXP
END INTERFACE
END MODULE NCMEXP_MOD
