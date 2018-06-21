MODULE SRWEXP_MOD
INTERFACE
SUBROUTINE SRWEXP(REPORT,REALEXP,MSGLEN)

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(INOUT)  :: REPORT        ! The SREW report
REAL,              INTENT(INOUT)  :: REALEXP(:)    ! Array to hold expanded elements
INTEGER,           INTENT(IN)     :: MSGLEN        ! Message length

END SUBROUTINE SRWEXP
END INTERFACE
END MODULE SRWEXP_MOD