MODULE SORTR_MOD
INTERFACE
SUBROUTINE SORTR(A,L,N,MASK)

IMPLICIT NONE

INTEGER, INTENT(IN)     :: L,N
REAL,    INTENT(INOUT)  :: A(L,N)
INTEGER, INTENT(IN)     :: MASK(L)

END SUBROUTINE SORTR
END INTERFACE
END MODULE SORTR_MOD
