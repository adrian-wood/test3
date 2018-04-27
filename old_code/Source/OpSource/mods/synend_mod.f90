MODULE synend_mod
  INTERFACE
    SUBROUTINE SYNEND(RSTART,BEND,REND,WMOBLK,BULL)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN)    :: RSTART  !A01
    INTEGER,          INTENT(IN)    :: BEND    !A02
    INTEGER,          INTENT(OUT)   :: REND    !A03
    INTEGER,          INTENT(IN)    :: WMOBLK  !A04
    CHARACTER(LEN=*), INTENT(INOUT) :: BULL    !A05

    END SUBROUTINE SYNEND
  END INTERFACE
END MODULE synend_mod
