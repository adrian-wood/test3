MODULE obhour_mod
  INTERFACE
    SUBROUTINE OBHOUR(DATE,DAY,HOUR,MIMJ,TOL,RC)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(INOUT) :: DATE(4)  !A01
    INTEGER,          INTENT(IN)    :: DAY      !A02
    INTEGER,          INTENT(IN)    :: HOUR     !A03
    CHARACTER(LEN=4), INTENT(IN)    :: MIMJ     !A04
    INTEGER,          INTENT(IN)    :: TOL      !A05
    INTEGER,          INTENT(OUT)   :: RC       !A06

    END SUBROUTINE OBHOUR
  END INTERFACE
END MODULE obhour_mod
