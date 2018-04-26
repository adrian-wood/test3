MODULE webbuoyp_mod
  INTERFACE
    SUBROUTINE WEBBUOYP(ARRAY,IOBS,IELS,I)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER, INTENT(IN)    :: IOBS             !a02- dimension 2 of ARRAY
    INTEGER, INTENT(IN)    :: IELS             !a03- dimension 1 of ARRAY
    REAL,    INTENT(IN)    :: ARRAY(IOBS,IELS) !a01- MetDB data array
    INTEGER, INTENT(IN)    :: I                !a04- pointer in ARRAY. Work on this ob.

    END SUBROUTINE WEBBUOYP
  END INTERFACE
END MODULE webbuoyp_mod
