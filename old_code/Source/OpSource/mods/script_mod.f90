MODULE script_mod
  INTERFACE
    SUBROUTINE SCRIPT(DESCR,ND,DSPLAY)

    IMPLICIT NONE

    ! Subroutine arguments:

    INTEGER,          INTENT(INOUT) :: DESCR(:)
    INTEGER,          INTENT(INOUT) :: ND
    LOGICAL,          INTENT(IN)    :: DSPLAY

    END SUBROUTINE SCRIPT
  END INTERFACE
END MODULE script_mod
