MODULE datimgp_mod
  INTERFACE
    SUBROUTINE DATIMGP(GROUP,BADTAF)

    IMPLICIT NONE

    CHARACTER(LEN=*),INTENT(IN)  :: GROUP
    LOGICAL,         INTENT(OUT) :: BADTAF

    END SUBROUTINE DATIMGP
  END INTERFACE
END MODULE datimgp_mod
