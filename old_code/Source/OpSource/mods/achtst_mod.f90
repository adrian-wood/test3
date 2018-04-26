MODULE achtst_mod
  INTERFACE
    SUBROUTINE ACHTST(POINT,LENGTH,ALLETT,ICHRAC,OSPACE,OLFCR,BULL)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN)    :: POINT !a1
    INTEGER,          INTENT(IN)    :: LENGTH !a2
    LOGICAL,          INTENT(OUT)   :: ALLETT !a3
    INTEGER,          INTENT(OUT)   :: ICHRAC !a4
    LOGICAL,          INTENT(OUT)   :: OSPACE !a5
    LOGICAL,          INTENT(OUT)   :: OLFCR !a6
    CHARACTER(LEN=*), INTENT(IN)    :: BULL !a7

    END SUBROUTINE ACHTST
  END INTERFACE
END MODULE achtst_mod
