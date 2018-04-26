MODULE ersrep_mod
  INTERFACE
    SUBROUTINE ERSREP(NOBS,DATIME,ENTRY,BULL,IFT)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,           INTENT(IN)    :: NOBS      !a01
    INTEGER,           INTENT(INOUT) :: DATIME(5) !a02
    CHARACTER(LEN=12), INTENT(INOUT) :: ENTRY     !a03
    CHARACTER(LEN=*),  INTENT(INOUT) :: BULL      !a04
    INTEGER,           INTENT(INOUT) :: IFT       !a05

    END SUBROUTINE ERSREP
  END INTERFACE
END MODULE ersrep_mod
