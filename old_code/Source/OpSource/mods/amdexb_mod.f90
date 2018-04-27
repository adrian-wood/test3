MODULE amdexb_mod
  INTERFACE
    SUBROUTINE AMDEXB(NDES,IDESCR,LOC_DES,NUMREP,EXPARR,OARRAY)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN)    :: NDES               !a1
    INTEGER,          INTENT(INOUT) :: IDESCR(:)          !a2
    INTEGER,          INTENT(IN)    :: LOC_DES            !a3
    INTEGER,          INTENT(IN)    :: NUMREP             !a4
    REAL,             INTENT(IN)    :: EXPARR(NUMREP,*)   !a5
    REAL,             INTENT(INOUT) :: OARRAY(:)          !a6

    END SUBROUTINE AMDEXB
  END INTERFACE
END MODULE amdexb_mod
