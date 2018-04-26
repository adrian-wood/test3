MODULE amdexc_mod
  INTERFACE
    SUBROUTINE AMDEXC(OB,R,CDISP,ID,IPOINT,LENGTH,BDAY,BHOUR,DATAEX)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: OB      !a1
    INTEGER,          INTENT(INOUT) :: R       !a2
    INTEGER,          INTENT(INOUT) :: CDISP   !a3
    CHARACTER(LEN=*), INTENT(INOUT) :: ID      !a4
    INTEGER,          INTENT(IN)    :: IPOINT  !a5
    INTEGER,          INTENT(IN)    :: LENGTH  !a6
    INTEGER,          INTENT(IN)    :: BDAY    !a7
    INTEGER,          INTENT(IN)    :: BHOUR   !a8
    REAL,        INTENT(INOUT):: DATAEX(29,*)  !a9 Seq 311200 has 29 items

    END SUBROUTINE AMDEXC
  END INTERFACE
END MODULE amdexc_mod
