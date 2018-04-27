MODULE synxp4_mod
  INTERFACE
    SUBROUTINE SYNXP4(REPORT,POINT,NGRPS,IMT,MTCL)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: REPORT    !a01
    INTEGER,          INTENT(INOUT) :: POINT     !a02
    INTEGER,          INTENT(IN)    :: NGRPS     !a03
    INTEGER,          INTENT(OUT)   :: IMT       !a04
    REAL,             INTENT(INOUT) :: MTCL(4,2) !a05

    END SUBROUTINE SYNXP4
  END INTERFACE
END MODULE synxp4_mod
