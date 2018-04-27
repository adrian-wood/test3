MODULE syncld_mod
  INTERFACE
    SUBROUTINE SYNCLD(POINT,REPORT,IG,NGRPS,ICL,CLOUD,VSIF,VERTV)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,            INTENT(INOUT) :: POINT      !a01
    CHARACTER(LEN=*),   INTENT(IN)    :: REPORT     !a02
    INTEGER,            INTENT(INOUT) :: IG         !a03
    INTEGER,            INTENT(IN)    :: NGRPS      !a04
    INTEGER,            INTENT(INOUT) :: ICL        !a05
    REAL,               INTENT(INOUT) :: CLOUD(3,4) !a06
    REAL,               INTENT(INOUT) :: VSIF(4)    !a07
    REAL,               INTENT(INOUT) :: VERTV      !a08

    END SUBROUTINE SYNCLD
  END INTERFACE
END MODULE syncld_mod
