MODULE synxp5_mod
  INTERFACE
    SUBROUTINE SYNXP5(REPORT,POINT,NGRPS,WNDKTS,T1GUST,T2GUST, &
                      VGUST,N,ICL,CLOUD,VSIF,VERTV)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: REPORT     !a01
    INTEGER,          INTENT(INOUT) :: POINT      !a02
    INTEGER,          INTENT(IN)    :: NGRPS      !a03
    LOGICAL,          INTENT(IN)    :: WNDKTS     !a04
    REAL,             INTENT(INOUT) :: T1GUST     !a05
    REAL,             INTENT(INOUT) :: T2GUST     !a06
    REAL,             INTENT(INOUT) :: VGUST      !a07
    REAL,             INTENT(INOUT) :: N          !a08
    INTEGER,          INTENT(INOUT) :: ICL        !a09
    REAL,             INTENT(INOUT) :: CLOUD(3,4) !a10
    REAL,             INTENT(INOUT) :: VSIF(4)    !a11
    REAL,             INTENT(INOUT) :: VERTV      !a12

    END SUBROUTINE SYNXP5
  END INTERFACE
END MODULE synxp5_mod
