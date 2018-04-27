MODULE boyenc_mod
  INTERFACE
    SUBROUTINE BOYENC(ARRAY,DATIME,TTAAII,CCCC,OCOR,DRGFLG,NFT)

    IMPLICIT NONE

! Subroutine arguments:

    REAL,             INTENT(IN)    ::     ARRAY(0:600)  !A01
    INTEGER,          INTENT(INOUT) ::     DATIME(5)     !A02
    CHARACTER(LEN=6), INTENT(IN)    ::     TTAAII        !A03
    CHARACTER(LEN=4), INTENT(IN)    ::     CCCC          !A04
    LOGICAL,          INTENT(IN)    ::     OCOR          !A05
    LOGICAL,          INTENT(IN)    ::     DRGFLG        !A06
    INTEGER,          INTENT(IN)    ::     NFT           !A07

    END SUBROUTINE BOYENC
  END INTERFACE
END MODULE boyenc_mod
