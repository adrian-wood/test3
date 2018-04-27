MODULE SHPSEQ_mod
  INTERFACE
    SUBROUTINE SHPSEQ(IDENT,IDATIM,PRESS,TEND,PFLAG, &
                      NEWLAT,NEWLON,DS,VS,LLFLAG)

    IMPLICIT NONE

    CHARACTER (LEN=9), INTENT(IN)  :: IDENT
    INTEGER, INTENT(IN)            :: IDATIM(5)
    REAL, INTENT(IN)               :: PRESS
    REAL, INTENT(IN)               :: TEND
    REAL, INTENT(OUT)              :: PFLAG
    REAL, INTENT(IN)               :: NEWLAT
    REAL, INTENT(IN)               :: NEWLON
    REAL, INTENT(IN)               :: DS
    REAL, INTENT(IN)               :: VS
    REAL, INTENT(OUT)              :: LLFLAG

    END SUBROUTINE SHPSEQ
  END INTERFACE
END MODULE SHPSEQ_mod
