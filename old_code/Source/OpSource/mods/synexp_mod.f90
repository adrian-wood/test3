MODULE SYNEXP_mod
  INTERFACE
    SUBROUTINE SYNEXP(IDENT,REPORT,REPLEN,FREXP,YYGGIW,IDATIM,LATLON, &
                      MIMJ,SYNTAX,NELM)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: IDENT
    CHARACTER (LEN=*), INTENT(IN)  :: REPORT
    INTEGER, INTENT(IN)            :: REPLEN
    REAL, INTENT(OUT)              :: FREXP(:)  ! Copy of Array REXP, minus associated
    !                                             data for replication count
    CHARACTER (LEN=5), INTENT(IN)  :: YYGGIW
    INTEGER, INTENT(INOUT)         :: IDATIM(5)
    REAL, INTENT(INOUT)            :: LATLON(4)
    CHARACTER (LEN=4), INTENT(IN)  :: MIMJ
    LOGICAL, INTENT(OUT)           :: SYNTAX
    INTEGER, INTENT(OUT)           :: NELM

    END SUBROUTINE SYNEXP
  END INTERFACE
END MODULE SYNEXP_mod
