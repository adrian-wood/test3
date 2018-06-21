MODULE arrsub_mod
  INTERFACE
    SUBROUTINE ARRSUB(FIXARR,SEGMENT,IVAL,NELREQ,DISPL,SOURCE,NVALEN, &
                      STYP,SEGST,NSEGS,QCINC,LFLAG)               !2.0

    IMPLICIT NONE

! Subroutine arguments:

    REAL,         INTENT(IN)  ::  FIXARR(*)
    INTEGER,      INTENT(IN)  ::  NELREQ
    INTEGER,      INTENT(IN)  ::  SEGMENT(NELREQ)
    INTEGER,      INTENT(IN)  ::  IVAL(NELREQ)
    INTEGER,      INTENT(OUT) ::  DISPL(NELREQ)
    INTEGER,      INTENT(OUT) ::  SOURCE(NELREQ)
    INTEGER,      INTENT(IN)  ::  NSEGS
    INTEGER,      INTENT(IN) ::  NVALEN(NSEGS)
    INTEGER,      INTENT(IN)  ::  STYP(NSEGS)
    INTEGER,      INTENT(IN)  ::  SEGST(NSEGS)
    INTEGER,      INTENT(IN)  ::  QCINC
    LOGICAL,      INTENT(IN)  ::  LFLAG

    END SUBROUTINE ARRSUB
  END INTERFACE
END MODULE arrsub_mod