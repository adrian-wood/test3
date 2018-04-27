MODULE upaind_mod
  INTERFACE
    SUBROUTINE UPAIND(ELMNUM,SEGNUM,SUBNUM,NROWS,STYP,SEGST, &
                      SEGLEN,NSEGS)                               !2.0

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,      INTENT(OUT)   ::  ELMNUM(:)
    INTEGER,      INTENT(OUT)   ::  SEGNUM(:)
    INTEGER,      INTENT(OUT)   ::  SUBNUM(:)
    INTEGER,      INTENT(OUT)   ::  NROWS
    INTEGER,      INTENT(OUT)   ::  STYP(:)
    INTEGER,      INTENT(OUT)   ::  SEGST(:)
    INTEGER,      INTENT(OUT)   ::  SEGLEN(:)
    INTEGER,      INTENT(OUT)   ::  NSEGS

    END SUBROUTINE UPAIND
  END INTERFACE
END MODULE upaind_mod
