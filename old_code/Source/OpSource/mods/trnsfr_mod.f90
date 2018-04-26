MODULE trnsfr_mod
  INTERFACE
    SUBROUTINE TRNSFR(IDISP,IMD,IREP1,NSEND,CSTR,IOB,ARRAY,CREP,NOBS, &
                      NELEM,ISECT1,NREP,VALUES,CNAM,CREPRT,II,LFLAG)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,      INTENT(INOUT) ::  IMD                           !2.0
    INTEGER,      INTENT(INOUT) ::  IDISP(IMD)
    INTEGER,      INTENT(INOUT) ::  IREP1
    INTEGER,      INTENT(INOUT) ::  NSEND
    INTEGER,      INTENT(INOUT) ::  NOBS                          !2.0
    CHARACTER(*), INTENT(INOUT) ::  CSTR(NOBS)
    INTEGER,      INTENT(INOUT) ::  IOB
    INTEGER,      INTENT(INOUT) ::  NELEM                         !2.0
    REAL,         INTENT(INOUT) ::  ARRAY(NOBS,NELEM)
    CHARACTER(*), INTENT(INOUT) ::  CREP
    INTEGER,      INTENT(INOUT) ::  ISECT1(9)
    INTEGER,      INTENT(INOUT) ::  NREP
    REAL,         INTENT(INOUT) ::  VALUES(:)
    CHARACTER(*), INTENT(INOUT) ::  CNAM
    CHARACTER(*), INTENT(INOUT) ::  CREPRT(NOBS)
    INTEGER,      INTENT(INOUT) ::  II
    LOGICAL,      INTENT(IN) ::  LFLAG

    END SUBROUTINE TRNSFR
  END INTERFACE
END MODULE trnsfr_mod
