MODULE tfmret_mod
  INTERFACE
    SUBROUTINE TFMRET(CTYPE,ITIME,TRANGE,IRTIM,AREA,CIDENT, &     !2.0
                      ARRAY,NOBS,NELEM,IOBS,IDESC,NDES,     &     !2.0
                      ISTAT,IERR,IDSK,CSTR,CREPRT,LFLAG,    &     !2.0
                      CHAREP,ORDER,LATEST,TAFLEN,METSPECI,  & !1.18!H!G
                      FOUND,RPOLE,IVER)                             !G

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN) ::  CTYPE
    INTEGER,      INTENT(IN) ::  ITIME(9) !
    INTEGER,      INTENT(IN) ::  TRANGE !
    INTEGER,      INTENT(IN) ::  IRTIM(10) !
    REAL,         INTENT(IN) ::  AREA(5) !- user-defined area
    CHARACTER(9), INTENT(IN) ::  CIDENT(50)
    INTEGER,      INTENT(INOUT) ::  NOBS !
    INTEGER,      INTENT(INOUT) ::  NELEM !
    REAL,         INTENT(INOUT) ::  ARRAY(NOBS,NELEM) !- users array
    INTEGER,      INTENT(INOUT) ::  IOBS !
    INTEGER,      INTENT(INOUT) ::  NDES !
    INTEGER,      INTENT(INOUT) ::  IDESC(NDES) !
    INTEGER,      INTENT(INOUT) ::  ISTAT !
    INTEGER,      INTENT(OUT) ::  IERR !
    INTEGER,      INTENT(INOUT) ::  IDSK(5) !
    CHARACTER(*), INTENT(INOUT) ::  CSTR(NOBS)
    CHARACTER(*), INTENT(INOUT) ::  CREPRT(NOBS)
    LOGICAL,      INTENT(IN) ::  LFLAG
    LOGICAL,      INTENT(IN) ::  CHAREP
    CHARACTER(1), INTENT(IN) ::  ORDER
    LOGICAL,      INTENT(IN) ::  LATEST
    INTEGER,      INTENT(IN) ::  TAFLEN !
    INTEGER,      INTENT(IN) ::  METSPECI !
    LOGICAL,      INTENT(IN) ::  FOUND(:)
    REAL,         INTENT(INOUT) ::  RPOLE(2) !- rotated pole lat/lon
    INTEGER,      INTENT(IN) ::  IVER !

    END SUBROUTINE TFMRET
  END INTERFACE
END MODULE tfmret_mod
