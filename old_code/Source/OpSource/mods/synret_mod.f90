MODULE synret_mod
  INTERFACE
    SUBROUTINE SYNRET(CTYPE,ITIME,IRTIM,AREA,CIDENT,ARRAY,NOBS, &
                      NELEM,IOBS,IDESC,NDES,IREPL,ISTAT,IERR,IDSK, &
                      CSTR,CREPRT,LFLAG,CHAREP,ORDER,LATEST,QCREQ, &
                      FOUND,VER,NEWMDBCALL,RPOLE,WANTSTNNAME,      &  !L!I!H
                      TRANGE,ELIST)                               !2.3

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)    ::  CTYPE
    INTEGER,      INTENT(IN)    ::  ITIME(9)
    INTEGER,      INTENT(IN)    ::  IRTIM(10)
    REAL,         INTENT(IN)    ::  AREA(5) !- user-defined area    !I
    CHARACTER(9), INTENT(IN)    ::  CIDENT(50)
    INTEGER,      INTENT(INOUT) ::  NOBS !- NOBS in users array.
    INTEGER,      INTENT(IN)    ::  NELEM !- NELEM in users array.
    REAL,         INTENT(OUT)   ::  ARRAY(NOBS,NELEM) !- users array.
    INTEGER,      INTENT(INOUT) ::  IOBS
    INTEGER,      INTENT(IN)    ::  NDES
    INTEGER,      INTENT(IN)    ::  IDESC(1+NDES) !- 1+NDES in case NDES=0
    INTEGER,      INTENT(IN)    ::  IREPL(:)                        !B
    INTEGER,      INTENT(INOUT) ::  ISTAT
    INTEGER,      INTENT(OUT)   ::  IERR
    INTEGER,      INTENT(IN)    ::  IDSK(5)
    CHARACTER(*), INTENT(OUT)   ::  CSTR(NOBS)
    CHARACTER(*), INTENT(OUT)   ::  CREPRT(NOBS)
    LOGICAL,      INTENT(IN)    ::  LFLAG ! TRUE if TEST diagnostics wanted
    LOGICAL,      INTENT(IN)    ::  CHAREP ! TRUE if raw report wanted
    CHARACTER(1), INTENT(IN)    ::  ORDER
    LOGICAL,      INTENT(IN)    ::  LATEST ! TRUE if latest ob wanted
    LOGICAL,      INTENT(IN)    ::  QCREQ ! TRUE if QC flags wanted for each elem
    LOGICAL,      INTENT(IN)    ::  FOUND(:) ! TRUE for each keyword if requested
    INTEGER,      INTENT(IN)    ::  VER
    LOGICAL,      INTENT(INOUT)    ::  NEWMDBCALL ! TRUE if new MetDB call (istat=0) !H
    REAL,         INTENT(IN)    ::  RPOLE(2) !- rotated pole lat/lon coords !I
    LOGICAL,      INTENT(IN)    ::  WANTSTNNAME ! TRUE if user wants station name !L
    INTEGER,      INTENT(IN)    ::  TRANGE !- START TIME sub-period !P
    CHARACTER(8), INTENT(IN)    ::  ELIST ! element index member name !2.3

    END SUBROUTINE SYNRET
  END INTERFACE
END MODULE synret_mod
