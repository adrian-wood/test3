MODULE uprret_mod
  INTERFACE
    SUBROUTINE UPRRET(CTYPE,LFLAG,ISTAT,IDSK,ITIME,IRTIM,FOUND, &
                      UAPART,CIDENT,AREA,IERR,IOVER,VER,IDESC,  &   !J
                      NDES,IREPL,QCREQ,ARRAY,NOBS,NELEM,CSTR, &
                      CREPRT,IOBS,WANTTEMPS,WANTWINDS,NUMTEMPS, &
                      NUMWINDS,TRANGE,NEWMDBCALL,CRTYP,         & !I!E
                      LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,ELIST)    !2.4

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)    ::  CTYPE !- retrieval subtype
    LOGICAL,      INTENT(IN)    ::  LFLAG !- diagnostics output
    INTEGER,      INTENT(INOUT) ::  ISTAT !- MetDB state indicator
    INTEGER,      INTENT(IN)    ::  IDSK(5) !- dataset storage details
    INTEGER,      INTENT(IN)    ::  ITIME(9) !- users end time in century hours
    INTEGER,      INTENT(IN)    ::  IRTIM(10) !- users time of receipt
    LOGICAL,      INTENT(IN)    ::  FOUND(:) !- array of keywords selected
    INTEGER,      INTENT(IN)    ::  UAPART !- 0=A&C or B&D, 1=AorB, 2=CorD
    CHARACTER(9), INTENT(IN)    ::  CIDENT(50) !- station identifiers
    REAL,         INTENT(IN)    ::  AREA(5) !- user-defined area    !J
    INTEGER,      INTENT(OUT)   ::  IERR !- error indicator
    INTEGER,      INTENT(IN)    ::  IOVER !- land/sea user-selection
    INTEGER,      INTENT(IN)    ::  VER !- 1=preferred (default), 2=all
    INTEGER,      INTENT(IN)    ::  NDES !- number of user elements !2.0
    INTEGER,      INTENT(IN)    ::  IDESC(1+NDES) !- array of user required element pointers
    INTEGER,      INTENT(IN)    ::  IREPL(:) !- array of user required replications
    LOGICAL,      INTENT(IN)    ::  QCREQ !- TRUE if users wants QC elements
    INTEGER,      INTENT(INOUT) ::  NOBS !- users no. of observations.
    INTEGER,      INTENT(IN)    ::  NELEM !- users no. of elements
    REAL,         INTENT(INOUT) ::  ARRAY(NOBS,NELEM) !- users array
    CHARACTER(*), INTENT(INOUT) ::  CSTR(NOBS) !- users strings
    CHARACTER(*), INTENT(INOUT) ::  CREPRT(NOBS) !- users report string
    INTEGER,      INTENT(INOUT) ::  IOBS !- next ob in users array
    LOGICAL,      INTENT(IN)    ::  WANTTEMPS !- TRUE if user wants temp levels
    LOGICAL,      INTENT(IN)    ::  WANTWINDS !- TRUE if user wants wind levels
    INTEGER,      INTENT(IN)    ::  NUMTEMPS !- number of user temp levels wanted
    INTEGER,      INTENT(IN)    ::  NUMWINDS !- number of user wind levels wanted
    INTEGER,      INTENT(IN)    ::  TRANGE !- users START TIME sub period.
    LOGICAL,      INTENT(INOUT) ::  NEWMDBCALL !- TRUE if new MetDB call (istat=0) !E
    CHARACTER(*), INTENT(IN)    ::  CRTYP !- type of data stored !I
    INTEGER,      INTENT(IN)    ::  LAT_SUBSCRIPT !- users lat subscript !J
    INTEGER,      INTENT(IN)    ::  LON_SUBSCRIPT !- users lon subscript !J
    REAL,         INTENT(IN)    ::  RPOLE(2) !- rotated pole lat lon coords !J
    CHARACTER(*), INTENT(IN)    ::  ELIST  !- elidx member      !2.4

    END SUBROUTINE UPRRET
  END INTERFACE
END MODULE uprret_mod
