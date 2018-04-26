MODULE SSMRET_MOD
INTERFACE

SUBROUTINE SSMRET(ITIME,IRTIM,AREA,ISAT,ILAND,ARRAY,NOBS,NELEM, &
                  IOBS,IDESC,NDES,ILVL,ISTAT,IERR,IDSK,LMSG,    &
                  LFLAG,QCREQ,CSTR,CREPT,SUBTYPE,NEWMDBCALL,    &
                  LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,FOUND,      &
                  ELIST)

IMPLICIT NONE

INTEGER,           INTENT(IN)    :: ITIME(9)          !- users request times
INTEGER,           INTENT(IN)    :: IRTIM(10)         !- users TOR request
REAL,              INTENT(IN)    :: AREA(5)           !- user defined area  !D
INTEGER,           INTENT(IN)    :: ISAT(10)          !- list of users satids
INTEGER,           INTENT(IN)    :: ILAND             !- user land/sea indicator
INTEGER,           INTENT(INOUT) :: NOBS              !- users no. of obs (ARRAY)
INTEGER,           INTENT(IN)    :: NELEM             !- users no. of elements (ARRAY)
REAL,              INTENT(INOUT) :: ARRAY(NOBS,NELEM) !- users array
INTEGER,           INTENT(INOUT) :: IOBS              !- next ob in users array
INTEGER,           INTENT(IN)    :: NDES              !- no. of user in-line elements
INTEGER,           INTENT(IN)    :: IDESC(NDES)       !- array of in-line elems pointers
INTEGER,           INTENT(IN)    :: ILVL(NDES)        !- user in-line elems replications
INTEGER,           INTENT(INOUT) :: ISTAT             !- MetDB status indicator
INTEGER,           INTENT(OUT)   :: IERR              !- MetDB error indicator
INTEGER,           INTENT(IN)    :: IDSK(5)           !- dataset details
LOGICAL,           INTENT(IN)    :: LMSG              ! TRUE if MESSAGE in CREQ
LOGICAL,           INTENT(IN)    :: LFLAG             ! TRUE for diagnostics on
LOGICAL,           INTENT(IN)    :: QCREQ             ! TRUE if QC bits required
CHARACTER (LEN=*), INTENT(INOUT) :: CSTR(NOBS)        ! users characters
CHARACTER (LEN=*), INTENT(INOUT) :: CREPT(NOBS)       ! users report text
CHARACTER (LEN=8), INTENT(IN)    :: SUBTYPE           ! data subtype
LOGICAL,           INTENT(INOUT) :: NEWMDBCALL        ! TRUE for new MetDB call
INTEGER,           INTENT(IN)    :: LAT_SUBSCRIPT     !- lat subscript in users request
INTEGER,           INTENT(IN)    :: LON_SUBSCRIPT     !- lon subscript in users request
REAL,              INTENT(IN)    :: RPOLE(2)          !- Rotated pole Lat Long coords
LOGICAL,           INTENT(IN)    :: FOUND(:)          ! MetDB keywords selected
CHARACTER (LEN=8), INTENT(IN)    :: ELIST             ! Element index member

END SUBROUTINE SSMRET
END INTERFACE
END MODULE SSMRET_MOD
