MODULE busret_mod
  INTERFACE
    SUBROUTINE BUSRET(SUBTYPE,ITIME,IRTIM,AREA,CHID,ARRAY, &
       NOBS,NELEM,IOBS,IDESC,NDES,ILVL,ISTAT,IERR,       &
       IDSK,CSTR,CREPT,LCHREP,LFLAG,QCREQ,               &
       NEWMDBCALL,LAT_SUBSCRIPT,LON_SUBSCRIPT,           &
       RPOLE,LMSG,FOUND,ELIST)

IMPLICIT NONE

CHARACTER(LEN=8),INTENT(IN)  :: SUBTYPE      !- MetDB subtype
INTEGER,INTENT(IN)           :: ITIME(9) !- users request times
INTEGER,INTENT(IN)           :: IRTIM(10)!- users TOR request
REAL,INTENT(IN)              :: AREA(5)  !- user defined area
CHARACTER(LEN=9),INTENT(IN)  :: CHID(50) !- list of users ids wanted
INTEGER,INTENT(INOUT)        :: NOBS     !- users no. of obs (ARRAY)
INTEGER,INTENT(IN)           :: NELEM    !- users no.of elements (ARRAY)
REAL,INTENT(INOUT)           :: ARRAY(NOBS,NELEM) !- users array
INTEGER,INTENT(INOUT)        :: IOBS    !- next ob in users array
INTEGER,INTENT(IN)           :: NDES !- no. of user in-line elements
INTEGER,INTENT(IN)           :: IDESC(NDES)
                             !- array of in-line element pointers
INTEGER,INTENT(IN)           ::  ILVL(NDES)
                             !- user in-line elems replications
INTEGER,INTENT(INOUT)        :: ISTAT      !- MetDB status indicator
INTEGER,INTENT(OUT)          :: IERR       !- MetDB error indicator
INTEGER,INTENT(IN)           :: IDSK(5)    !- dataset details
CHARACTER(LEN=*),INTENT(INOUT) :: CSTR(NOBS) !- users character strings
CHARACTER(LEN=*),INTENT(INOUT) :: CREPT(NOBS)!- users report text
LOGICAL,INTENT(IN)           :: LCHREP !-TRUE if report text only wanted
LOGICAL,INTENT(IN)           :: LFLAG  !- TRUE for diagnostics on
LOGICAL,INTENT(IN)           :: QCREQ  !- TRUE for QC flags required
LOGICAL,INTENT(INOUT)        :: NEWMDBCALL    !- TRUE for new MetDB call
INTEGER,INTENT(IN)           :: LAT_SUBSCRIPT !- users lat subscript
INTEGER,INTENT(IN)           :: LON_SUBSCRIPT !- users lon subscript
REAL,INTENT(IN)              :: RPOLE(2) !- Lat Long of rotated pole
LOGICAL,INTENT(IN)           :: LMSG     !- TRUE if MESSAGE in CREQ
LOGICAL,INTENT(IN)           :: FOUND(:) !- MetDB keywords array
CHARACTER(LEN=8),INTENT(IN)  :: ELIST    !- Element index member name

    END SUBROUTINE BUSRET
  END INTERFACE
END MODULE busret_mod
