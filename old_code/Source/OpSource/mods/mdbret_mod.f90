MODULE mdbret_mod
  INTERFACE
    SUBROUTINE MDBRET (ITIME, IRTIM, AREA, CHID, ISELECT, ILAND, &
               ARRAY, NOBS2, NELEM, IOBNUM, CREQ, ISTAT, IDSK, &
               LMSG, LFLAG, CSTR, CREP, SUBTYPE, RPOLE, FOUND, &
               ELIST, ICODE)

    IMPLICIT NONE
! Subroutine arguments:

    INTEGER, INTENT(IN)             ::  ITIME(8) ! User's data time window (y,m,d,hhmm)*2
    INTEGER, INTENT(IN)             ::  IRTIM(10) ! User's TOR window limits (y,m,d,h,m)*2
    REAL, INTENT(IN)                ::  AREA(5) ! User defined area specification
    CHARACTER(9), INTENT(IN)        ::  CHID(50) ! User's list of platform IDs
    INTEGER, INTENT(IN)             ::  ISELECT(50) ! User's list of data selection values
    INTEGER, INTENT(IN)             ::  ILAND ! User land/sea indicator
    INTEGER, INTENT(IN)             ::  NOBS2 ! Number of decoded obs. ARRAY can hold
    INTEGER, INTENT(IN)             ::  NELEM ! User's no. of elements
    REAL, INTENT(OUT)               ::  ARRAY(NOBS2,NELEM) ! User's array to hold decoded data
    INTEGER, INTENT(INOUT)          ::  IOBNUM ! (In/Out) Next/last ob. slot in ARRAY
    CHARACTER(*), INTENT(INOUT)     ::  CREQ ! User's request string
    INTEGER, INTENT(INOUT)          ::  ISTAT ! MetDB status indicator
    INTEGER, INTENT(IN)             ::  IDSK(5) ! Dataset details
    LOGICAL, INTENT(IN)             ::  LMSG ! TRUE if retrieving messages one by one
    LOGICAL, INTENT(IN)             ::  LFLAG ! TRUE to produce diagnostic printout
    CHARACTER(*), INTENT(OUT)       ::  CSTR(NOBS2)! Character strings from message
    CHARACTER(*), INTENT(OUT)       ::  CREP(NOBS2)! Undecoded BUFR message
    CHARACTER(8), INTENT(IN)        ::  SUBTYPE ! MetDB data subtype
    REAL, INTENT(IN)                ::  RPOLE(2) ! Rotated pole lat. & long. co-ordinates
    LOGICAL, INTENT(IN)             ::  FOUND(*) ! Flags for MetDB keywords selected
    CHARACTER(8), INTENT(IN)        ::  ELIST ! Element index member name
    INTEGER, INTENT(OUT)            ::  ICODE ! Return code

    END SUBROUTINE MDBRET
  END INTERFACE
END MODULE mdbret_mod
