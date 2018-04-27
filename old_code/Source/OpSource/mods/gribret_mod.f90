MODULE gribret_mod
  INTERFACE
SUBROUTINE GRIBRET (DATYPE, DSNX, NOBS, IDATA, KEYWRD, LTEST, &
                    ISELECT, ITIME, ISTAT, CREP, RVALS)       !2.2

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(IN)        ::  DATYPE ! MET.D.B.DATA TYPE
CHARACTER(*), INTENT(IN)        ::  DSNX ! NAME OF INDEX DATA SET
INTEGER, INTENT(INOUT)          ::  NOBS ! NO. OF ELEMENTS IN USER BUFFER
INTEGER, INTENT(IN)             ::  IDATA(5) ! INFORMATION ABOUT INDEX DATA SET (SEE ABOVE)
LOGICAL, INTENT(IN)             ::  KEYWRD(:) ! MDB KEYWORDS (= 'FOUND' ARRAY IN 'MDB') !2.2
LOGICAL, INTENT(IN)             ::  LTEST ! .TRUE. IF DIAGNOSTIC PRINTOUT IS REQUIRED
INTEGER, INTENT(IN)             ::  ISELECT(50) ! USER'S LIST OF DATA SELECTION VALUES !3
INTEGER, INTENT(INOUT)          ::  ITIME(8) ! START AND END TIMES OF REQUEST PERIOD
INTEGER, INTENT(INOUT)          ::  ISTAT ! STATUS CODE RETURNED BY THIS ROUTINE
CHARACTER(*), INTENT(OUT)       ::  CREP(NOBS) ! USER BUFFER TO HOLD    GRIB DATA
REAL, INTENT(OUT)               ::  RVALS(NOBS) ! NUMBER OF OUTPUT BYTES IN EACH 'CREP' ELEMENT

END SUBROUTINE GRIBRET
END INTERFACE
END MODULE gribret_mod
