MODULE getvals_mod
  INTERFACE
SUBROUTINE GETVALS (LISTDES, LISTLEN, MESSAGE, MAXVALS, VALUES, &
                    CSTR, NPOS, NUMOBS, KODE)
IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)             ::  LISTLEN ! Number of descriptors to be decoded
INTEGER, INTENT(IN)             ::  LISTDES(LISTLEN) ! List of descriptors to be decoded
CHARACTER(*), INTENT(IN)        ::  MESSAGE ! BUFR message
INTEGER, INTENT(IN)             ::  MAXVALS ! Size of VALUES array
REAL, INTENT(OUT)               ::  VALUES(MAXVALS) ! Parameter values returned by DECORD
CHARACTER(*), INTENT(OUT)       ::  CSTR ! Character elements from DE   CORD !2.2
INTEGER, INTENT(OUT)            ::  NPOS(LISTLEN) ! Position of LISTDES items in sequence
INTEGER, INTENT(OUT)            ::  NUMOBS ! Number of observations in message
INTEGER, INTENT(OUT)            ::  KODE ! Return code

END SUBROUTINE GETVALS
END INTERFACE
END MODULE getvals_mod
