MODULE gribopen_mod
  INTERFACE
SUBROUTINE GRIBOPEN (NYMDH, SKEL, LTEST, IUNIT, LENREC, KODE)

IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)             ::  NYMDH(4) ! YEAR, MONTH, DAY, HOUR FOR DATA SET NAME
CHARACTER(*), INTENT(IN)        ::  SKEL ! SKELETON DATA SET NAME
LOGICAL, INTENT(IN)             ::  LTEST ! .TRUE. IF DIAGNOSTICS WANTED
INTEGER, INTENT(OUT)            ::  IUNIT ! DATA SET UNIT NUMBER
INTEGER, INTENT(OUT)            ::  LENREC ! DATA SET RECORD LENGTH
INTEGER, INTENT(OUT)            ::  KODE ! RETURN CODE FROM MDBRSN

END SUBROUTINE GRIBOPEN
END INTERFACE
END MODULE gribopen_mod