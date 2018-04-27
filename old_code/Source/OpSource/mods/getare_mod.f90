MODULE getare_mod
  INTERFACE
    SUBROUTINE GETARE(REQ,IPOS,ILEN,AREA,RPOLE,IFAIL,CERR)        !C

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)    ::  REQ !- Users request
    INTEGER,      INTENT(INOUT) ::  IPOS !- Pointer within users request strng
    INTEGER,      INTENT(IN)    ::  ILEN
    REAL,         INTENT(INOUT)   ::  AREA(5) !- Users Lat/Long area  !A
    REAL,         INTENT(INOUT) ::  RPOLE(2) !- Lat/Long of rotated pole
    INTEGER,      INTENT(OUT)   ::  IFAIL !- Indicates failure in decode
    CHARACTER(*), INTENT(OUT)   ::  CERR !- Error messages

    END SUBROUTINE GETARE
  END INTERFACE
END MODULE getare_mod
