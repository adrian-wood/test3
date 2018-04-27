MODULE SETDEF_MOD
  INTERFACE
    SUBROUTINE SETDEF (ITIME,TRANGE,IRECV,IFORM,LATEST,ISTAN,ISATID, &
                       AREA,CIDENT,IOVER,IVER,ORDER,TEST,MSG,ISTYP,  &
                       UAPART,DDICTNAME,IMODEL,RPOLE,IDTYPE,SELECT)
    IMPLICIT NONE
!                                  Subroutine arguments

    INTEGER,      INTENT(OUT)  ::  ITIME(9)
    INTEGER,      INTENT(OUT)  ::  TRANGE
    INTEGER,      INTENT(OUT)  ::  IRECV(10)
    INTEGER,      INTENT(OUT)  ::  IFORM
    LOGICAL,      INTENT(OUT)  ::  LATEST
    INTEGER,      INTENT(OUT)  ::  ISTAN(50)
    INTEGER,      INTENT(OUT)  ::  ISATID(10)
    REAL,         INTENT(OUT)  ::  AREA(5)
    CHARACTER(9), INTENT(OUT)  ::  CIDENT(50)
    INTEGER,      INTENT(OUT)  ::  IOVER
    INTEGER,      INTENT(OUT)  ::  IVER
    CHARACTER(1), INTENT(OUT)  ::  ORDER
    LOGICAL,      INTENT(OUT)  ::  TEST
    LOGICAL,      INTENT(OUT)  ::  MSG
    INTEGER,      INTENT(OUT)  ::  ISTYP
    INTEGER,      INTENT(OUT)  ::  UAPART
    CHARACTER(*), INTENT(OUT)  ::  DDICTNAME
    INTEGER,      INTENT(OUT)  ::  IMODEL
    REAL,         INTENT(OUT)  ::  RPOLE(2)
    INTEGER,      INTENT(OUT)  ::  IDTYPE     !- STNMAS identifier type
    INTEGER,      INTENT(OUT)  ::  SELECT(50) !- SELECT keyword values

    END SUBROUTINE SETDEF
  END INTERFACE
END MODULE SETDEF_MOD
