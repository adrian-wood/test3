MODULE getreq_mod
  INTERFACE
SUBROUTINE GETREQ(CTYPE,REQ,ILEN,ITIME,TRANGE,IRECV,IFORM,LATEST, &
           ISTAN,ISATID,AREA,CIDENT,ORDER,IOVER,IDTYPE,IVER, &
           ICENT,TEST,MSG,FOUND,ITOD,ISTYP,IFAIL,CERR,UAPART, &
           DDICTNAME,IMODEL,RPOLE,SELECT)                     !2.2

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(INOUT)     :: CTYPE !- users subtype
CHARACTER(*), INTENT(INOUT)     :: REQ !- request string
INTEGER, INTENT(INOUT)          :: ILEN !- length of                    request string
INTEGER, INTENT(INOUT)          ::  ITIME(9) !- request times
INTEGER, INTENT(INOUT)          ::  TRANGE !- sub-period range
INTEGER, INTENT(INOUT)          ::  IRECV(10) !- cutoff times
INTEGER, INTENT(INOUT)          ::  IFORM !- 1 for current data, 2 for merged
LOGICAL, INTENT(INOUT)          ::  LATEST !- keyword LATEST
INTEGER, INTENT(INOUT)          ::  ISTAN(50) !- array of stations requested
INTEGER, INTENT(INOUT)          ::  ISATID(10) !- array of satellite ids requested
REAL, INTENT(INOUT)             ::  AREA(5) !- user-selected area !F
CHARACTER(*), INTENT(INOUT)     ::  CIDENT(50) !- character identifie   rs array
CHARACTER(*), INTENT(INOUT)     ::  ORDER !- order keyword
INTEGER, INTENT(INOUT)          ::  IOVER !- over land/sea
INTEGER, INTENT(INOUT)          ::  IDTYPE !- type of identifier
INTEGER, INTENT(INOUT)          ::  IVER !- version number of report requested
INTEGER, INTENT(INOUT)          ::  ICENT !- current century hour
LOGICAL, INTENT(INOUT)          ::  TEST !- keyword TEST
LOGICAL, INTENT(INOUT)          ::  MSG !- keyword MSG
LOGICAL, INTENT(INOUT)          ::  FOUND(:) !- array of 'found' keywords
INTEGER, INTENT(INOUT)          ::  ITOD !- 1 if request in TODAY format
INTEGER, INTENT(INOUT)          ::  ISTYP !- 1 surface, 2 uprair for STNMAS
INTEGER, INTENT(INOUT)          ::  IFAIL !- status flag from GETREQ
CHARACTER(*), INTENT(INOUT)     ::  CERR !- error message
INTEGER, INTENT(INOUT)          ::  UAPART !- 0 = A&C or B&D (Upper air retrieval) !B
CHARACTER(*), INTENT(INOUT)     ::  DDICTNAME !- Data Dictionary dats   et name !E
INTEGER, INTENT(INOUT)          ::  IMODEL !- model type (so to get right merge data)
REAL, INTENT(INOUT)             ::  RPOLE(2) !- Rotated Lat/Long pole coords !F
INTEGER, INTENT(INOUT)          ::  SELECT(50) !- SELECT keyword values !2.2

END SUBROUTINE GETREQ
END INTERFACE
END MODULE getreq_mod
