MODULE VALARR_mod
 INTERFACE
  SUBROUTINE VALARR(IDISP,IMD,SOURCE,INTELM,CHRELM,LENCHR,IEXTRA,&
  VALUES,CNAM,CREP,IREP1,NSEND,NREP,                             &
  ARRAY,NOBS,NELEM,IOB,II,CSTR,CREPRT,LFLAG,                     &
  LAT_SUBSCRIPT,LON_SUBSCRIPT,AREA,RPOLE)
  INTEGER             ::    IDISP(IMD)
  INTEGER             ::    IEXTRA(:)
  INTEGER             ::    II
  INTEGER             ::    INTELM(:)
  INTEGER             ::    IOB
  INTEGER             ::    IREP1
  INTEGER             ::    LAT_SUBSCRIPT
  INTEGER             ::    LENCHR(:)
  INTEGER             ::    LON_SUBSCRIPT
  INTEGER             ::    NELEM
  INTEGER             ::    NOBS
  INTEGER             ::    NREP
  INTEGER             ::    NSEND
  INTEGER             ::    SOURCE(IMD)
  REAL                ::    AREA(5)  !- Users Lat/Lon box selection
  REAL                ::    ARRAY(NOBS,NELEM)  !- Users array
  REAL                ::    RPOLE(2)  !- Rotated Lat Lon pole coords
  REAL                ::    VALUES(NREP,*)     !- Changed from VALUES(*)
  LOGICAL             ::    LFLAG     !- TRUE for diagnostics
  CHARACTER(LEN=*)    ::    CHRELM(:)
  CHARACTER(LEN=*)    ::    CNAM
  CHARACTER(LEN=*)    ::    CREP
  CHARACTER(LEN=*)    ::    CREPRT(NOBS)
  CHARACTER(LEN=*)    ::    CSTR(NOBS)
  END SUBROUTINE VALARR
 END INTERFACE
END MODULE
