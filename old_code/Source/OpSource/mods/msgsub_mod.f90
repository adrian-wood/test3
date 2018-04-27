MODULE msgsub_mod
 INTERFACE
   SUBROUTINE MSGSUB(MESSAGE,SEGMENT,IVAL,IDSC,DISPL,SOURCE,IEXTRA,&
   NELEM,NSEG,STYP,NDSLEN,NVALEN,VALUES,MDATA,    &
   CNAME,INOBS,LFLAG,LOCD,MEXT)
   INTEGER               :: NELEM
   INTEGER               :: NSEG
   INTEGER               :: MDATA
   INTEGER               :: MEXT  !- SIZE OF IEXTRA ARRAY
   CHARACTER(LEN=*)      :: MESSAGE
   INTEGER               :: SEGMENT(NELEM)
   INTEGER               :: IVAL(NELEM)
   INTEGER               :: IDSC(NELEM)
   INTEGER               :: DISPL(NELEM)
   INTEGER               :: SOURCE(NELEM)
   INTEGER               :: IEXTRA(MEXT)
   INTEGER               :: STYP(NSEG)
   INTEGER               :: NDSLEN(NSEG)
   INTEGER               :: NVALEN(NSEG)
   REAL                  :: VALUES(MDATA)
   CHARACTER(LEN=*)      :: CNAME
   CHARACTER(LEN=*)      :: LOCD
   INTEGER               :: INOBS
   LOGICAL               :: LFLAG
  END SUBROUTINE MSGSUB
 END INTERFACE
END MODULE msgsub_mod
