MODULE bitindx_mod
  INTERFACE
    SUBROUTINE BITINDX(CINDEX,IDESC,NELEM,IREPL,QCREQ,IFAIL, &
                       LFLAG,LOCALD,CMSG,NELREQ,DISPL,WIDTH, &
                       SCALE,REFVAL,MXSIZE,NEWMDBCALL)          !2.0!B

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)    ::  CINDEX(*)                 !1.3
    INTEGER,      INTENT(IN)    ::  NELEM ! number of elements in list
    INTEGER,      INTENT(IN)    ::  IDESC(NELEM)
    INTEGER,      INTENT(IN)    ::  IREPL(NELEM)
    LOGICAL,      INTENT(IN)    ::  QCREQ
    INTEGER,      INTENT(OUT)   ::  IFAIL ! see IRDFAIL
    LOGICAL,      INTENT(IN)    ::  LFLAG
    CHARACTER(6), INTENT(IN)    ::  LOCALD
    CHARACTER(*), INTENT(IN)    ::  CMSG
    INTEGER,      INTENT(OUT)   ::  NELREQ ! number of values in table
    INTEGER,      INTENT(IN)    ::  MXSIZE
    INTEGER,      INTENT(INOUT) ::  DISPL(MXSIZE)
    INTEGER,      INTENT(OUT)   ::  WIDTH(MXSIZE)
    INTEGER,      INTENT(OUT)   ::  SCALE(MXSIZE)
    INTEGER,      INTENT(OUT)   ::  REFVAL(MXSIZE)
    LOGICAL,      INTENT(INOUT) ::  NEWMDBCALL

    END SUBROUTINE BITINDX
  END INTERFACE
END MODULE bitindx_mod
