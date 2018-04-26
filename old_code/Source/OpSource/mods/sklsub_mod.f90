MODULE sklsub_mod
 INTERFACE
  SUBROUTINE SKLSUB(IDESC,NELEM,IREPL,LMINDEX,NINDEX,MESTRUCT,     &
  QCREQ,SEGMENT,IVAL,IDSC,NELREQ,NSEG,STYP,                        &
  NVALEN,NDSLEN,MAXELM,MAXSEG,LFLAG)
  INTEGER              ::    NELEM
  INTEGER              ::    MAXELM
  INTEGER              ::    MAXSEG
  INTEGER              ::    IDESC(NELEM)
  INTEGER              ::    IREPL(NELEM)
  CHARACTER(LEN=*)     ::    LMINDEX(:) !- ELEMENT INDEX ARRAY
  INTEGER              ::    NINDEX
  CHARACTER(LEN=*)     ::    MESTRUCT
  LOGICAL              ::    QCREQ
  INTEGER              ::    SEGMENT(MAXELM)
  INTEGER              ::    IVAL(MAXELM)
  INTEGER              ::    IDSC(MAXELM)
  INTEGER              ::    NELREQ
  INTEGER              ::    NSEG
  INTEGER              ::    STYP(MAXSEG)
  INTEGER              ::    NVALEN(MAXSEG)
  INTEGER              ::    NDSLEN(MAXSEG)
  LOGICAL              ::    LFLAG
  END SUBROUTINE SKLSUB
 END INTERFACE
END MODULE sklsub_mod
