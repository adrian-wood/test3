MODULE sklsubn_mod
 INTERFACE
  SUBROUTINE SKLSUBN(IDESC,NELEM,LMINDEX,NINDEX,QCREQ, &
   SEGMENT,IVAL,IDSC,NELREQ,LFLAG)
   INTEGER            ::  NELEM
   INTEGER            ::  IDESC(NELEM)
   INTEGER            ::  NINDEX
   INTEGER            ::  SEGMENT(:)
   INTEGER            ::  IVAL(:)
   INTEGER            ::  IDSC(:)
   INTEGER            ::  NELREQ
   LOGICAL            ::  LFLAG
   LOGICAL            ::  QCREQ
   CHARACTER(LEN=*)   ::  LMINDEX(:)
  END SUBROUTINE SKLSUBN
 END INTERFACE
END MODULE sklsubn_mod
