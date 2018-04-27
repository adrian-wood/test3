module msgsubn_mod
  INTERFACE
   SUBROUTINE MSGSUBN(MESSAGE,SEGMENT,IVAL,IDSC,DISPL,SOURCE,IEXTRA, &
   NELEM, IREPL,QCREQ, &
   INPUT_NSEG,INPUT_SEGNUM,INPUT_STYP,INPUT_NDSLEN,INPUT_NVALEN, &
   VALUES,VALDIM,CNAME,NOBS,LFLAG,MEXT)
INTEGER                 ::   NELEM
INTEGER                 ::   IVAL(NELEM)
INTEGER                 ::   VALDIM
INTEGER                 ::   MEXT  ! Size of IEXTRA array
CHARACTER(LEN=*)        ::   MESSAGE
INTEGER                 ::   SEGMENT(NELEM)
INTEGER                 ::   IDSC(NELEM)
INTEGER                 ::   DISPL(NELEM)
INTEGER                 ::   SOURCE(NELEM)
INTEGER                 ::   IREPL(:)
INTEGER                 ::   IEXTRA(MEXT)
INTEGER  ::   INPUT_NSEG                ! Number of segments
INTEGER  ::   INPUT_SEGNUM(INPUT_NSEG)  ! Segment number
INTEGER  ::   INPUT_STYP(INPUT_NSEG)    ! Segment type (8=replication)
INTEGER  ::   INPUT_NDSLEN(INPUT_NSEG)  ! No. of descriptors in segment
INTEGER  ::   INPUT_NVALEN(INPUT_NSEG)  ! No. of values in segment

REAL               ::   VALUES(VALDIM)    ! value array for decode
CHARACTER(len=*)   ::   CNAME    ! character strings for decode
INTEGER            ::   NOBS
LOGICAL            ::   LFLAG
LOGICAL            ::   QCREQ
  END SUBROUTINE MSGSUBN
  END INTERFACE
 END MODULE msgsubn_mod
