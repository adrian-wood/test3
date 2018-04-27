MODULE merbitx_mod
  INTERFACE
      SUBROUTINE MERBITX(SEQDES,NL,FS,XS,YS,SCALES,REFVALS,WIDTHS,BITS)
      IMPLICIT NONE
      INTEGER SEQDES       ! input (local) sequence descriptor
      INTEGER NL           ! current line number in table
      INTEGER FS(*)
      INTEGER XS(*)
      INTEGER YS(*)
      INTEGER SCALES(*)    ! scales returned
      INTEGER REFVALS(*) ! reference values returned
      INTEGER WIDTHS(*)    ! widths returned
      INTEGER BITS(*)      ! bits before value
      END SUBROUTINE MERBITX
  END INTERFACE
END MODULE merbitx_mod
