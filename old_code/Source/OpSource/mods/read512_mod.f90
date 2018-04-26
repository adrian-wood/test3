MODULE read512_mod
  INTERFACE
    SUBROUTINE READ512 (NFT, BUFREC, NFIRST, KODE)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN)    :: NFT    !a01 Unit number of message file being read
    CHARACTER(LEN=*), INTENT(INOUT) :: BUFREC !a02 28K Buffer to hold bulletins
    INTEGER,          INTENT(INOUT) :: NFIRST !a03 First byte which must be kept
    INTEGER,          INTENT(INOUT) :: KODE   !a04 Return code (see above for coding)

    END SUBROUTINE READ512
  END INTERFACE
END MODULE read512_mod
