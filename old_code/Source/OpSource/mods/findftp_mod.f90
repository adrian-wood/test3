MODULE findftp_mod
  INTERFACE
    SUBROUTINE FINDFTP &
               (NFT, OWNER, MESAGE, MSTART, MLNGTH, DSNAME, MSGCODE, MHSTYPE)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,           INTENT(IN)    :: NFT     !a01 Unit number of message file
    CHARACTER(LEN=*),  INTENT(IN)    :: OWNER   !a02 Mhs ownership of required dataset
    CHARACTER(LEN=*),  INTENT(INOUT) :: MESAGE  !a03 28K buffer containing message(s)
    INTEGER,           INTENT(OUT)   :: MSTART  !a04 Pointer to start of BUFR message
    INTEGER,           INTENT(OUT)   :: MLNGTH  !a05 Length of BUFR message (bytes)
    CHARACTER(LEN=44), INTENT(OUT)   :: DSNAME  !a06 Data set name (returned to calling pgm.)
    INTEGER,           INTENT(OUT)   :: MSGCODE !a07 Return code (see comments above)
    CHARACTER(LEN=1),  INTENT(IN)    :: MHSTYPE !a08 MHS d/s type (see comments above)

    END SUBROUTINE FINDFTP
  END INTERFACE
END MODULE findftp_mod
