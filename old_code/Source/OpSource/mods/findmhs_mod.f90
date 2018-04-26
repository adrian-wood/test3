MODULE findmhs_mod
  INTERFACE
    SUBROUTINE FINDMHS (NFT, OWNER, MESAGE, MSTART, MLNGTH, MSGCODE, MHSTYPE)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,           INTENT(IN)    :: NFT     !a01 UNIT NUMBER OF MESSAGE FILE
    CHARACTER(LEN=*),  INTENT(IN)    :: OWNER   !a02 OWNERSHIP OF REQUIRED DATASET
    CHARACTER(LEN=*),  INTENT(INOUT) :: MESAGE  !a03 28K BUFFER CONTAINING MESSAGE(S)
    INTEGER,           INTENT(OUT)   :: MSTART  !a04 POINTER TO START OF MESSAGE
    INTEGER,           INTENT(OUT)   :: MLNGTH  !a05 LENGTH OF MESSAGE (BYTES)
    INTEGER,           INTENT(OUT)   :: MSGCODE !a06 RETURN CODE FROM "FINDMHS"
    CHARACTER(LEN=1),  INTENT(IN)    :: MHSTYPE !a07 MHS D/s type

    END SUBROUTINE FINDMHS
  END INTERFACE
END MODULE findmhs_mod
