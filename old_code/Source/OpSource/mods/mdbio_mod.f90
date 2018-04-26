MODULE MDBIO_mod
  INTERFACE
    SUBROUTINE MDBIO(IDSK,NXBLK,NXHRS,NAMHR,INDLEN,LOCDFG,    &
                     NELMIX,IBLOCK,ITRIES,CNTRY,IDATHR,IDREC, &
                     RECLEN,CMSG,CTYPE)

    IMPLICIT NONE

    INTEGER, INTENT(IN)             :: IDSK(5)   ! DATA SET DETAILS
    INTEGER, INTENT(OUT)            :: NXBLK     ! NUMBER OF FIXED INDEX BLOCKS
    INTEGER, INTENT(OUT)            :: NXHRS     ! NO OF HOURS PER INDEX
    INTEGER, INTENT(OUT)            :: NAMHR     ! START OF FIRST INDEX AFTER 00Z
    INTEGER, INTENT(IN)             :: INDLEN    ! LENGTH OF INDEX ENTRY
    LOGICAL, INTENT(OUT)            :: LOCDFG    ! LOCAL TABLE D FLAG
    INTEGER, INTENT(OUT)            :: NELMIX    ! block number of element index
    INTEGER, INTENT(IN)             :: IBLOCK
    INTEGER, INTENT(OUT)            :: ITRIES    ! number of index entries
    CHARACTER (LEN=*), INTENT(OUT)  :: CNTRY(:)  ! ARRAY OF INDEX ENTRIES
    INTEGER, INTENT(OUT)            :: IDATHR
    INTEGER, INTENT(IN)             :: IDREC
    INTEGER, INTENT(OUT)            :: RECLEN
    CHARACTER (LEN=*), INTENT(OUT)  :: CMSG      ! THE MESSAGE ITSELF
    CHARACTER (LEN=5), INTENT(IN)   :: CTYPE     ! type of block to read

    END SUBROUTINE MDBIO
  END INTERFACE
END MODULE MDBIO_mod
