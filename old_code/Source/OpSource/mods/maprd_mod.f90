MODULE MAPRD_MOD
INTERFACE
SUBROUTINE MAPRD(IDSK,NXBLK,NXHRS,NAMHR,INDLEN,LFLAG,     &
                 LOCDFG,NELMIX,IBLOCK,ITRIES,CNTRY,       &
                 IDATHR,IDREC,RECLEN,CMSG,CTYPE)

IMPLICIT NONE

INTEGER,           INTENT(IN)    :: IDSK(5)    ! DATA SET DETAILS
INTEGER,           INTENT(INOUT) :: NXBLK      ! NUMBER OF FIXED INDEX BLOCKS
INTEGER,           INTENT(INOUT) :: NXHRS      ! NO OF HOURS PER INDEX
INTEGER,           INTENT(INOUT) :: NAMHR      ! START OF FIRST INDEX AFTER 00Z
INTEGER,           INTENT(IN)    :: INDLEN     ! LENGTH OF INDEX ENTRY 
LOGICAL,           INTENT(IN)    :: LFLAG      ! DIAGNOSTICS FLAG
LOGICAL,           INTENT(INOUT) :: LOCDFG     ! LOCAL TABLE D FLAG
INTEGER,           INTENT(INOUT) :: NELMIX     ! block number of element index
INTEGER,           INTENT(INOUT) :: IBLOCK     ! physical block number to read
INTEGER,           INTENT(INOUT) :: ITRIES     ! number of entries read
CHARACTER (LEN=*), INTENT(OUT)   :: CNTRY(:)   ! ARRAY OF INDEX EN      TRIES
INTEGER,           INTENT(INOUT) :: IDATHR     ! time tag of index read
INTEGER,           INTENT(INOUT) :: IDREC      ! logical record number
INTEGER,           INTENT(OUT)   :: RECLEN     ! total length of message
CHARACTER (LEN=*), INTENT(OUT)   :: CMSG       ! THE MESSAGE ITSELF 
CHARACTER (LEN=5), INTENT(IN)    :: CTYPE      ! type of block to read 

END SUBROUTINE MAPRD
END INTERFACE
END MODULE MAPRD_MOD
