MODULE freeblk_mod
  INTERFACE
    SUBROUTINE FREEBLK (NBLK1, NBLK2, MAXPTR, NUMMAP, MAPBLK, NFREE)

    IMPLICIT NONE

    ! Subroutine arguments:

    INTEGER, INTENT(IN)  :: NBLK1  !a1 First block number to be checked
    INTEGER, INTENT(IN)  :: NBLK2  !a2 Last block number to be checked
    INTEGER, INTENT(IN)  :: MAXPTR !a3 No of pointers a map block can hold
    INTEGER, INTENT(IN)  :: NUMMAP !a4 No of map blocks in storage ds
    CHARACTER(LEN=*), INTENT(IN) :: MAPBLK(NUMMAP) !a5 Map blocks
    INTEGER, INTENT(OUT) :: NFREE  !a6 Next free block in storage dS

    END SUBROUTINE FREEBLK
  END INTERFACE
END MODULE freeblk_mod
