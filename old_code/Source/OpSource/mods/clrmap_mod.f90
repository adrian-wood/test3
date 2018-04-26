MODULE clrmap_mod
  INTERFACE
    SUBROUTINE CLRMAP (NINDEX, NBLK1, NBLK2,  &
                       LENREC, NUMMAP, MAPBLK, NUMAP, RESET)

    IMPLICIT NONE

    ! Subroutine arguments:

    INTEGER, INTENT(IN)    :: NINDEX !a1 No. base index block to be freed
    INTEGER, INTENT(IN)    :: NBLK1  !a2 Block no in data set to be checked
    INTEGER, INTENT(IN)    :: NBLK2  !a3 Block no in data set to be checked
    INTEGER, INTENT(IN)    :: LENREC !a4 Record length of storage data set
    INTEGER, INTENT(IN)    :: NUMMAP !a5 No. map blocks in storage data set
    CHARACTER(LEN=*), INTENT(INOUT) :: MAPBLK(NUMMAP) !a6 Map blocks
    INTEGER, INTENT(INOUT) :: NUMAP(NUMMAP)  !a7 Map blocks unit nos.
                                             !   (<0 if altered)
    LOGICAL, INTENT(OUT)   :: RESET !a8 TRUE if any pointers have been reset

    END SUBROUTINE CLRMAP
  END INTERFACE
END MODULE clrmap_mod
