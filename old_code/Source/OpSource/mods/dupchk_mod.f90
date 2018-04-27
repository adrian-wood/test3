MODULE dupchk_mod
  INTERFACE
    SUBROUTINE DUPCHK (ENTRY, LENTRY, LENCHK, NDXBLK, NUMNDX, KODE)

    IMPLICIT NONE

    ! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN) :: ENTRY !a1 New index entry
    INTEGER, INTENT(IN)    :: LENTRY !a2 Length of index entries (in bytes)
    INTEGER, INTENT(IN)    :: LENCHK !a3 Len of index used for duplicate chk
    INTEGER, INTENT(IN)    :: NUMNDX !a5 Number of index blocks in chain
    CHARACTER(LEN=*), INTENT(IN)    :: NDXBLK(NUMNDX) !a4 Chain of idx blks
    INTEGER, INTENT(OUT)   :: KODE   !a6 Return code (see above for values)

    END SUBROUTINE DUPCHK
  END INTERFACE
END MODULE dupchk_mod
