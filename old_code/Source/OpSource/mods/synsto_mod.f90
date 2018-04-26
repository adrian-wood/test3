MODULE SYNSTO_mod
  INTERFACE
    SUBROUTINE SYNSTO(DATIME,ENTRY,BULL,IFT,BLKSIZ,IDENT)

    IMPLICIT NONE

    INTEGER, INTENT(INOUT)            :: DATIME(5)        ! DATE/TIME (YEAR, MONTH...),                    a1
    CHARACTER (LEN=*), INTENT(INOUT)  :: ENTRY            ! INDEX ENTRY (WITHOUT TIMES & BLOCK/RECORD NO), a2
    CHARACTER (LEN=*), INTENT(IN)     :: BULL             ! REPORT TO BE STORED,                           a3
    INTEGER, INTENT(IN)               :: IFT              ! FT NUMBER (ASSUME IT'S ALWAYS THE SAME),       a4
    INTEGER, INTENT(IN)               :: BLKSIZ           ! BLOCKSIZE OF OUTPUT DATA SET.                  a5
    CHARACTER (LEN=*), INTENT(IN)     :: IDENT            ! IDENTIFIER TO GO IN INDEX ENTRY,               a6

    END SUBROUTINE SYNSTO
  END INTERFACE
END MODULE SYNSTO_mod
