MODULE bufrep_mod
  INTERFACE
    SUBROUTINE BUFREP (IUNIT, LENREC, NTTOR, FLAGS, ITEMS, NSEQ,  &
                       BULL, KODE)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER, INTENT(IN)    :: IUNIT    !a1 Unit number of storage data set
    INTEGER, INTENT(IN)    :: LENREC   !a2 Record length of storage data set
    INTEGER, INTENT(IN)    :: NTTOR(8) !a3 Date/time of receipt (from DATIM)
    LOGICAL, INTENT(IN)    :: FLAGS(:) !a4 Processing flags for cur data type
    INTEGER, INTENT(IN)    :: ITEMS(:) !a5 Processing items for cur data type
    INTEGER, INTENT(IN)    :: NSEQ     !a6 BUFR seq from storage d/s ('FXXYYY')
    CHARACTER(LEN=*), INTENT(INOUT) ::  BULL  !a7 Bulletin to be stored
    INTEGER, INTENT(OUT)   :: KODE     !a8 Return code (for a list, see above)

    END SUBROUTINE BUFREP
  END INTERFACE
END MODULE bufrep_mod
