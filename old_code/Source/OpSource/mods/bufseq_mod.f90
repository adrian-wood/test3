MODULE bufseq_mod
  INTERFACE
    SUBROUTINE BUFSEQ (SEQDES, BULL, DIFF, LENBUL)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN)    :: SEQDES ! (Input) Descriptor sequence to insert
    CHARACTER(LEN=*), INTENT(INOUT) :: BULL   ! (Input) BUFR bulletin
    LOGICAL,          INTENT(OUT)   :: DIFF   ! (Output) Result of descriptor sequence comparison
    INTEGER,          INTENT(OUT)   :: LENBUL ! Length of bulletin

    END SUBROUTINE BUFSEQ
  END INTERFACE
END MODULE bufseq_mod
