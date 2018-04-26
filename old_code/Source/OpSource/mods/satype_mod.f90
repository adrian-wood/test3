MODULE satype_mod
  INTERFACE
    SUBROUTINE SATYPE (TEXT, LIST1, LIST2, NTYPES, NUMTYP)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: TEXT           ! (Input) Text to be searched for
    INTEGER, INTENT(IN)            :: NTYPES         ! (Input) Number of search ranges
    CHARACTER (LEN=*), INTENT(IN)  :: LIST1(NTYPES)  ! Array of beginnings of text ranges
    CHARACTER (LEN=*), INTENT(IN)  :: LIST2(NTYPES)  ! Array of ends of text ranges
    INTEGER, INTENT(OUT)           :: NUMTYP         ! (Output) Number of range containing TEXT

    END SUBROUTINE SATYPE
  END INTERFACE
END MODULE satype_mod
