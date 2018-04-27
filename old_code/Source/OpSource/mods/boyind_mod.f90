MODULE boyind_mod
  INTERFACE
    SUBROUTINE BOYIND(ARRAY,OCOR,DRGFLG,ENTRY)

    IMPLICIT NONE

! Subroutine arguments:

    REAL,              INTENT(IN)    ::   ARRAY(0:600) !A01
    LOGICAL,           INTENT(IN)    ::   OCOR         !A02
    LOGICAL,           INTENT(IN)    ::   DRGFLG       !A03
    CHARACTER(LEN=23), INTENT(OUT)   ::   ENTRY        !A04

    END SUBROUTINE BOYIND
  END INTERFACE
END MODULE boyind_mod
