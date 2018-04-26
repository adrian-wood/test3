MODULE tesind_mod
  INTERFACE
    SUBROUTINE TESIND(EXPARR,IDFLG,ID,OCOR,ENTRY)

    IMPLICIT NONE

! Subroutine arguments:
    REAL,              INTENT(IN)    ::  EXPARR(0:1000) !a01
    LOGICAL,           INTENT(IN)    ::  IDFLG          !a02
    CHARACTER(LEN=9),  INTENT(IN)    ::  ID             !a03
    LOGICAL,           INTENT(IN)    ::  OCOR           !a04
    CHARACTER(LEN=23), INTENT(OUT)   ::  ENTRY          !a05 INDEX ENTRY

    END SUBROUTINE TESIND
  END INTERFACE
END MODULE tesind_mod
