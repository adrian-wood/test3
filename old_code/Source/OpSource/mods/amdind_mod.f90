MODULE amdind_mod
  INTERFACE
    SUBROUTINE AMDIND(ARRAY,ID,DATIME,ENTRY,TTAAII,CCCC,IDENT)

    IMPLICIT NONE

! Subroutine arguments:

    REAL,              INTENT(IN)    :: ARRAY(:)   !a1
    CHARACTER(LEN=*),  INTENT(IN)    :: ID         !a2
    INTEGER,           INTENT(OUT)   :: DATIME(5)  !a3
    CHARACTER(LEN=23), INTENT(OUT)   :: ENTRY      !a4
    CHARACTER(LEN=6),  INTENT(IN)    :: TTAAii     !a5
    CHARACTER(LEN=4),  INTENT(IN)    :: CCCC       !a6
    CHARACTER(LEN=9),  INTENT(OUT)   :: IDENT      !a7

    END SUBROUTINE AMDIND
  END INTERFACE
END MODULE amdind_mod
