MODULE tafrep_mod
  INTERFACE
    SUBROUTINE TAFREP(DATIME,ENTRY,BULL,IFT,BLKSIZ,IDENT)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(INOUT) :: DATIME(5) !a1
    CHARACTER(LEN=*), INTENT(INOUT) :: ENTRY     !a2
    CHARACTER(LEN=*), INTENT(IN)    :: BULL      !a3
    INTEGER,          INTENT(IN)    :: IFT       !a4
    INTEGER,          INTENT(IN)    :: BLKSIZ    !a5
    CHARACTER(LEN=*), INTENT(INOUT) :: IDENT     !a6

    END SUBROUTINE TAFREP
  END INTERFACE
END MODULE tafrep_mod
