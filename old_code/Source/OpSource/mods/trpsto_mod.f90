MODULE trpsto_mod
  INTERFACE
    SUBROUTINE TRPSTO(DATIME,ENTRY,BULL,IFT,BLKSIZ,IDENT,TOR)

    IMPLICIT NONE

    ! Subroutine arguments:
    INTEGER,          INTENT(INOUT) :: DATIME(5) !A01
    CHARACTER(LEN=*), INTENT(INOUT) :: ENTRY     !A02
    CHARACTER(LEN=*), INTENT(IN)    :: BULL      !A03
    INTEGER,          INTENT(IN)    :: IFT       !A04
    INTEGER,          INTENT(IN)    :: BLKSIZ    !A05
    CHARACTER(LEN=*), INTENT(IN)    :: IDENT     !A06
    INTEGER,          INTENT(INOUT) :: TOR(5)    !A07

    END SUBROUTINE TRPSTO
  END INTERFACE
END MODULE trpsto_mod
