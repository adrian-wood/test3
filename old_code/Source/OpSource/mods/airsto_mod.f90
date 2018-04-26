MODULE AIRSTO_mod
  INTERFACE
    SUBROUTINE AIRSTO(DATIME,ENTRY,BULL,IFT,BLKSIZ,IDENT,TOR)

    IMPLICIT NONE

    INTEGER, INTENT(INOUT)            :: DATIME(5)      ! (A1)
    CHARACTER (LEN=*), INTENT(INOUT)  :: ENTRY          ! (A2)
    CHARACTER (LEN=*), INTENT(IN)     :: BULL           ! (A3)
    INTEGER, INTENT(IN)               :: IFT            ! (A4)
    INTEGER, INTENT(IN)               :: BLKSIZ         ! (A5)
    CHARACTER (LEN=*), INTENT(INOUT)  :: IDENT          ! (A6)
    INTEGER, INTENT(INOUT)            :: TOR(5)         ! (A7)

    END SUBROUTINE AIRSTO
  END INTERFACE
END MODULE AIRSTO_mod
