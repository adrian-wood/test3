MODULE UAEDIT_mod
  INTERFACE
    SUBROUTINE UAEDIT(REPORT,TTAAII,CCCC,ICCCC,YYGGGG,CORN,IFT)

    IMPLICIT NONE

    ! Interface Arguments

    CHARACTER (LEN=*), INTENT(INOUT):: REPORT ! raw bulletin from synopt
    CHARACTER (LEN=6), INTENT(IN)   :: TTAAII ! report header identifier
    CHARACTER (LEN=4), INTENT(IN)   :: CCCC   ! collecting centre
    INTEGER, INTENT(IN)             :: ICCCC  ! collecting centre BUFR code table no.
    CHARACTER (LEN=6), INTENT(IN)   :: YYGGGG ! date/time in bull header
    CHARACTER (LEN=2), INTENT(IN)   :: CORN   ! cor number
    INTEGER, INTENT(IN)             :: IFT    ! dataset ft unit number

    END SUBROUTINE UAEDIT
  END INTERFACE
END MODULE UAEDIT_mod
