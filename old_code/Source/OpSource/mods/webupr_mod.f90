MODULE webupr_mod
  INTERFACE
    SUBROUTINE WEBUPR(CREP,RPRTEND,WRITEHEADER,IN_TEXT, &
                      TOR_UPR)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: CREP        !a01- input report text
    INTEGER,          INTENT(IN)    :: RPRTEND     !a02- length of input report text
    LOGICAL,          INTENT(IN)    :: WRITEHEADER !a03- TRUE to output header
    CHARACTER(LEN=*), INTENT(INOUT) :: IN_TEXT     !a04- text to preceed report on page
    CHARACTER(LEN=*), INTENT(IN)    :: TOR_UPR(4)  !a05- Time Of Receipt per part

    END SUBROUTINE WEBUPR
  END INTERFACE
END MODULE webupr_mod
