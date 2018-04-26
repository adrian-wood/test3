MODULE webesaw_mod
  INTERFACE
    SUBROUTINE WEBESAW(CREP,RPRTEND,WRITEHEADER,TOR,IN_TEXT)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: CREP        !a01- input report text
    INTEGER,          INTENT(IN)    :: RPRTEND     !a02- length of input report text
    LOGICAL,          INTENT(IN)    :: WRITEHEADER !a03- TRUE to output header
    CHARACTER(LEN=*), INTENT(IN)    :: TOR         !a04- input time of receipt
    CHARACTER(LEN=*), INTENT(INOUT) :: IN_TEXT     !a05- text to preceed report on page

    END SUBROUTINE WEBESAW
  END INTERFACE
END MODULE webesaw_mod
