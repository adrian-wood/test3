MODULE weblnd_mod
  INTERFACE
    SUBROUTINE WEBLND(CREP,RPRTEND,WRITEHEADER,TOR,CREP2,IN_TEXT)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(INOUT) :: CREP        !a01- input report text
    INTEGER,          INTENT(INOUT) :: RPRTEND     !a02- length of input report text
    LOGICAL,          INTENT(IN)    :: WRITEHEADER !a03- TRUE to output header
    CHARACTER(LEN=*), INTENT(IN)    :: TOR         !a04- input time of receipt
    CHARACTER(LEN=*), INTENT(INOUT) :: CREP2       !a05- text workspace (for expansion)
    CHARACTER(LEN=*), INTENT(INOUT) :: IN_TEXT     !a06- text to preceed report on page

    END SUBROUTINE WEBLND
  END INTERFACE
END MODULE weblnd_mod
