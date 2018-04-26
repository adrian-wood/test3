MODULE webgexp_mod
  INTERFACE
    SUBROUTINE WEBGEXP(CREP,CREP2,RPRTEND,LOOKUP)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(INOUT) :: CREP    !a01- input report text
    CHARACTER(LEN=*), INTENT(OUT)   :: CREP2   !a02- output report text (expanded)
    INTEGER,          INTENT(INOUT) :: RPRTEND !a03- end of report text in CREP
    CHARACTER(LEN=*), INTENT(IN)    :: LOOKUP  !a04- 3 character group to expand

    END SUBROUTINE WEBGEXP
  END INTERFACE
END MODULE webgexp_mod
