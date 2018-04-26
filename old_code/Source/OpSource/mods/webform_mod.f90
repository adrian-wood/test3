MODULE webform_mod
  INTERFACE
    SUBROUTINE WEBFORM(CREP,START,RPRTEND,COL_START,COL_END, &
                       IN_TEXT)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: CREP      !a01- report to output to stdout
    INTEGER,          INTENT(IN)    :: START     !a02- start of report in string CREP
    INTEGER,          INTENT(IN)    :: RPRTEND   !a03- end of report in string CREP
    INTEGER,          INTENT(IN)    :: COL_START !a04- stdout page start column
    INTEGER,          INTENT(IN)    :: COL_END   !a05- stdout page end column
    CHARACTER(LEN=*), INTENT(IN)    :: IN_TEXT   !a06- input text to preceed report text

    END SUBROUTINE WEBFORM
  END INTERFACE
END MODULE webform_mod
