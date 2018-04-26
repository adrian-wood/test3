MODULE webform2_mod
  INTERFACE
    SUBROUTINE WEBFORM2(STRING,VALUE,FORM,UNITS)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: STRING !a01- description of value
    REAL,             INTENT(IN)    :: VALUE  !a02- value to format/output
    CHARACTER(LEN=*), INTENT(IN)    :: FORM   !a03- format string of output
    CHARACTER(LEN=*), INTENT(IN)    :: UNITS  !a04- units of value

    END SUBROUTINE WEBFORM2
  END INTERFACE
END MODULE webform2_mod
