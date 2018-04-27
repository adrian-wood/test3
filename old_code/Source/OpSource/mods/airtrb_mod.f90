MODULE airtrb_mod
  INTERFACE
    SUBROUTINE AIRTRB(REPORT,POINT,OPT_TRB)
    IMPLICIT NONE

    ! Arguments

    CHARACTER(LEN=*),INTENT(IN) :: REPORT  !character form of report
    INTEGER,INTENT(INOUT)       :: POINT   !Position within report pointer
    REAL,INTENT(OUT)            :: OPT_TRB !Decoded Turb. Value
    END SUBROUTINE AIRTRB
  END INTERFACE
END MODULE airtrb_mod
