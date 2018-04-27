MODULE uabul_mod
  INTERFACE
    SUBROUTINE UABUL(BULL,PTR,BULEND,TTAAII,CCCC,YYGGGG,CORN,IFT)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT)    :: BULL   !bulletin report,             a1
      INTEGER,          INTENT(INOUT)    :: PTR    ! pointer,                    a2
      INTEGER,          INTENT(INOUT)    :: BULEND ! end of bulletin,            a3
      CHARACTER(LEN=6), INTENT(IN)       :: TTAAII ! bulletin id header,         a4
      CHARACTER(LEN=4), INTENT(IN)       :: CCCC   !collecting centre,           a5
      CHARACTER(LEN=6), INTENT(IN)       :: YYGGGG !date/time of bulletin,       a6
      CHARACTER(LEN=2), INTENT(IN)       :: CORN   !report correction number,    a7
      INTEGER,          INTENT(IN)       :: IFT    !FT number of storage dataset,a8
    END SUBROUTINE UABUL
  END INTERFACE
END MODULE uabul_mod
