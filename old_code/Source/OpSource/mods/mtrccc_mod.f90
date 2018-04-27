MODULE mtrccc_mod
  INTERFACE
    SUBROUTINE MTRCCC(REPORT,POINT,GRPLEN,CHARR,ELEM,ESTAR, &     !2.0
                      CLDAMT,CLDTYPE,CLDHT,VVIS)

    IMPLICIT NONE

    ! Subroutine arguments:

    CHARACTER(*), INTENT(IN)    ::  REPORT ! 1 Report being expanded
    INTEGER,      INTENT(IN)    ::  POINT ! 2 Current position in report
    INTEGER,      INTENT(IN)    ::  GRPLEN ! 3 Length of group
    CHARACTER(*), INTENT(IN)    ::  CHARR ! 4 positions of character types
                                          ! within group
    INTEGER,      INTENT(INOUT) ::  ELEM(*) ! 5 flag array
    INTEGER,      INTENT(INOUT) ::  ESTAR ! 6 Starting point in ELEM flag array
    INTEGER,      INTENT(INOUT) ::  CLDAMT ! 7 Amount of cloud
    INTEGER,      INTENT(OUT)   ::  CLDTYPE ! 8 Type of cloud
    REAL,         INTENT(OUT)   ::  CLDHT ! 9 Base height of cloud
    REAL,         INTENT(OUT)   ::  VVIS ! 10 Vertical visibility

    END SUBROUTINE MTRCCC
  END INTERFACE
END MODULE mtrccc_mod
