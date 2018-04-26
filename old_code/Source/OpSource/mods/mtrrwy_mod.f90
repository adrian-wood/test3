MODULE MTRRWY_MOD
INTERFACE
SUBROUTINE MTRRWY(POINT,REPORT,GRPLEN,CHARR,RVRDIR,VISTEND, &
                  RVRPRL,MINQUAL,MINVIS,MAXQUAL,MAXVIS)

IMPLICIT NONE

INTEGER,           INTENT(IN)  ::  POINT        ! Point within report and group
CHARACTER (LEN=*), INTENT(IN)  ::  REPORT       ! Complete METAR report
INTEGER,           INTENT(IN)  ::  GRPLEN       ! Length of group
CHARACTER (LEN=*), INTENT(IN)  ::  CHARR        ! Character content of group.
REAL,              INTENT(OUT) ::  RVRDIR       ! Runway direction
REAL,              INTENT(OUT) ::  VISTEND      ! Visibility tendency.
REAL,              INTENT(OUT) ::  RVRPRL       ! Parallel runway identifier.
REAL,              INTENT(OUT) ::  MINQUAL      ! Minimum visibility qualifier
REAL,              INTENT(OUT) ::  MINVIS       ! Minimum visibility.
REAL,              INTENT(OUT) ::  MAXQUAL      ! Maximum visibility qualifier.
REAL,              INTENT(OUT) ::  MAXVIS       ! Maximum visibility, when variable.

END SUBROUTINE MTRRWY
END INTERFACE
END MODULE MTRRWY_MOD
