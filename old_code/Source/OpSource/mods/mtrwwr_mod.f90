MODULE MTRWWR_MOD
INTERFACE
SUBROUTINE MTRWWR(POINT,REPORT,GRPLEN,ELEM,ESTART,WDWD,REWW,ERROR)

IMPLICIT NONE

INTEGER,           INTENT(INOUT) ::  POINT    ! Start of group being checked
CHARACTER (LEN=*), INTENT(IN)    ::  REPORT   ! Report being expanded.
INTEGER,           INTENT(INOUT) ::  GRPLEN   ! Length of group (2-9 characters valid)
INTEGER,           INTENT(INOUT) ::  ELEM(:)  ! Counts for current & recent weather
INTEGER,           INTENT(IN)    ::  ESTART   ! Displacement within element array.
REAL,              INTENT(INOUT) ::  WDWD(:)  ! Expanded weather group.
REAL,              INTENT(INOUT) ::  REWW(:)  ! Expanded recent weather.
LOGICAL,           INTENT(OUT)   ::  ERROR    ! Group cannot be successfully decoded.

END SUBROUTINE MTRWWR
END INTERFACE
END MODULE MTRWWR_MOD
