MODULE MTRWND_MOD
INTERFACE
SUBROUTINE MTRWND(Report,Pointer,WindGroupUnits,RXpanAray,  &
                  RawSpeed2MS,MoreTrend,ReportLength,       &
                  UnitLength,Displace)

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)    ::  Report
INTEGER,           INTENT(INOUT) ::  Pointer
INTEGER,           INTENT(IN)    ::  WindGroupUnits
REAL,              INTENT(INOUT) ::  RXpanAray(:)
REAL,              INTENT(IN)    ::  RawSpeed2MS
LOGICAL,           INTENT(INOUT) ::  MoreTrend
INTEGER,           INTENT(IN)    ::  ReportLength
INTEGER,           INTENT(IN)    ::  UnitLength
INTEGER,           INTENT(IN)    ::  Displace

END SUBROUTINE MTRWND
END INTERFACE
END MODULE MTRWND_MOD
