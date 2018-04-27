MODULE MTRWTR_MOD
INTERFACE
SUBROUTINE MTRWTR(Report,Pointer,RXpanAray,MoreTrend,  &
                  ReportLength,Displace)

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)    ::  REPORT
INTEGER,           INTENT(INOUT) ::  Pointer
REAL,              INTENT(INOUT) ::  RXpanAray(:)
LOGICAL,           INTENT(INOUT) ::  MoreTrend
INTEGER,           INTENT(IN)    ::  ReportLength
INTEGER,           INTENT(IN)    ::  Displace

END SUBROUTINE MTRWTR
END INTERFACE
END MODULE MTRWTR_MOD
