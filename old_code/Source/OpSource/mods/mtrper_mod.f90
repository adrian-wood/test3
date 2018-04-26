MODULE MTRPER_MOD
INTERFACE
SUBROUTINE MTRPER(Report,Pointer,RXpanAray,MoreTrend, &
                  ReportLength,Displace)

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)    ::  Report
INTEGER,           INTENT(INOUT) ::  Pointer
REAL,              INTENT(INOUT) ::  RXpanAray(:)
LOGICAL,           INTENT(OUT)   ::  MoreTrend
INTEGER,           INTENT(IN)    ::  ReportLength
INTEGER,           INTENT(IN)    ::  Displace

END SUBROUTINE MTRPER
END INTERFACE
END MODULE MTRPER_MOD
