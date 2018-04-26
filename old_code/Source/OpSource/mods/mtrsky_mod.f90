MODULE MTRSKY_MOD
INTERFACE
SUBROUTINE MTRSKY(Report,Pointer,RXpanAray,MoreTrend,  &
                  ReportLength,Displace)

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)    ::  Report
INTEGER,           INTENT(INOUT) ::  Pointer
REAL,              INTENT(OUT)   ::  RXpanAray(:)
LOGICAL,           INTENT(OUT)   ::  MoreTrend
INTEGER,           INTENT(IN)    ::  ReportLength
INTEGER,           INTENT(IN)    ::  Displace

END SUBROUTINE MTRSKY
END INTERFACE
END MODULE MTRSKY_MOD
