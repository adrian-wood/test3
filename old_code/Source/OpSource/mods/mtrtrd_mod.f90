MODULE MTRTRD_MOD
INTERFACE
SUBROUTINE MTRTRD(Report,Pointer,ReportLength, &
                  RXpanAray,MoreTrend,Displace)

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)    ::  Report
INTEGER,           INTENT(INOUT) ::  Pointer
INTEGER,           INTENT(IN)    ::  ReportLength
REAL,              INTENT(INOUT) ::  RXpanAray(:)
LOGICAL,           INTENT(OUT)   ::  MoreTrend
INTEGER,           INTENT(IN)    ::  Displace

END SUBROUTINE MTRTRD
END INTERFACE
END MODULE MTRTRD_MOD
