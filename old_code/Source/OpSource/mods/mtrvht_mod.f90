MODULE MTRVHT_MOD
INTERFACE
SUBROUTINE MTRVHT(Report,Pointer,RXpanAray,Displace)

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)    ::  REPORT
INTEGER,           INTENT(INOUT) ::  Pointer
REAL,              INTENT(INOUT) ::  RXpanAray(:)
INTEGER,           INTENT(IN)    ::  Displace

END SUBROUTINE MTRVHT
END INTERFACE
END MODULE MTRVHT_MOD
