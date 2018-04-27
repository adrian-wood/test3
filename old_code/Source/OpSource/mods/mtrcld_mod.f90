MODULE mtrcld_mod
  INTERFACE
    SUBROUTINE MTRCLD(Report,Pointer,RXpanAray,MoreTrend, &
                      ReportLength,Displace)                     !2.0

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN) ::  Report
    INTEGER,      INTENT(INOUT) ::  Pointer
    REAL,         INTENT(INOUT) ::  RXpanAray(*)
    LOGICAL,      INTENT(INOUT) ::  MoreTrend
    INTEGER,      INTENT(IN) ::  ReportLength
    INTEGER,      INTENT(IN) ::  Displace

    END SUBROUTINE MTRCLD
  END INTERFACE
END MODULE mtrcld_mod
