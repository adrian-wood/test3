MODULE mtrddf_mod
  INTERFACE
    SUBROUTINE MTRDDF(REPORT,POINT,GRPLEN,CHARR,DDD,FFF,GUST)     !2.0

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)  ::  REPORT ! Report being expanded
    INTEGER,      INTENT(IN)  ::  POINT ! Current position within report
    INTEGER,      INTENT(IN)  ::  GRPLEN ! Length of group
    CHARACTER(*), INTENT(IN)  ::  CHARR ! Group content
    REAL,         INTENT(OUT) ::  DDD ! Wind direction
    REAL,         INTENT(OUT) ::  FFF ! Wind force (converted)
    REAL,         INTENT(OUT) ::  GUST ! Gust force (converted)

    END SUBROUTINE MTRDDF
  END INTERFACE
END MODULE mtrddf_mod
