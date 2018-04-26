MODULE ICEINT_MOD
  INTERFACE
    SUBROUTINE ICEINT (ARRAY, CDATA, YEAR, MONTH, DAY, HOUR, MIN, &
                       TOR, REPLEN, TTAAII, CCCC, THISID, CORFLAG)
    IMPLICIT NONE
!                                  Subroutine arguments

    REAL,         INTENT(OUT)   :: ARRAY(:) ! Expanded numeric values
    CHARACTER(*), INTENT(OUT)   :: CDATA    ! Text element data
    INTEGER,      INTENT(IN)    :: YEAR     ! Year of report
    INTEGER,      INTENT(IN)    :: MONTH    ! Month of report
    INTEGER,      INTENT(IN)    :: DAY      ! Day of report
    INTEGER,      INTENT(IN)    :: HOUR     ! Hour of report
    INTEGER,      INTENT(IN)    :: MIN      ! Minute of report
    INTEGER,      INTENT(IN)    :: TOR(:)   ! Time of receipt array
    INTEGER,      INTENT(IN)    :: REPLEN   ! Length of report
    CHARACTER(*), INTENT(IN)    :: TTAAII   ! Bulletin identifier
    CHARACTER(*), INTENT(IN)    :: CCCC     ! Originating centre
    CHARACTER(*), INTENT(IN)    :: THISID   ! Report identifier
    INTEGER,      INTENT(IN)    :: CORFLAG  ! Correction flag.

    END SUBROUTINE ICEINT
  END INTERFACE
END MODULE ICEINT_MOD
