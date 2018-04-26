MODULE aircod_mod
  INTERFACE
    SUBROUTINE AIRCOD(AIR_ARAY1,AIR_ARAY2,ARYFLG,SIGN,BEAC_NAME,   &
                      DESCR,NDESC,REPLEN,MESAGE,LENGTH,DATIME)

    IMPLICIT NONE

    ! Arguments

    REAL,INTENT(INOUT)    :: AIR_ARAY1(18)   ! Elements array
    REAL,INTENT(INOUT)    :: AIR_ARAY2(18)   ! Elements array with Midpoint
    INTEGER,INTENT(IN)    :: ARYFLG          ! number of reports
    CHARACTER(LEN=8),INTENT(IN)    :: SIGN   !- Callsign
    CHARACTER(LEN=*),INTENT(IN)    :: BEAC_NAME
    INTEGER,INTENT(INOUT) :: DESCR(22)       !- BUFR descriptors array.
    INTEGER,INTENT(INOUT) :: NDESC           !- no. of descriptors
    INTEGER,INTENT(IN)    :: REPLEN          !- report length
    CHARACTER(LEN=10240),INTENT(INOUT) :: MESAGE     !- final BUFR message.
    INTEGER,INTENT(INOUT) :: LENGTH          !- length of BUFR mes
    INTEGER,INTENT(INOUT) :: DATIME(5)       !- date/time array.
    END SUBROUTINE AIRCOD
  END INTERFACE
END MODULE aircod_mod
