MODULE airmid_mod
  INTERFACE
    SUBROUTINE AIRMID(TIMEDD,TIMEHH,TIMEMM,MID_YY,MID_MTH,MID_DD,  &
     MID_HH,MID_MM,LAT,LONG,MID_LAT,MID_LONG,SIGN,MATCH,NFT)
    IMPLICIT NONE

    ! Arguments

    REAL,INTENT(IN)    :: TIMEDD       !present day
    REAL,INTENT(IN)    :: TIMEHH       !present hour
    REAL,INTENT(IN)    :: TIMEMM       !present minutes
    REAL,INTENT(INOUT) :: MID_YY
    REAL,INTENT(INOUT) :: MID_MTH
    REAL,INTENT(INOUT) :: MID_DD       !Mid point day
    REAL,INTENT(INOUT) :: MID_HH       !Mid point hour
    REAL,INTENT(INOUT) :: MID_MM       !Mid point minutes
    REAL,INTENT(IN)    :: LAT          !present Latitude
    REAL,INTENT(IN)    :: LONG         !present Longitude
    REAL,INTENT(INOUT) :: MID_LAT      !Mid point Latitude
    REAL,INTENT(INOUT) :: MID_LONG     !Mid point Longitude
    CHARACTER(LEN=8),INTENT(INOUT) :: SIGN    !Aircraft callsign
    INTEGER,INTENT(OUT) ::  MATCH
    INTEGER,INTENT(IN)    ::  NFT
    END SUBROUTINE AIRMID
  END INTERFACE
END MODULE airmid_mod
