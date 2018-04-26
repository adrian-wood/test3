MODULE STBIND_mod
  INTERFACE
    SUBROUTINE STBIND(NOBS,ARRAY,ID,DATETIME,ENTRY)

    IMPLICIT NONE

    INTEGER, INTENT(IN)                :: NOBS        ! Number of observations in BUFR message
    REAL, INTENT(IN)                   :: ARRAY(:)    ! Array of data values for BUFR message
    CHARACTER (LEN=3), INTENT(IN)      :: ID          ! Satellite identifier
    INTEGER, INTENT(OUT)               :: DATETIME(5) ! Data date & time (yr, mon, day, hr, min)
    CHARACTER (LEN=23), INTENT(INOUT)  :: ENTRY       ! Index entry

    END SUBROUTINE STBIND
  END INTERFACE
END MODULE STBIND_mod
