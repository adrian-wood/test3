MODULE LATBOX_mod
  INTERFACE
    SUBROUTINE LATBOX(A,NOBS,NLAT,BOX)

    IMPLICIT NONE

    REAL,    INTENT(IN)   :: A(:)       ! (a1) Array of decoded BUFR data values
    INTEGER, INTENT(IN)   :: NOBS       ! (a2) Number of obs. in bulletin
    INTEGER, INTENT(IN)   :: NLAT       ! (a3) Latitude subscript for A array
    REAL,    INTENT(OUT)  :: BOX(4)     ! (a4) Limits of lat/long box (output)

    END SUBROUTINE LATBOX
  END INTERFACE
END MODULE LATBOX_mod
