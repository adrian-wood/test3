MODULE HRS2DT_MOD
  INTERFACE
    SUBROUTINE HRS2DT (IY, IM, ID, IH, ICNTH)

    IMPLICIT NONE
!                           Subroutine arguments

    INTEGER, INTENT(OUT) :: IY      ! Year
    INTEGER, INTENT(OUT) :: IM      ! Month
    INTEGER, INTENT(OUT) :: ID      ! Day of month
    INTEGER, INTENT(OUT) :: IH      ! Hour
    INTEGER, INTENT(IN)  :: ICNTH   ! Century day

    END SUBROUTINE HRS2DT
  END INTERFACE
END MODULE HRS2DT_MOD
