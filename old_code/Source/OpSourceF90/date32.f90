SUBROUTINE  DATE32(ID,IM,IY,IDY,INY)
!
!     DAY OF YEAR FROM DAY, MONTH, YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ID, IM, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: IDY, INY
!                       LOCAL VARIABLES
  INTEGER :: K, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS



  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF

! array ranges from 0 to November (not from Jan to Dec as in
! previous version)                                                 !1.2

  MONTHS = (/0,31,days_in_feb,31,30,31,30,31,31,30,31,30/)          !1.2

  K = SUM(MONTHS(1:IM))                                             !1.2

  IDY = K + ID
  INY = IY

END SUBROUTINE DATE32
