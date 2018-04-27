!-----------------------------------------------------------------------
SUBROUTINE DATE31 (ID, IM, IY, ICD)
!
!     DAYS SINCE 1.1.1900 FROM DAY, MONTH, YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN) :: ID, IM, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ICD
!                       LOCAL VARIABLES
  INTEGER :: IDY, INY
  INTEGER :: K,IYN, days_in_feb
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
  IYN = INY - 1900
  IF (IYN .GT. 0) THEN
     ICD = IDY + IYN*365 + (IYN-1)/4 - (IYN-1)/100 + (IYN+299)/400
  ELSE
    ICD = IDY + IYN*365 + IYN/4 - IYN/100
  ENDIF


 END SUBROUTINE DATE31
