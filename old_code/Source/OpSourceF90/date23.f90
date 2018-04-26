SUBROUTINE DATE23 (IDY, IY, ID, IM, INY)
!
!     DAY, MONTH, YEAR FROM DAY OF YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN) ::  IDY, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ID, IM, INY
!                       LOCAL VARIABLES
  INTEGER :: I, K, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS
!
  CALL ISALEAP(IY)
  IF (ISALEAP) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF


  MONTHS = (/31,days_in_feb,31,30,31,30,31,31,30,31,30,31/)
  K = IDY

  DO I=1,12
    K = K - MONTHS(I)
    IF (K .GT. 0) THEN
       CYCLE
    ELSE
      ID = K + MONTHS(I)
      IM = I
      INY = IY
    ENDIF
    EXIT
  END DO

END SUBROUTINE DATE23
