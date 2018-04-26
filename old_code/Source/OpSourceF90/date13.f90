!-----------------------------------------------------------------------
SUBROUTINE DATE13 (ICD, ID, IM, INY)
!
!     DAY, MONTH, YEAR FROM DAYS SINCE 1.1.1900
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN) :: ICD
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ID, IM, INY
!                       LOCAL VARIABLES
  INTEGER :: IDY, IY
  INTEGER :: K,KD,KE,KY,I,K1X, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS

  K = ICD
  KE = 0
  IF (K .GE. 366) THEN				!these allow for the non-leap years 1900,
     K = K + 1
     IF (K .GE. 73416) THEN         !2100,
        K = K + 1
        IF (K .GE. 109941) THEN     !2200,
           K = K + 1
           IF (K .GE. 146466) THEN  !2300 ...
       		  K = K + 1
           ENDIF
        ENDIF
     ENDIF
  ENDIF
  IF (K .LE. -36159) THEN			! and 1800 respectively
     K = K - 1
  ENDIF 	

  KY = K/1461*4
  KD = K - K/1461*1461
  IF (KD .LT. 0) THEN
     KD = KD + 1461
     KY = KY - 4
  ENDIF
  KY = KY + 1900
  IF (KD .GT. 366) THEN
     KD = KD - 1
     KE = KD/365
     KD = KD - KD/365*365
  ENDIF
  IF (KD .EQ. 0) THEN
     KE = KE - 1
     KD = 365
  ENDIF
  INY = KY + KE
  IDY = KD
  IY = INY

  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF

  MONTHS = (/31,days_in_feb,31,30,31,30,31,31,30,31,30,31/)
	
  K1X = IDY

  DO I=1,12
    K1X = K1X - MONTHS(I)
    IF (K1X .GT. 0) THEN
	   CYCLE
	ELSE
      ID = K1X + MONTHS(I)
      IM = I
      INY = IY

    ENDIF
    EXIT
  END DO	

END SUBROUTINE DATE13
