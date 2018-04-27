SUBROUTINE  DATE12(ICD,IDY,IY)
!
!     DAY OF YEAR FROM DAYS SINCE 1.1.1900
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ICD
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: IDY, IY
!                       LOCAL VARIABLES
  INTEGER :: K, KD, KE, KY

  K = ICD
  KE = 0
  IF (K .GE. 366) THEN
     K = K + 1
     IF (K .GE. 73416) THEN
        K = K + 1
        IF (K .GE. 109941) THEN
           K = K + 1
           IF (K .GE. 146466) THEN
		      K = K + 1
		   ENDIF
         ENDIF
     ENDIF
  ENDIF
  IF (K .LE. -36159) THEN
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
     IY = KY + KE
     IDY = KD

END SUBROUTINE DATE12
