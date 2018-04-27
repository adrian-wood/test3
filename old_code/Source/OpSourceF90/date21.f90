!-----------------------------------------------------------------------
SUBROUTINE DATE21 (IDY, IY, ICD)
!
!     DAYS SINCE 1.1.1900, FROM DAY OF YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: IDY, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ICD
!                       LOCAL VARIABLES
  INTEGER :: IYN

  IYN = IY - 1900
  IF (IYN .GT. 0) THEN
     ICD = IDY + IYN*365 + (IYN-1)/4 - (IYN-1)/100 + (IYN+299)/400
  ELSE
     ICD = IDY + IYN*365 + IYN/4 - IYN/100
  ENDIF

END SUBROUTINE DATE21
