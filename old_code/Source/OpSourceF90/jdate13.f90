!-----------------------------------------------------------------------
SUBROUTINE JDATE13(ID,OD,OM,OY)
!
!     Returns the Day, Month, Year from a Julian Day Number
!

  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ID
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: OD,OM,OY
!                       LOCAL VARIABLES
  INTEGER :: J,I,L,N

  L = ID + 68569
  N = ( 4 * L ) / 146097
  L = L - ( 146097 * N + 3 ) / 4
  I = ( 4000 * ( L + 1 ) ) / 1461001
  L = L - ( 1461 * I ) / 4 + 31
  J = ( 80 * L ) / 2447
  OD = L - ( 2447 * J ) / 80
  L = J / 11
  OM = J + 2 - ( 12 * L )
  OY = 100 * ( N - 49 ) + I + L


END SUBROUTINE JDATE13
