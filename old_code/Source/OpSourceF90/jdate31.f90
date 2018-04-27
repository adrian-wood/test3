SUBROUTINE JDATE31(ID,IM,IY,OD)
!
!     Returns the Julian Day Number for a Day, Month, Year
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ID,IM,IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: OD

  OD = ID - 32075											     &
           + 1461 * ( IY + 4800 - ( 14 - IM ) / 12 )/4		     &
           + 367 * ( IM - 2 + (( 14 - IM ) / 12 ) * 12 ) / 12    &
           - 3 * ( ( IY + 4900 - ( 14 - IM ) / 12 ) / 100 ) / 4


END SUBROUTINE JDATE31

