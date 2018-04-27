!+    Returns the least integer not less than log to the base 2 of x
! $Header: ceil_log2.f, 1, 27/02/2007 16:11:05, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Function interface:

      INTEGER FUNCTION Ceil_Log2(x)

      IMPLICIT NONE

!     Description:
!       Returns the least integer not less than log to the base 2 of x.
!       x must be greater than zero.
!
!     Current Code Owner: Andrew Edmunds OS(US)
!
!     History:
!     Version    Date     Comment
!   
!        1.0     6/97     Original code. (A. Edmunds)
!
!     Code Description:
!       FORTRAN 77 with extensions recommended in the Met. Office
!       F77 Standard.
!
!     Scalar arguments with INTENT(IN):

      INTEGER    x                  ! x > 0

!     Local Scalars:

      INTEGER    x_shifted
      INTEGER    result

!-    End of header

      x_shifted = x
      result = 0
      
      DO WHILE ( x_shifted .NE. 0 )
        x_shifted = x_shifted / 2
        result = result + 1
      END DO
      
      Ceil_Log2 = result
      
      END ! FUNCTION Ceil_Log2
