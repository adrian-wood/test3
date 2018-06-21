!+    Return the minimum value in an array of real numbers
! $Header: realarr_min.f, 1, 27/02/2007 16:11:09, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Function interface:

      REAL FUNCTION Realarr_Min(array, len_array)

      IMPLICIT NONE

!     Description:
!       Returns the minimum value in an array of reals. Provides a
!       subset of the functionality of the Fortran 90 intrinsic
!       'Minval'
!
!     Current Code Owner: Andrew Edmunds OS(US)
!
!     History:
!     Version  Date      Comment
!
!       1.0    6/97    Original code. (A. Edmunds)
!
!     Code Description:
!       FORTRAN 77 with extensions recommended in the Met. Office
!       F77 Standard.
!
!     Scalar arguments with INTENT(IN):

      INTEGER    len_array         ! Length of array

!     Array arguments with INTENT(IN):

      REAL       array(len_array) ! Array

!     Local Scalars:

      INTEGER    indx             ! Index into array
      REAL       min_so_far       ! Smallest value found so far

!-    End of header

      min_so_far = array(1)

      DO indx = 1, len_array
        min_so_far = Min(min_so_far, array(indx))
      END DO

      Realarr_Min = min_so_far

      END ! FUNCTION Realarr_Min