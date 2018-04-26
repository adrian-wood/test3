!+    Return the minimum value in an array of integers
! $Header: intarr_min.f, 1, 27/02/2007 16:11:06, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Function interface:

      FUNCTION Intarr_Min(array, len_array)

      IMPLICIT NONE

!     Description:
!       Returns the minimum value in an array of integers. Provides a
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
!     Function declaration
      INTEGER    Intarr_Min 
!
!     Scalar arguments with INTENT(IN):

      INTEGER    len_array         ! Length of array

!     Array arguments with INTENT(IN):

      INTEGER    array(len_array) ! Array

!     Local Scalars:

      INTEGER    indx             ! Index into array
      INTEGER    min_so_far       ! Smallest value found so far

!-    End of header

      min_so_far = array(1)

      DO indx = 1, len_array
        min_so_far = Min(min_so_far, array(indx))
      END DO

      Intarr_Min = min_so_far
      
      RETURN
      END ! FUNCTION Intarr_Min
