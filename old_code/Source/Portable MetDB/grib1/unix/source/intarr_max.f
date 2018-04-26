!+    Return the maximum value in an array of integers
! $Header: intarr_max.f, 1, 27/02/2007 16:11:06, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Function interface:

      FUNCTION Intarr_Max(array, len_array)

      IMPLICIT NONE

!     Description:
!       Returns the maximum value in an array of integers. Provides a
!       subset of the functionality of the Fortran 90 intrinsic
!       'Maxval'
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
      INTEGER    Intarr_Max        ! Function declaration
      
!     Scalar arguments with INTENT(IN):

      INTEGER    len_array         ! Length of array

!     Array arguments with INTENT(IN):

      INTEGER    array(len_array) ! Array

!     Local Scalars:

      INTEGER    indx             ! Index into array
      INTEGER    max_so_far       ! Largest value found so far

!-    End of header

      max_so_far = array(1)

      DO indx = 1, len_array
        max_so_far = Max(max_so_far, array(indx))
      END DO

      Intarr_Max = max_so_far

      END ! FUNCTION Intarr_Max
