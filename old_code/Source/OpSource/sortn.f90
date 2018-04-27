SUBROUTINE SORTN(A,L,N,MASK)

!-----------------------------------------------------------------------
!
! ROUTINE       : SORTN
!
! PURPOSE       : to sort a 2-dimensional array on fields defined in
!               : mask into ascending order
!
! DESCRIPTION   : this is a fortran version of various sorts based
!               : on the kdf9 program l82.  it uses a "diminishing
!               : increment" method based on frank & lazarus (comm
!               : a.c.m.,jan 1960).  for a general discussion see
!               : knuth: "art of computer programming: sorting and
!               : searching" (1973) -  shelf mark ec5b(43).
!
! CALLED BY     : anything
!
! ARGUMENTS     : (1) array(l,n): n sets of l numbers
!               : (2) number of numbers in each item
!               : (3) number of items to be sorted
!               : (4) mask with zeros for numbers to be ignored
!
! REVISION INFO :
!
!
! $Workfile: sortn.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 16/11/2010 14:07:18$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         12/11/2010 17:09:49    Rosemary Lavery update
!        
!  1    MetDB_Refresh 1.0         04/11/2010 13:28:15    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(IN)     :: L          ! Dimensions for array
INTEGER, INTENT(IN)     :: N          ! Dimensions for array
INTEGER, INTENT(INOUT)  :: A(L,N)
INTEGER, INTENT(IN)     :: MASK(L)

! Local Variables

INTEGER                 :: X
INTEGER                 :: MM
INTEGER                 :: I
INTEGER                 :: J
INTEGER                 :: K
INTEGER                 :: M
INTEGER                 :: INT

SAVE

!-----------------------------------------------------------------------
! first set the increment x.  this will be quartered or halved as the
! sort goes on, being rounded up to an odd number to give a decreasing
! sequence ending with 1.  items with subscripts x apart will be
! compared and swapped if out of order.
!-----------------------------------------------------------------------

X=N

10 CONTINUE
X=X/2
IF (X >= 8) X=X/2
IF (MOD(X,2) == 0) X=X+1

!-----------------------------------------------------------------------
! repeat the comparison involving the item just moved up.
!-----------------------------------------------------------------------

DO_COMPARE: &
DO K=1,N-X
  I=K

20 CONTINUE
  J=I+X

!-----------------------------------------------------------------------
! where the mask is set, compare the numbers in the two items in turn,
! working from left to right (i.e. the order of importance of fields
! in the mask is simply their order in the items to be sorted), and if
! necessary swap them using a single number as work area.
!-----------------------------------------------------------------------

DO_MASK: &
  DO M=1,L

IF_MASK: &
    IF (MASK(M) /= 0) THEN

IF_SWAP: &
      IF (A(M,I) > A(M,J)) THEN
        DO MM=1,L
          INT=A(MM,I)
          A(MM,I)=A(MM,J)
          A(MM,J)=INT
        END DO

!-----------------------------------------------------------------------
! if a comparison x back can be repeated, do so. in that way
! each subset of items x apart will be put into order.
!-----------------------------------------------------------------------

        IF (I > X) THEN
          I=I-X
          GO TO 20
        ELSE
          GO TO 50
        END IF
      ELSE IF (A(M,I) < A(M,J)) THEN
        GO TO 50
      END IF IF_SWAP
    END IF IF_MASK
  END DO DO_MASK

50 CONTINUE
END DO DO_COMPARE

!-----------------------------------------------------------------------
! repeat the process with a smaller increment unless the increment
! has reached 1, in which case the sort has been completed.
!-----------------------------------------------------------------------

IF (X > 1) GO TO 10

RETURN
END SUBROUTINE SORTN
