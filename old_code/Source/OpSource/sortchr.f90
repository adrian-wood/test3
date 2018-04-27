SUBROUTINE SORTCHR(C,L,N,MASK)

!-----------------------------------------------------------------------
!
! ROUTINE       : SORTCHR
!
! PURPOSE       : To sort an array of character strings on fields
!               : defined in a mask, lettered in order of importance
!               : & uc/lc to indicate ascending or descending order.
!
! DESCRIPTION   : This is a Fortran version of various sorts based
!               : on the KDF9 program L82.  It uses a "diminishing
!               : increment" method based on Frank & Lazarus (Comm
!               : A.C.M., Jan 1960).  For a general discussion see
!               : Knuth: "Art of Computer Programming: Sorting and
!               : Searching" (1973) -  shelf mark EC5b(43).
!
!                 SORTCHR copies the structure of SORTR for numbers.
!
! CALLED BY     : SYNRET
!
! CALLS         : SORTN
!
! ARGUMENTS     : (1) N character strings of length L
!               : (2) length of each string (unnecessary argument!)
!               : (3) number of items to be sorted
!               : (4) mask with blanks for characters to be ignored
!                     & letters for fields to sort on.  Position in
!                     alphabet indicates order of importance (A most
!                     important, B less so, and so on - though X,Y,Z
!                     has the same effect as A,B,C.  Use upper case
!                     for ascending order, lower case for descending.
!
! REVISION INFO :
!
!
! $Workfile: sortchr.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 09/02/2011 17:14:06$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         09/02/2011 17:14:06    Sheila Needham  Put
!       explcit lengths on character assignments
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

USE SORTN_MOD

IMPLICIT NONE

! Interface Arguments

INTEGER,           INTENT(IN)      ::  L
INTEGER,           INTENT(IN)      ::  N
CHARACTER (LEN=*), INTENT(INOUT)   ::  C(N)
CHARACTER (LEN=*), INTENT(IN)      ::  MASK

! Local Variables

CHARACTER (LEN=52)   ::  ABC
CHARACTER (LEN=500)  ::  CSWAP
INTEGER              ::  I
INTEGER              ::  J
INTEGER              ::  K
INTEGER              ::  M
INTEGER              ::  X
INTEGER              ::  NF
INTEGER              ::  INLET                        ! number for letter in mask
INTEGER              ::  NFIELDS                      ! number of letters in mask
INTEGER              ::  FIELD(2,100)                 ! 100 is arbitrary dimension
INTEGER              ::  FIELD_MASK(2) = (/ 1, 1 /)   ! init to sort FIELD on importance AND position
LOGICAL              ::  ASC                          ! set if upper case in mask

SAVE

DATA ABC/'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'/


! Give up if input strings too long for fixed-length swap string

IF (L > LEN(CSWAP)) THEN
  PRINT *,' SORTCHR: Strings too long for sort'
  RETURN
END IF

!-----------------------------------------------------------------------
! List the fields in order of importance, keeping the position of each
! mask letter & a corresponding number, then sorting on those numbers
! and the positions: this allows the same character to be used for all
! characters in a field.  The number represents position in the alpha-
! bet regardless of case - & regardless of whether ASCII or EBCDIC...
!-----------------------------------------------------------------------

NFIELDS=0

DO_MASK: &
DO I=1,L
  IF (MASK(I:I) /= ' ' .AND. NFIELDS < 100) THEN
    NFIELDS=NFIELDS+1
    FIELD(2,NFIELDS)=I

    J=0
    INLET=0
    DO WHILE (INLET == 0 .AND. J < LEN(ABC))
      J=J+1
      IF (ABC(J:J) == MASK(I:I)) INLET=J
    END DO

    IF (INLET > 26) INLET=INLET-26    ! ignore case here
    FIELD(1,NFIELDS)=INLET
  END IF
END DO DO_MASK

CALL SORTN(FIELD,2,NFIELDS,FIELD_MASK)

!-----------------------------------------------------------------------
! First set the increment X.  This will be quartered or halved as the
! sort goes on, being rounded up to an odd number to give a decreasing
! sequence ending with 1.  Items with subscripts X apart will be
! compared and swapped if out of order.
!-----------------------------------------------------------------------

X=N
   10 X=X/2
IF (X >= 8) X=X/2
IF (MOD(X,2) == 0) X=X+1

!-----------------------------------------------------------------------
! Do N minus X comparisons for each increment.  If a swap is done,
! repeat the comparison involving the item just moved up.
!-----------------------------------------------------------------------

DO_COMPARE: &
DO K=1,N-X
  I=K
   20   J=I+X

!-----------------------------------------------------------------------
! Where the mask is set, compare the corresponding characters
! in the two items, and swap the two items if necessary.
! Loop round the fields in order of importance, comparing
! in a way that depends on whether the sort for that field is in
! ascending or descending order.  (I.e. read the condition below as
! "if ascending order & I-th greater than J-th or descending order
!  & I-th less than J-th, then swap..."
!-----------------------------------------------------------------------

DO_SWAP: &
  DO NF=1,NFIELDS
    M=FIELD(2,NF)
    ASC=MASK(M:M) >= 'A' .AND. MASK(M:M) <= 'Z'

IF_SWAP: &
    IF ((ASC      .AND. C(I)(M:M) > C(J)(M:M)) .OR.   &
        (.NOT.ASC .AND. C(I)(M:M) < C(J)(M:M))) THEN
      CSWAP(1:L)=C(I)(1:L)
      C(I)(1:L)=C(J)(1:L)
      C(J)(1:L)=CSWAP(1:L)

!-----------------------------------------------------------------------
! If a comparison X back can be repeated, do so. In that way
! each subset of items X apart will be put into order.
!-----------------------------------------------------------------------

      IF (I > X) THEN
        I=I-X
        GO TO 20
      ELSE
        GO TO 50
      END IF
    ELSE
      IF (ASC      .AND. C(I)(M:M) < C(J)(M:M)) GO TO 50
      IF (.NOT.ASC .AND. C(I)(M:M) > C(J)(M:M)) GO TO 50
    END IF IF_SWAP
  END DO DO_SWAP
   50   CONTINUE
END DO DO_COMPARE

!-----------------------------------------------------------------------
! Repeat the process with a smaller increment unless the increment
! has reached 1, in which case the sort has been completed.
!-----------------------------------------------------------------------

IF (X > 1) GO TO 10

RETURN
END SUBROUTINE SORTCHR
