SUBROUTINE SORTCH(C,L,N,MASK)

!-----------------------------------------------------------------------
!
! PROGRAM       : SORTCH
!
! PURPOSE       : TO SORT A CHARACTER ARRAY ON FIELDS DEFINED IN THE
!                 MASK INTO ASCENDING ORDER
!
! DESCRIPTION   : THIS IS A FORTRAN VERSION OF VARIOUS SORTS BASED
!                 ON THE KDF9 PROGRAM L82.  IT USES A "DIMINISHING
!                 INCREMENT" METHOD BASED ON FRANK & LAZARUS (COMM
!                 A.C.M.,JAN 1960).  FOR A GENERAL DISCUSSION SEE
!                 KNUTH: "ART OF COMPUTER PROGRAMMING: SORTING AND
!                 SEARCHING" (1973) -  SHELF MARK EC5B(43).
!
! CALLED BY     : VARIOUS
!
! PARAMETERS    : (1) CHAR*L(N): N STRINGS OF LENGTH L
!                 (2) LENGTH OF EACH STRING
!                 (3) NUMBER OF ITEMS TO BE SORTED
!                 (4) MASK WITH BLANKS FOR CHARACTERS TO BE IGNORED
!
! REVISION INFO :
!
!
! $Workfile: sortch.f90$ $Folder: OpSource$
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

INTEGER,           INTENT(IN)      ::  L
INTEGER,           INTENT(IN)      ::  N
CHARACTER (LEN=*), INTENT(INOUT)   ::  C(N)
CHARACTER (LEN=*), INTENT(IN)      ::  MASK

! Local Variables

CHARACTER (LEN=500)   ::  CSWAP
INTEGER               ::  X
INTEGER               ::  M
INTEGER               ::  K
INTEGER               ::  I
INTEGER               ::  J
INTEGER               ::  MINVAL
INTEGER               ::  MAXVAL


! GIVE UP IF INPUT STRINGS TOO LONG FOR FIXED-LENGTH SWAP STRING

IF (L > 500) THEN
  PRINT *,' STRINGS TOO LONG FOR SORT'
  RETURN
END IF

! THE MASK MAY START AND/OR END WITH BLANKS.  IN COMPARING STRINGS IT'S
! ENOUGH TO LOOP FROM FIRST NON-BLANK TO LAST RATHER THAN FROM 1 TO L.

MINVAL=1
DO WHILE (MASK(MINVAL:MINVAL) == ' ')
  MINVAL=MINVAL+1
END DO

MAXVAL=L
DO WHILE (MASK(MAXVAL:MAXVAL) == ' ')
  MAXVAL=MAXVAL-1
END DO

! FIRST SET THE INCREMENT X.  THIS WILL BE QUARTERED OR HALVED AS THE
! SORT GOES ON, BEING ROUNDED UP TO AN ODD NUMBER TO GIVE A DECREASING
! SEQUENCE ENDING WITH 1.  STRINGS WITH SUBSCRIPTS X APART WILL BE
! COMPARED AND SWAPPED IF OUT OF ORDER.

X=N

10 CONTINUE

X=X/2
IF (X >= 8) X=X/2
IF (MOD(X,2) == 0) X=X+1

! DO N MINUS X COMPARISONS FOR EACH INCREMENT.  IF A SWAP IS DONE,
! REPEAT THE COMPARISON INVOLVING THE ITEM JUST MOVED UP.

DO_COMPARE: &
DO K=1,N-X
  I=K

20 CONTINUE
  J=I+X

! WHERE THE MASK IS SET, COMPARE CHARACTERS IN THE TWO ITEMS IN TURN,
! WORKING FROM LEFT TO RIGHT (I.E. THE ORDER OF IMPORTANCE OF FIELDS
! IN THE MASK IS SIMPLY THEIR ORDER IN THE ITEMS TO BE SORTED), AND IF
! NECESSARY SWAP THE STRINGS.

DO_SWAP: &
  DO M=MINVAL,MAXVAL

IF_MASK: &
    IF (MASK(M:M) /= ' ') THEN

IF_SWAP: &
      IF (C(I)(M:M) > C(J)(M:M)) THEN
        CSWAP=C(I)
        C(I)=C(J)
        C(J)=CSWAP

! IF A COMPARISON X BACK CAN BE REPEATED, DO SO.  IN THAT WAY
! EACH SUBSET OF ITEMS X APART WILL BE PUT INTO ORDER.

        IF (I > X) THEN
          I=I-X
          GO TO 20
        ELSE
          GO TO 50
        END IF
      ELSE IF (C(I)(M:M) < C(J)(M:M)) THEN
        GO TO 50
      END IF IF_SWAP
    END IF IF_MASK
30  CONTINUE
  END DO DO_SWAP

50 CONTINUE
END DO DO_COMPARE

! REPEAT THE PROCESS WITH A SMALLER INCREMENT UNLESS THE INCREMENT
! HAS REACHED 1, IN WHICH CASE THE SORT HAS BEEN COMPLETED.

IF (X > 1) GO TO 10

RETURN
END SUBROUTINE SORTCH
