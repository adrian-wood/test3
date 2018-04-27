SUBROUTINE ISRCH (NUMBER, LIST, NSIZE, NPOS)

!-----------------------------------------------------------------------
!
! PROGRAM     : ISRCH
!
! PURPOSE     : TO FIND A GIVEN INTEGER IN AN ARRAY OF INTEGERS
!               ALREADY SORTED INTO INCREASING NUMERICAL ORDER.
!
! DESCRIPTION : USING A BINARY SEARCH PROCEDURE, THE LOCATION OF THE
!               GIVEN INTEGER IN THE LIST IS FOUND AND RETURNED TO
!               THE CALLING PROGRAM.  IF THE INTEGER IS NOT IN THE
!               LIST, A ZERO OR NEGATIVE VALUE IS RETURNED.
!
! USAGE       : CALL ISRCH (NUMBER, LIST, NSIZE, NPOS)
!
! ARGUMENTS   : NUMBER  (I)  INTEGER TO BE LOCATED IN LIST
!               LIST    (I)  ARRAY OF INTEGERS TO BE SEARCHED
!               NSIZE   (I)  NUMBER OF INTEGERS IN LIST
!               NPOS    (O)  LOCATION OF NUMBER IN LIST
!
!               IF THE INTEGER IS NOT IN THE LIST AND LIES BETWEEN
!               THE NTH AND (N+1)TH ELEMENTS, THE VALUE RETURNED IN
!               "NPOS" IS -N ("N" IN RANGE 0 TO NSIZE).
!
! CALLED BY   : STAPOS (OR OTHER USER'S PROGRAM)
!
! REVISION INFO :
!
! $Workfile: isrch.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 14/12/2010 10:51:40$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         14/12/2010 10:51:40    Alison Weir
!       Initial version MDBSTOR batch 5
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements: none

IMPLICIT NONE

! Subroutine arguments:

INTEGER,  INTENT(IN)  :: NUMBER     ! A01 Target integer
INTEGER,  INTENT(IN)  :: NSIZE      ! A03 Number of integers in list
INTEGER,  INTENT(IN)  :: LIST(NSIZE) ! A02 Array of ints to be searched
INTEGER,  INTENT(OUT) :: NPOS       ! A04 Location in list of target int

! Local declarations:

INTEGER       :: IFIRST      ! ) First and last ranges during binary
INTEGER       :: ILAST       ! )   search (they home in on NPOS)

!-----------------------------------------------------------------------
!                                       INITIALISE TO COVER WHOLE ARRAY
IFIRST = 0
ILAST  = NSIZE
!                                  LOOP UNTIL RANGE REDUCED TO 1 NUMBER
DO WHILE (IFIRST < ILAST)
!                                         CHOOSE ELEMENT NEAR MID-RANGE
   NPOS = (IFIRST + ILAST + 1) / 2
!                                           COMPARE TARGET WITH CURRENT
!                                          LIMITS AND RESET ONE OF THEM
   IF (NUMBER < LIST(NPOS)) THEN
      ILAST = NPOS - 1
   ELSE
      IFIRST = NPOS
   END IF
END DO
!                 CHECK THAT THE TARGET NUMBER AGREES WITH THE ENTRY IN
!                    THE LIST (SIGN OF "NPOS" IS CHANGED IF IT DOESN'T)
NPOS = IFIRST
IF (NPOS == 0 .OR. NUMBER /= LIST(NPOS)) NPOS = -NPOS
RETURN
END SUBROUTINE ISRCH
