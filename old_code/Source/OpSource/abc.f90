SUBROUTINE ABC (CHAR_ARR, IPOS, NSIZE)
!
!-----------------------------------------------------------------------
! SUBROUTINE    : ABC
!
! PURPOSE       : TO SORT THE ELEMENTS OF A CHARACTER ARRAY INTO
!                 ALPHABETICAL ORDER.
!
! DESCRIPTION   : 'ABC' SORTS THE ELEMENTS OF THE CHARACTER ARRAY
!                 'CHAR' INTO ALPHABETICAL ORDER. AN INTEGER ARRAY
!                 'IPOS' IS RE-ORDERED IN THE SAME WAY AND CAN BE
!                 USED TO STORE THE UNSORTED POSITIONS OF THE
!                 ELEMENTS OF 'CHAR'.  INITIAL VALUES OF 'IPOS' CAN
!                 EITHER BE SET BY THE USER OR THE ROUTINE CAN SET
!                 THEM TO 1,2,3...  BEFORE SORTING.
!
!                 IF ELEMENTS OF 'CHAR' ARE LONGER THAN CHARACTER*8,
!                 SORTING IS BASED ON THE FIRST 8 CHARACTERS ONLY.
!
!                 'ABC' USES A VERY EFFICIENT SORTING TECHNIQUE BUT
!                 I DO NOT HAVE FURTHER DETAILS. (BRIAN BARWELL)
!
! USAGE         : CALL ABC (CHAR, IPOS, NSIZE)
!
! ARGUMENTS     : CHAR_ARR I/O  CHARACTER*(*) ARRAY UNSORTED ON
!                             INPUT, SORTED ALPHABETICALLY ON OUTPUT.
!                 IPOS   I/O  USER-SUPPLIED INTEGER ARRAY RE-ORDERED
!                             IN THE SAME WAY AS 'CHAR' (OR CAN BE
!                             SET IN THE ROUTINE - SEE NOTE BELOW).
!                 NSIZE   I   SIZE OF ARRAYS 'CHAR_ARR' AND 'IPOS'
!                             (SEE ALSO NOTE BELOW).
!
!                   NOTE:  ELEMENTS OF 'IPOS' WILL BE INITIALISED TO
!                          1,2,3...  BEFORE SORTING IF 'NSIZE' IS
!                          SUPPLIED WITH A MINUS SIGN.
!
! CALLED BY     : MONITOR
!
! SOURCE        : MDB.STORAGE.SRCE(ABC)
!
! HISTORY       : THIS ROUTINE STARTED LIFE IN OXFORD UNIVERSITY AS
!                 AN ALGOL PROCEDURE FOR SORTING INTEGERS WHICH WAS
!                 RECEIVED IN THE MET OFFICE ABOUT 1974 AS PART OF A
!                 PACKAGE OF SOFTWARE FOR RADIATION CALCULATIONS. IT
!                 WAS TRANSLATED INTO FORTRAN BY BRIAN BARWELL AND
!                 ABOUT 1986 WAS MODIFIED BY THE SAME PROGRAMMER TO
!                 SORT CHARACTERS ALPHABETICALLY, THE NAME BEING
!                 CHANGED TO 'ABC'. SHORTLY AFTERWARDS THE FACILITY
!                 TO INITIALISE 'IPOS' WITHIN THE ROUTINE WAS ADDED.
!                 IN APRIL 1999 IT WAS MODIFIED FOR MET.D.B. USE BY
!                 THE ADDITION OF COMMENTS AND COSMETIC CHANGES ONLY.
!
! AUTHOR        : BRIAN BARWELL,  IT DIVISION,  APRIL 1999
!
! REVISION INFO :
!
! $Workfile: abc.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 07/06/2011 15:53:08$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         07/06/2011 15:53:08    Brian Barwell
!       Restructure IF test.
!  3    MetDB_Refresh 1.2         18/02/2011 14:35:41    John Norton     Rework
!        done as listed in review document MONITORBatches1&2.doc
!  2    MetDB_Refresh 1.1         14/02/2011 14:29:03    John Norton     After
!       porting for Monitor batches 1 & 2
!  1    MetDB_Refresh 1.0         07/02/2011 11:28:26    John Norton     f77
!       version of MONITOR porting batch 
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)    :: NSIZE       !a03 SIZE OF IPOS AND CHAR ARRAYS
CHARACTER(LEN=8), INTENT(INOUT) :: CHAR_ARR(*) !a01 CHARACTER ARRAY TO BE SORTED
INTEGER,          INTENT(INOUT) :: IPOS(*)     !a02 POS OF ELEMENTS BEFORE SORTING

! Local declarations:

INTEGER          ::  I    ! LOCAL VARIABLE FOR TEMPORARY STORAGE
INTEGER          ::  J    ! LOCAL VARIABLE FOR TEMPORARY STORAGE
INTEGER          ::  K    ! LOCAL VARIABLE FOR TEMPORARY STORAGE
INTEGER          ::  L    ! LOCAL VARIABLE FOR TEMPORARY STORAGE
INTEGER          ::  M    ! LOCAL VARIABLE FOR TEMPORARY STORAGE
INTEGER          ::  N    ! LOCAL VARIABLE FOR TEMPORARY STORAGE
INTEGER          ::  INT4 ! LOCAL VARIABLE TO HOLD ELEMENT OF IPOS
CHARACTER(LEN=8) ::  CHAR8 ! LOCAL VARIABLE TO HOLD ELEMENT OF CHAR
LOGICAL          ::  NOSWAP ! .TRUE. While chosing element to swap

!-----------------------------------------------------------------------
!                                         INITIALISE 'IPOS' IF REQUIRED
N = IABS(NSIZE)
IF (NSIZE > 0) THEN
   DO J=1,N
     IPOS(J) = J
   END DO ! J
END IF
!                                                INITIALISE 'I' AND 'M'
I = 1
M = 1
!                                          CHOOSE NEXT ELEMENTS TO SWAP
DO WHILE (I <= N)
  I = I + I
  M = I - 1
END DO

M = M/2
DOLABEL1: &
DO WHILE (M /= 0)
  K = N - M

DOLABEL2: &
  DO J=1,K
    I = J + M
    NOSWAP=.TRUE.
DOLABEL3: &
    DO WHILE (NOSWAP)
      L = I
      I = I - M
      IF (I < 1) THEN
        NOSWAP=.FALSE.
      ELSE IF (CHAR_ARR(L)(1:8) >= CHAR_ARR(I)(1:8)) THEN
        NOSWAP=.FALSE.
      ELSE
!                                               SWAP ELEMENTS OF 'CHAR'
        CHAR8(1:8) = CHAR_ARR(I)(1:8)
        CHAR_ARR(I)(1:8) = CHAR_ARR(L)(1:8)
        CHAR_ARR(L)(1:8) = CHAR8(1:8)
!                                               SWAP ELEMENTS OF 'IPOS'
        INT4 = IPOS(I)
        IPOS(I) = IPOS(L)
        IPOS(L) = INT4
      END IF
    END DO DOLABEL3 ! While NOSWAP
  END DO DOLABEL2 ! J
  M = M/2
END DO DOLABEL1 ! While (m /= 0)
!
RETURN
END SUBROUTINE ABC
