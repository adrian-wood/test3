      SUBROUTINE ABC (CHAR, IPOS, NSIZE)
!
      IMPLICIT NONE
!---------------------------------------------------------------------!
! SUBROUTINE    : ABC                                                 !
!                                                                     !
! PURPOSE       : TO SORT THE ELEMENTS OF A CHARACTER ARRAY INTO      !
!                 ALPHABETICAL ORDER.                                 !
!                                                                     !
! DESCRIPTION   : 'ABC' SORTS THE ELEMENTS OF THE CHARACTER ARRAY     !
!                 'CHAR' INTO ALPHABETICAL ORDER. AN INTEGER ARRAY    !
!                 'IPOS' IS RE-ORDERED IN THE SAME WAY AND CAN BE     !
!                 USED TO STORE THE UNSORTED POSITIONS OF THE         !
!                 ELEMENTS OF 'CHAR'.  INITIAL VALUES OF 'IPOS' CAN   !
!                 EITHER BE SET BY THE USER OR THE ROUTINE CAN SET    !
!                 THEM TO 1,2,3...  BEFORE SORTING.                   !
!                                                                     !
!                 IF ELEMENTS OF 'CHAR' ARE LONGER THAN CHARACTER*8,  !
!                 SORTING IS BASED ON THE FIRST 8 CHARACTERS ONLY.    !
!                                                                     !
!                 'ABC' USES A VERY EFFICIENT SORTING TECHNIQUE BUT   !
!                 I DO NOT HAVE FURTHER DETAILS. (BRIAN BARWELL)      !
!                                                                     !
! USAGE         : CALL ABC (CHAR, IPOS, NSIZE)                        !
!                                                                     !
! PARAMETERS    : CHAR   I/O  CHARACTER*(*) ARRAY UNSORTED ON INPUT,  !
!                             SORTED ALPHABETICALLY ON OUTPUT.        !
!                 IPOS   I/O  USER-SUPPLIED INTEGER ARRAY RE-ORDERED  !
!                             IN THE SAME WAY AS 'CHAR' (OR CAN BE    !
!                             SET IN THE ROUTINE - SEE NOTE BELOW).   !
!                 NSIZE   I   SIZE OF ARRAYS 'CHAR' AND 'IPOS' (SEE   !
!                             ALSO NOTE BELOW).                       !
!                                                                     !
!                   NOTE:  ELEMENTS OF 'IPOS' WILL BE INITIALISED TO  !
!                          1,2,3...  BEFORE SORTING IF 'NSIZE' IS     !
!                          SUPPLIED WITH A MINUS SIGN.                !
!                                                                     !
! CALLED BY     : ICOLOD                                              !
!                                                                     !
! SOURCE        : MDB.STORAGE.SRCE(ABC)                               !
!                                                                     !
! HISTORY       : THIS ROUTINE STARTED LIFE IN OXFORD UNIVERSITY AS   !
!                 AN ALGOL PROCEDURE FOR SORTING INTEGERS WHICH WAS   !
!                 RECEIVED IN THE MET OFFICE ABOUT 1974 AS PART OF A  !
!                 PACKAGE OF SOFTWARE FOR RADIATION CALCULATIONS. IT  !
!                 WAS TRANSLATED INTO FORTRAN BY BRIAN BARWELL AND    !
!                 ABOUT 1986 WAS MODIFIED BY THE SAME PROGRAMMER TO   !
!                 SORT CHARACTERS ALPHABETICALLY, THE NAME BEING      !
!                 CHANGED TO 'ABC'. SHORTLY AFTERWARDS THE FACILITY   !
!                 TO INITIALISE 'IPOS' WITHIN THE ROUTINE WAS ADDED.  !
!                 IN APRIL 1999 IT WAS MODIFIED FOR MET.D.B. USE BY   !
!                 THE ADDITION OF COMMENTS AND COSMETIC CHANGES ONLY. !
!                                                                     !
! AUTHOR        : BRIAN BARWELL,  IT DIVISION,  APRIL 1999            !
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:32$
! $Source: /home/us0400/mdb/op/lib/source/RCS/abc.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:32    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:16  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/07/10 11:18:51  usmdb
! Change array declaration of IPOS & CHAR from (NSIZE) to (*) to
! prevent array out of bounds failures on HPUX - S.Cox
!
! Revision 1.1  99/05/06  13:57:33  13:57:33  usmdb (Generic MDB account)
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      INTEGER I,J,K,L,M,N       ! LOCAL VARIABLES FOR TEMPORARY STORAGE
      INTEGER IPOS(*)           ! POS OF ELEMENTS BEFORE SORTING    !1.2
      INTEGER INT4              ! LOCAL VARIABLE TO HOLD ELEMENT OF IPOS
      INTEGER NSIZE             ! SIZE OF IPOS AND CHAR ARRAYS
      CHARACTER*(*) CHAR(*)     ! CHARACTER ARRAY TO BE SORTED      !1.2
      CHARACTER*8 CHAR8         ! LOCAL VARIABLE TO HOLD ELEMENT OF CHAR
      CHARACTER*132 HEAD        ! FOR REVISION INFORMATION
!
!                                                  REVISION INFORMATION
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/abc.F,v $
     &'//'$Date: 30/01/2006 20:20:32$ $Revision: 1$'
!
!                                         INITIALISE 'IPOS' IF REQUIRED
      N = IABS(NSIZE)
      IF (NSIZE.GT.0) THEN
         DO J=1,N
           IPOS(J) = J
         END DO ! J
      END IF
!                                                INITIALISE 'I' AND 'M'
      I = 1
      M = 1
!                                          CHOOSE NEXT ELEMENTS TO SWAP
    6 I = I + I
      M = I - 1
      IF(I.LE.N) GO TO 6
    1 M = M/2
      IF(M.EQ.0) GO TO 4
      K = N - M
!
      DO J=1,K
         I = J + M
    2    L = I
         I = I - M
         IF(I.LT.1 .OR. CHAR(L).GE.CHAR(I)) GO TO 3
!
!                                               SWAP ELEMENTS OF 'CHAR'
         CHAR8 = CHAR(I)
         CHAR(I) = CHAR(L)
         CHAR(L) = CHAR8
!                                               SWAP ELEMENTS OF 'IPOS'
         INT4 = IPOS(I)
         IPOS(I) = IPOS(L)
         IPOS(L) = INT4
         GO TO 2
    3    CONTINUE
      END DO ! J
!
      GO TO 1
    4 RETURN
      END
