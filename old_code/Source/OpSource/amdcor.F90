SUBROUTINE AMDCOR(POINT,BEND,OAMD,AMDNUM,OCOR,CORNUM,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDCOR
!
! PURPOSE       : TO TEST IF A BULLETIN OR REPORT IS AMENDED OR
!                 CORRECTED AT THIS POINT
!
! DESCRIPTION   : IF THE NEXT THREE CHARACTERS ARE LETTERS AND THEY
!                 START WITH A OR C, THEN ASSUME AMD OR COR.
!                 THIS GROUP IS REMOVED BEFORE RETURNING,
!                 together with any following figures.
!
! CALLED BY     : BULHED, METBUL, TAFBUL ,SYNHED
!
! CALLS         : ACHTST & NCHTST
!
! ARGUMENTS     : (1) POINT  POINTER TO GROUP TO BE CHECKED
!                 (2) BEND   POINTER TO END OF BULLETIN
!                 (3) OAMD   SET IF AMENDMENT FOUND
!                 (4) AMDNUM NUMBER OF AMENDMENT (ZERO IF NONE FOUND)
!                 (5) OCOR   SET IF CORRECTION FOUND
!                 (6) CORNUM NUMBER OF CORRECTION (ZERO IF NONE FOUND)
!                 (7) BULL   REPORT DATA
!
! REVISION INFO :
!
! $Workfile: amdcor.F90$ $Folder: OpSource$
! $Revision: 2$ $Date: 20/12/2010 12:17:21$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         20/12/2010 12:17:21    Stan Kellett
!       corrected declaration to work in build for CR and LF variables on MVS
!  1    MetDB_Refresh 1.0         07/12/2010 15:31:04    John Norton
!       MDBSTOR batch 3 code ready for review
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

! Use statements:
! <Interfaces>

USE achtst_mod
USE nchtst_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,          INTENT(INOUT) :: POINT !A1
INTEGER,          INTENT(INOUT) :: BEND !A2
LOGICAL,          INTENT(OUT)   :: OAMD !A3
CHARACTER(LEN=*), INTENT(OUT)   :: AMDNUM !A4
LOGICAL,          INTENT(OUT)   :: OCOR !A5
CHARACTER(LEN=*), INTENT(OUT)   :: CORNUM !A6
CHARACTER(LEN=*), INTENT(INOUT) :: BULL !A7

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  I
INTEGER          ::  ICHRA
INTEGER          ::  INUM

LOGICAL          ::  OCHAR
LOGICAL          ::  OLFCR
LOGICAL          ::  ONUM
LOGICAL          ::  OSPACE

CHARACTER(LEN=1) ::  CR
CHARACTER(LEN=1) ::  LETTER(9)
CHARACTER(LEN=1) ::  LF
CHARACTER(LEN=2) ::  NUMBER(9)
CHARACTER(LEN=1) ::  SPACE=' '

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DATA LETTER/'A','B','C','D','E','F','G','H','I'/
DATA NUMBER/'01','02','03','04','05','06','07','08','09'/
#if defined (MVS)
CR = CHAR(21)
LF = CHAR(37)
#else
CR = CHAR(13)
LF = CHAR(10)
#endif

OSPACE=.FALSE.
OAMD=.FALSE.
OCOR=.FALSE.
AMDNUM='00'
CORNUM='00'
!
! MOVE PAST ANY SPACES
!
10 CONTINUE
IF (POINT >= BEND) RETURN
IF (BULL(POINT:POINT) == SPACE) THEN
  POINT=POINT+1
  GOTO 10
END IF
!
! FIND OUT HOW MANY LETTERS IN GROUP.  IF 3, TRY AA OR CC FOLLOWED BY
! A LETTER, OR AMD OR COR FOLLOWED BY A SPACE AND 1 OR 2 FIGURES.
!
INUM=0
   20 CALL ACHTST(POINT,4,OCHAR,ICHRA,OSPACE,OLFCR,BULL)
IFLABEL1: &
IF (ICHRA == 4 .AND. (OSPACE.OR.OLFCR)) THEN
IFLABEL2: &
  IF(BULL(POINT:POINT) == 'A')THEN
    OAMD=.TRUE.
IFLABEL3: &
    IF(BULL(POINT+1:POINT+1) == 'A')THEN
      DO I=1,9
       IF (BULL(POINT+2:POINT+2) == LETTER(I)) THEN
         AMDNUM(1:2)=NUMBER(I)
         GO TO 100
       END IF
      END DO
    ELSE IF (BULL(POINT+1:POINT+2) == 'MD') THEN
      CALL NCHTST(POINT+4,3,ONUM,INUM,OSPACE,OLFCR,BULL)
      IF (OSPACE.OR.OLFCR) THEN
        IF (INUM == 2) AMDNUM(2:2)=BULL(POINT+4:POINT+4)
        IF (INUM == 3) AMDNUM(1:2)=BULL(POINT+4:POINT+5)
      END IF
    END IF IFLABEL3
!
  ELSE IF (BULL(POINT:POINT) == 'C') THEN
    OCOR=.TRUE.
IFLABEL4: &
    IF (BULL(POINT+1:POINT+1) == 'C') THEN
      DO I=1,9
       IF(BULL(POINT+2:POINT+2) == LETTER(I))THEN
         CORNUM(1:2)=NUMBER(I)
         GO TO 100
       END IF
      END DO
    ELSE IF (BULL(POINT+1:POINT+2) == 'OR') THEN
      CALL NCHTST(POINT+4,3,ONUM,INUM,OSPACE,OLFCR,BULL)
      IF (OSPACE.OR.OLFCR) THEN
        IF (INUM == 2) CORNUM(2:2)=BULL(POINT+4:POINT+4)
        IF (INUM == 3) CORNUM(1:2)=BULL(POINT+4:POINT+5)
      END IF
    END IF IFLABEL4
  END IF IFLABEL2

! If an AMD or COR group is found, ICHRA must be 4 for 3 letters and
! the space at the end.  If there are figures after a space as above,
! INUM is the number of figures plus 1 for the space before them; but
! if there are no figures after AMD or COR, then INUM has been set to
! 1 by NCHTST, so set it to zero for consistency with the code below,
! which subtracts 1 from INUM whether or not figures were found.

  IF (INUM == 1) INUM=0

ELSE IF (ICHRA == 4 .AND. .NOT.(OSPACE.OR.OLFCR)) THEN
IFLABEL5: &
  IF (BULL(POINT:POINT+2) == 'AMD') THEN
    OAMD=.TRUE.
    CALL NCHTST(POINT+3,3,ONUM,INUM,OSPACE,OLFCR,BULL)
    IF(OSPACE.OR.OLFCR) THEN
      IF (INUM == 2) AMDNUM(2:2)=BULL(POINT+3:POINT+3)
      IF (INUM == 3) AMDNUM(1:2)=BULL(POINT+3:POINT+4)
    END IF
    INUM=INUM-1
!
  ELSE IF (BULL(POINT:POINT+2) == 'COR') THEN
    OCOR=.TRUE.
    CALL NCHTST(POINT+3,3,ONUM,INUM,OSPACE,OLFCR,BULL)
    IF (OSPACE.OR.OLFCR) THEN
      IF (INUM == 2) CORNUM(2:2)=BULL(POINT+3:POINT+3)
      IF (INUM == 3) CORNUM(1:2)=BULL(POINT+3:POINT+4)
    END IF
    INUM=INUM-1
  END IF IFLABEL5
!
! IF NOT 3 LETTERS, RETURN TO CALLER
!
ELSE
  RETURN
END IF IFLABEL1
!
! IF AMD OR COR GROUP FOUND, DELETE IT (& THE FOLLOWING SPACE)
! MAY BE BOTH AMD & COR, SO GO ROUND AGAIN IF NOT BOTH YET
!
100 CONTINUE
IF (OAMD.OR.OCOR) THEN
  BULL(POINT:BEND)=BULL(POINT+ICHRA+INUM:BEND)
  BEND=BEND-ICHRA-INUM
  IF (OSPACE .AND. .NOT.(OAMD.AND.OCOR)) GO TO 10
END IF
!
RETURN
END SUBROUTINE AMDCOR
