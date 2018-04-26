SUBROUTINE SYNHED(POINT,BULEND,TTAAII,CCCC,YYGGGG, &
                  OAMD,AMDNUM,OCOR,CORNUM,MIMJ,IERR,BULL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNHED
!
! PURPOSE       : TO FIND TTAA, CCCC & DATE/TIME AND MIMIMJMJ FROM
!                 BULLETIN HEADER.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : ACHTST,NCHTST, AMDCOR
!
! ARGUMENTS     : (1) POINT    POSITION OF CHARACTER AFTER LAST
!                              IDENTIFIED HEADING GROUP ON EXIT    (O)
!                 (2) BULEND   POSITION OF LAST CHAR IN BULLETIN
!                              (1 BEFORE NNNN)                     (O)
!                 (3) TTAAII   BULLETIN IDENTIFIER                 (O)
!                 (4) CCCC     COLLECTING CENTRE                   (O)
!                 (5) YYGGGG   TIME OF BULLETIN                    (O)
!                 (6) OAMD     SET IF BULLETIN AMENDED             (O)
!                 (7) AMDNUM   NUMBER OF BULLETIN AMENDMENT        (O)
!                 (8) OCOR     SET IF BULLETIN CORRECTED           (O)
!                 (9) CORNUM   NUMBER OF BULLETIN CORRECTION       (O)
!                (10) MIMJ     MIMIMJMJ OF BULLETIN                (O)
!                (11) IERR     SET TO 16 FOR A DUD BULLETIN        (O)
!                      (I.E. NO TTAA, A QTA, NO NNNN )
!                (12) BULL     BULLETIN                            (I)
!
! REVISION INFO :
!
! $Workfile: synhed.F90$ $Folder: OpSource$
! $Revision: 4$ $Date: 20/12/2010 16:56:04$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         20/12/2010 16:56:04    Sheila Needham  As
!       before - with the right values!
!  3    MetDB_Refresh 1.2         20/12/2010 16:53:09    Sheila Needham
!       Corrected CR/LF values in #if defined(MVS) block
!  2    MetDB_Refresh 1.1         20/12/2010 16:09:39    Alison Weir
!       Initialise POINT intent=out argument
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

! Use statements:
USE achtst_mod
USE amdcor_mod
USE nchtst_mod

IMPLICIT NONE

! Subroutine arguments:
INTEGER,          INTENT(OUT)   :: POINT    !A01
INTEGER,          INTENT(OUT)   :: BULEND   !A02
CHARACTER(LEN=6), INTENT(OUT)   :: TTAAII   !A03
CHARACTER(LEN=4), INTENT(OUT)   :: CCCC     !A04
CHARACTER(LEN=6), INTENT(OUT)   :: YYGGGG   !A05
LOGICAL,          INTENT(OUT)   :: OAMD     !A06
CHARACTER(LEN=2), INTENT(OUT)   :: AMDNUM   !A07
LOGICAL,          INTENT(OUT)   :: OCOR     !A08
CHARACTER(LEN=2), INTENT(OUT)   :: CORNUM   !A09
CHARACTER(LEN=*), INTENT(OUT)   :: MIMJ     !A10
INTEGER,          INTENT(OUT)   :: IERR     !A11
CHARACTER(LEN=*), INTENT(INOUT) :: BULL     !A12

! Local declarations:

INTEGER          ::  ICHAR
INTEGER          ::  LET
INTEGER          ::  LTTAA
INTEGER          ::  LCCCC
INTEGER          ::  LTIME
INTEGER          ::  INUM
INTEGER          ::  I
INTEGER          ::  LMIMJ

CHARACTER(LEN=1) ::  CH
CHARACTER(LEN=1) ::  CR
CHARACTER(LEN=1) ::  LF
CHARACTER(LEN=1) ::  SPACE=' '

LOGICAL          ::  ONUM
LOGICAL          ::  OCHAR
LOGICAL          ::  OSPACE
LOGICAL          ::  OLFCR

#if defined (MVS)
CR=CHAR(21)
LF=CHAR(37)
#else
CR=CHAR(13)
LF=CHAR(10)
#endif

OAMD=.FALSE.
AMDNUM='00'
OCOR=.FALSE.
CORNUM='00'
IERR=0
MIMJ=' '
YYGGGG=' '
TTAAII=' '
CCCC=' '
POINT=0

!-----------------------------------------------------------------------
! FIND END OF BULLETIN
!-----------------------------------------------------------------------

BULEND=INDEX(BULL,'NNNN')-1
IF (BULEND <= -1) THEN
  IERR=16
  GOTO 999
END IF

!-----------------------------------------------------------------------
! LOOK FOR THE FIRST LETTERS. IF 4 LETTERS, ASSUME TTAA.
! ('TTAAII' WILL NOT BE BEFORE THE 15TH CHARACTER OF THE HEADER.)
!-----------------------------------------------------------------------

#if defined (MVS)
LET=14
#else
LET=0
#endif
   10 LET=LET+1
CH=BULL(LET:LET)
IF (CH < 'A' .OR. (CH > 'I' .AND. CH < 'J') .OR. &
   (CH > 'R' .AND. CH < 'S') .OR. CH > 'Z')THEN
  IF (LET < 26) THEN
    GO TO 10
  ELSE
    TTAAII=' '
    IERR=16
    GOTO 999
  END IF
END IF
CALL ACHTST(LET,7,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
IF (ICHAR /= 5) GO TO 10      ! 5-TH CHARACTER MUST NOT BE LETTER
IF(BULL(LET+4:) /= ' ') THEN  ! IF NOT ' ' THEN AT LEAST ONE    B
                              ! FIGURE LET+5 MAY BE ' ' OR FIG. B
  TTAAII(1:6)=BULL(LET:LET+5)
ELSE
  TTAAII(1:6)=BULL(LET:LET+3)
END IF
LTTAA=LET

!-----------------------------------------------------------------------
! FIND ORIGINATING CENTRE OF BULLETIN (CCCC)
!-----------------------------------------------------------------------

LCCCC=LTTAA+INDEX(BULL(LTTAA:BULEND),SPACE)
CALL ACHTST(LCCCC,5,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
IF (ICHAR == 5 .AND. OSPACE) THEN
  CCCC=BULL(LCCCC:LCCCC+3)    ! CCCC IF 4 LETTERS, THEN SPACE
  POINT=LCCCC+5
ELSE
  IERR=16
  GOTO 999
END IF

!-----------------------------------------------------------------------
! FIND TIME OF ORIGIN
!-----------------------------------------------------------------------

LTIME=LCCCC+INDEX(BULL(LCCCC:BULEND),SPACE)
CALL NCHTST(LTIME,7,ONUM,INUM,OSPACE,OLFCR,BULL)
IF (INUM == 7 .AND. (OSPACE.OR.OLFCR)) THEN
  YYGGGG=BULL(LTIME:LTIME+5)  !YYGGGG IF 6 FIGURES,THEN SP/CRLF
  POINT=LTIME+6
  IF(OSPACE)CALL AMDCOR(POINT,BULEND,OAMD,AMDNUM,OCOR,CORNUM,BULL)
ELSE
  YYGGGG=' '
END IF

!-----------------------------------------------------------------------
! MOVE PAST THE END OF THIS LINE
!-----------------------------------------------------------------------

LET=POINT
I=INDEX(BULL(LET:BULEND),LF)
IF(I > 0)THEN
  LET=LET+I
20      IF (BULL(LET:LET) == CR.OR.BULL(LET:LET) == LF.OR. &
        BULL(LET:LET) == SPACE)THEN
    LET=LET+1
    GOTO 20
  ELSE
    POINT=LET
  END IF
END IF

!THE NEXT SECTION TO FIND MIMIMJMJ WILL BE SKIPPED IF THE BULLETIN
!IS A TROPICAL ADVISORY. THIS PREVENTS THE WORD 'TYPHOON' BEING
!TREATED AS AN MIMIMJMJ AND NOT AS PART OF THE REPORT.

IFLABEL1: &
IF (TTAAII(1:2)  /=  'WT' .AND. TTAAII(1:2)  /=  'WH' .AND. &
    TTAAII(1:2)  /=  'FX') THEN

!-----------------------------------------------------------------------
! FIND MIMIMJMJ OR WORD AT START OF SECOND LINE (UP TO 7 CHARACTERS)
! IF THE NEXT CHARACTER IS NOT A LETTER, ADVANCE THE POINTER; IF IT IS
! A LETTER, SEE HOW MANY LETTERS AND RETURN IT AS MIMJ IF NOT MORE THAN
! 7, SKIPPING 'NIL' OR 'OTR' (THIS RETURNS 'CLIMAT' ETC).
! IF 'SHPANC' IS FOUND (SPURIOUS GROUP COMMON IN SHPSYN REPORTS
! FROM 'PANC') SKIP IT AND LOOK FOR THE NEXT GROUP.
!-----------------------------------------------------------------------

  LMIMJ=POINT
  I=0
30      CONTINUE
  I=I+1
  CH= BULL(LMIMJ:LMIMJ)
IFLABEL2: &
  IF (CH < 'A' .OR. (CH > 'I' .AND. CH < 'J') .OR. &
      (CH > 'R' .AND. CH < 'S') .OR. CH > 'Z')THEN
    IF (I < 10) THEN
      LMIMJ=LMIMJ+1
      GO TO 30
    ELSE
      MIMJ=' '
    END IF
  ELSE
    CALL ACHTST(LMIMJ,8,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
IFLABEL3: &
    IF (ICHAR <= 8.AND.(OSPACE.OR.OLFCR))THEN
      MIMJ=BULL(LMIMJ:LMIMJ+ICHAR-2)
      IF(MIMJ == 'NIL'.OR.MIMJ == 'OTR')THEN
        MIMJ=' '
      ELSE IF (MIMJ == 'SHPANC') THEN
        LMIMJ = LMIMJ + ICHAR           ! Skip 'SHPANC'
        I = 0                           ! Reset counter
        GO TO 30                        ! Find next group
      ELSE
        POINT=LMIMJ+ICHAR-1
      END IF
    ELSE
      MIMJ=' '
    END IF IFLABEL3
  END IF IFLABEL2
END IF IFLABEL1
999   CONTINUE
RETURN
END SUBROUTINE SYNHED
