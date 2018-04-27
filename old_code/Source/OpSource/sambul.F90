SUBROUTINE SAMBUL(POINT,BEND,TTAAII,CCCC,YYGGGG, &
                  AMD,AMDNUB,COR,CORNUB,NFT,OERR,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : SAMBUL
!
! PURPOSE       : TO FIND THE STARTS OF SAMOS REPORTS IN A BULLETIN
!
! CALLED BY     : MDBSTOR
!
! CALLS         : NCHTST,TAFIND
!
! ARGUMENTS     : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) BEND     NUMBER OF LAST CHARACTER IN BULLETIN
!                 (3) TTAAII   TT TYPE OF BULLETIN (NCNC80 - SAMOS)
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (5) YYGGGG   TIME OF BULLETIN
!                 (6) AMD      SET IF BULLETIN AMENDED
!                 (7) AMDNUB   NUMBER OF BULLETIN AMENDMENT
!                 (8) COR      SET IF BULLETIN CORRECTED
!                 (9) CORNUB   NUMBER OF BULLETIN CORRECTION
!                (10) NFT      FT NUMBER FOR SAMOSX STORAGE
!                (11) OERR     LOGICAL SET IF REPORT SPLITTING ERROR
!                (12) BULL     REPORT DATA
!
! REVISION INFO :
!
! $Workfile: sambul.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 20/12/2010 12:35:44$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         20/12/2010 12:35:44    Stan Kellett
!       corrected declaration of LF and CR for MVS
!  2    MetDB_Refresh 1.1         16/12/2010 17:09:14    John Norton
!       Updated after rework identified by review of MDBSTOR batch 3 done.
!  1    MetDB_Refresh 1.0         07/12/2010 15:31:04    John Norton
!       MDBSTOR batch 3 code ready for review
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
! <Interfaces>

USE nchtst_mod
USE tafind_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,          INTENT(INOUT) :: POINT !A1
INTEGER,          INTENT(IN)    :: BEND !A2
CHARACTER(LEN=6), INTENT(IN)    :: TTAAII !A3
CHARACTER(LEN=4), INTENT(IN)    :: CCCC !A4
CHARACTER(LEN=6), INTENT(IN)    :: YYGGGG !A5
LOGICAL,          INTENT(IN)    :: AMD !A6
CHARACTER(LEN=2), INTENT(IN)    :: AMDNUB !A7 FOR WHOLE BULLETIN
LOGICAL,          INTENT(IN)    :: COR !A8
CHARACTER(LEN=2), INTENT(IN)    :: CORNUB !A9 FOR WHOLE BULLETIN
INTEGER,          INTENT(IN)    :: NFT !A10
LOGICAL,          INTENT(OUT)   :: OERR !A11
CHARACTER(LEN=*), INTENT(INOUT) :: BULL !A12

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

CHARACTER(LEN=1) ::  CR        ! CARRIAGE RETURN
CHARACTER(LEN=4) ::  GGGG
CHARACTER(LEN=1) ::  LF        ! LINE FEED
CHARACTER(LEN=1) ::  SPACE=' '

INTEGER          ::  BLKSIZ=4240
INTEGER          ::  ICC
INTEGER          ::  INUM
INTEGER          ::  IQUAL
INTEGER          ::  RSTART

LOGICAL          ::  OLFCR
LOGICAL          ::  ONUM
LOGICAL          ::  OSPACE

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! CR IS CARRIAGE RETURN,  LF IS LINE FEED

#if defined (MVS)
CR = CHAR(21)
LF = CHAR(37)
#else
CR=CHAR(13)
LF=CHAR(10)
#endif

!**********************************************************************
!
! IGNORE CARRIAGE RETURNS, LINE FEEDS AND SPACES BEFORE NEXT GROUP
!
!**********************************************************************
GGGG=' '
10 CONTINUE
IF (POINT >= BEND) RETURN
IF (BULL(POINT:POINT) == CR .OR. BULL(POINT:POINT) == LF &
   .OR. BULL(POINT:POINT) == SPACE) THEN
  POINT=POINT+1
  GO TO 10
END IF
!**********************************************************************
!
! ACCEPT REPORT IF 5-FIGURE 03... STATION NUMBER, OR IF ONLY 3 FIGURES
! (PUT 03 IN FRONT).  CALL TAFIND TO INDEX AND STORE.
!
!**********************************************************************
CALL NCHTST(POINT,6,ONUM,INUM,OSPACE,OLFCR,BULL)
IFLABEL1: &
IF ((INUM == 6 .AND. BULL(POINT:POINT+1) == '03' .AND. OSPACE) &
.OR.(INUM == 4 .AND. OSPACE)) THEN
  IF (INUM == 4) THEN
    RSTART=POINT-2
    BULL(RSTART:RSTART+1)='03'
  ELSE
    RSTART=POINT
  END IF
  ICC=0
  CALL TAFIND(RSTART,BEND,AMD,COR,AMDNUB,CORNUB,TTAAII, &
              CCCC,YYGGGG,GGGG,ICC,NFT,BLKSIZ,OERR,BULL)
  POINT=RSTART
ELSE
  IQUAL=INDEX(BULL(POINT:BEND),'=')
  IF (IQUAL == 0) RETURN
  POINT=POINT+IQUAL
END IF IFLABEL1
!
! IF MORE DATA TO BE PROCESSED, GO BACK TO START
!
IF (POINT < BEND) GO TO 10
RETURN
END SUBROUTINE SAMBUL
