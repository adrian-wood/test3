SUBROUTINE BULHED(POINT,BULEND,TTAAII,CCCC,YYGGGG, &
                  OAMD,AMDNUM,OCOR,CORNUM,BULL)

!-----------------------------------------------------------------------
!
! ROUTINE       : BULHED
!
! PURPOSE       : TO FIND TTAA, CCCC & DATE/TIME IN BULLETIN HEADER
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, ACHTST,NCHTST, AMDCOR
!
! ARGUMENTS     : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) BULEND   NUMBER OF LAST CHARACTER IN BULLETIN
!                 (3) TTAAII   TT TYPE OF BULLETIN
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (5) YYGGGG   TIME OF BULLETIN
!                 (6) OAMD     SET IF BULLETIN AMENDED
!                 (7) AMDNUM   NUMBER OF BULLETIN AMENDMENT
!                 (8) OCOR     SET IF BULLETIN CORRECTED
!                 (9) CORNUM   NUMBER OF BULLETIN CORRECTION
!                (10) BULL     REPORT DATA
!
! REVISION INFO :
!
! $Workfile: bulhed.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 25/01/2011 21:46:44$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         25/01/2011 21:46:44    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         25/01/2011 15:12:42    John Norton     After
!       initial porting of batches MDBSTOR 19 & 23
!  1    MetDB_Refresh 1.0         19/01/2011 12:17:56    John Norton
!       Pre-porting f77 code for MDBSTOR batch 23.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
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
USE amdcor_mod
USE bulled_mod
USE nchtst_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,          INTENT(INOUT) :: POINT   !a01
INTEGER,          INTENT(OUT)   :: BULEND  !a02
CHARACTER(LEN=*), INTENT(OUT)   :: TTAAII  !a03
CHARACTER(LEN=*), INTENT(OUT)   :: CCCC    !a04
CHARACTER(LEN=*), INTENT(OUT)   :: YYGGGG  !a05
LOGICAL,          INTENT(OUT)   :: OAMD    !a06
CHARACTER(LEN=*), INTENT(OUT)   :: AMDNUM  !a07
LOGICAL,          INTENT(OUT)   :: OCOR    !a08
CHARACTER(LEN=*), INTENT(OUT)   :: CORNUM  !a09
CHARACTER(LEN=*), INTENT(INOUT) :: BULL    !a10

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  INUM
INTEGER          ::  INTCHAR
INTEGER          ::  IQTA
INTEGER          ::  LET
INTEGER          ::  LCCCC
INTEGER          ::  LTIME
INTEGER          ::  LTTAA

CHARACTER(LEN=1) ::  CR ! CARRIAGE RETURN
CHARACTER(LEN=1) ::  LF ! LINE FEED
CHARACTER(LEN=1) ::  SPACE ! SPACE

LOGICAL          ::  ONUM
LOGICAL          ::  OCHAR
LOGICAL          ::  OSPACE
LOGICAL          ::  OLFCR

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

#if defined (MVS)
CR = CHAR(21)
LF = CHAR(37)
SPACE=' '
#else
CR=CHAR(13)
LF=CHAR(10)
SPACE=' '
#endif

OAMD=.FALSE.
AMDNUM='00'
OCOR=.FALSE.
CORNUM='00'
LET=0

!-----------------------------------------------------------------------
! FIND END OF BULLETIN, REJECTING ANY WITH 'QTA' (CANCEL TELEX NUMBER)
!-----------------------------------------------------------------------

BULEND=INDEX(BULL,'NNNN')-1
IF (BULEND <= -1) RETURN
IQTA=INDEX(BULL(1:BULEND),'QTA')
IF (IQTA /= 0) RETURN

!-----------------------------------------------------------------------
! LOOK FOR THE FIRST LETTERS. IF 4 LETTERS, ASSUME TTAA.
! ('TTAAII' WILL NOT BE BEFORE THE 15TH CHARACTER OF THE HEADER.)
!-----------------------------------------------------------------------

#if defined (MVS)
LET=14
#else
LET=0
#endif
10 CONTINUE
LET=LET+1
IF (BULL(LET:LET) < 'A' .OR. BULL(LET:LET) > 'Z') THEN
  IF (LET < 26) THEN
    GO TO 10
  ELSE
    TTAAII=' '
    RETURN
  END IF
END IF
CALL ACHTST(LET,7,OCHAR,INTCHAR,OSPACE,OLFCR,BULL)
IF (INTCHAR /= 5) GO TO 10      ! 5TH CHARACTER MUST NOT BE LETTER
TTAAII(1:6)=BULL(LET:LET+5)   ! IF 4 LETTERS, KEEP TTAAII
LTTAA=LET

!-----------------------------------------------------------------------
! FIND ORIGINATING CENTRE OF BULLETIN (CCCC)
!-----------------------------------------------------------------------

LCCCC=LTTAA+INDEX(BULL(LTTAA:BULEND),SPACE)
CALL ACHTST(LCCCC,5,OCHAR,INTCHAR,OSPACE,OLFCR,BULL)
IF (INTCHAR == 5 .AND. OSPACE) THEN
  CCCC=BULL(LCCCC:LCCCC+3)    ! CCCC IF 4 LETTERS, THEN SPACE
ELSE
  CCCC=' '
  RETURN
END IF

!-----------------------------------------------------------------------
! FIND TIME OF ORIGIN
!-----------------------------------------------------------------------

LTIME=LCCCC+5
CALL NCHTST(LTIME,7,ONUM,INUM,OSPACE,OLFCR,BULL)
IF (INUM == 7 .AND. (OSPACE.OR.OLFCR)) THEN
  YYGGGG=BULL(LTIME:LTIME+5)  ! YYGGGG IF 6 FIGURES, THEN SP/CRLF
  POINT=LTIME+6
  IF (OSPACE) THEN
    CALL AMDCOR(POINT,BULEND,OAMD,AMDNUM,OCOR,CORNUM,BULL)
  END IF
ELSE
  YYGGGG=' '
  RETURN
END IF

!-----------------------------------------------------------------------
! TIDY UP REST OF BULLETIN
!-----------------------------------------------------------------------

CALL BULLED(POINT,BULEND,BULL)

RETURN
END SUBROUTINE BULHED
