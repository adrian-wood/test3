SUBROUTINE CLMBUL(POINT,BEND,TTAAII,CCCC,OCOR,NFT,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : CLMBUL
!
! PURPOSE       : To store CLIMAT bulletins, as data for the month in
!                 the month/year group or last month if no group found
!                 (Attempts to store individual reports were abandoned
!                 when one bulletin had some plain language to switch
!                 from monthly to 10-day obs, indistinguishable from
!                 monthly obs if stored without the plain language!)
!
! CALLED BY     : MDBSTOR
!
! CALLS         : NCHTST, DATIM, BULLED, CENTURY, TAFREP
!
! ARGUMENTS     : (1) POINT    to next group (group after CLIMAT?) (i)
!                 (2) BEND     POSN OF LAST CHARACTER IN BULLETIN  (i)
!                 (3) TTAAII   TYPE OF BULLETIN                    (i)
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN     (i)
!                 (5) OCOR     TRUE IF BULLETIN CORRECTED          (i)
!                 (6) NFT      FT NUMBER FOR CLIMAT STORAGE        (i)
!                 (7) BULL     bulletin                            (i)
!
! REVISION INFO :
!
! $Workfile: clmbul.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 06/04/2011 17:24:12$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         06/04/2011 17:24:12    Brian Barwell
!       Initialise elements 4 and 5 of DATIME before calling TAFREP.
!  3    MetDB_Refresh 1.2         14/01/2011 11:30:31    Alison Weir
!       Correct character in Revision info
!  2    MetDB_Refresh 1.1         14/01/2011 11:21:03    Alison Weir     Ported
!        to f95 - MDBSTOR batch12
!  1    MetDB_Refresh 1.0         05/01/2011 17:04:51    Alison Weir
!       MDBSTOR batch 12 initial F77 version
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

! Use statements:

USE nchtst_mod
USE datim_mod
USE bulled_mod
USE century_mod
USE ivalue_mod
USE tafrep_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) ::   POINT  !A01
INTEGER,          INTENT(INOUT) ::   BEND   !A02
CHARACTER(LEN=*), INTENT(IN)    ::   TTAAII !A03
CHARACTER(LEN=*), INTENT(IN)    ::   CCCC   !A04
LOGICAL,          INTENT(IN)    ::   OCOR   !A05
INTEGER,          INTENT(IN)    ::   NFT    !A06
CHARACTER(LEN=*), INTENT(INOUT) ::   BULL   !A07

! Local declarations:

CHARACTER(LEN=5)  ::    MMJJJ     ! MM=MONTH, JJJ=YEAR E.G. 1994 =994
CHARACTER(LEN=6)  ::    YYGGGG    ! day & hour of bulletin
CHARACTER(LEN=23) ::    ENTRY     ! to store whole bulletin
CHARACTER(LEN=9)  ::    IDENT     ! identifier in index entry

INTEGER           ::   DATIME(5) ! to store whole bulletin
INTEGER           ::   NOW(8)
INTEGER           ::   IMON
INTEGER           ::   IYR
INTEGER           ::   MONDIF
INTEGER           ::   BLKSIZ = 27998
INTEGER           ::   INUM
INTEGER           ::   INC     ! pointer to C in TTAAii
INTEGER           ::   IXCCCC   ! displacement of CCCC in BULL

LOGICAL           ::   ONUM
LOGICAL           ::   OLFCR
LOGICAL           ::   OSPACE
LOGICAL           ::   TEMP       ! set if TEMP after CLIMAT

! Convert CRLF to space and strings of spaces to single spaces

CALL BULLED(POINT,BEND,BULL)

! Look for 'TEMP' on second line of bulletin (should be between
! CLIMAT & MMJJJ).  If TEMP, month can be in range 51-62 if wind
! is in knots.  (+20 below is arbitrary.)

TEMP=INDEX(BULL(POINT:MIN(POINT+20,BEND)),'TEMP') > 0

! Find CCCC & assume 6-figure YYGGGG is next group (day/hour/min)

IXCCCC=INDEX(BULL,CCCC)
YYGGGG=BULL(IXCCCC+5:IXCCCC+10)

! Look for 5-figure (or 4-figure) group with acceptable month & year
! (Keep looking for 5-figure or 4-figure group till end of bulletin!)
! (Code below sets 100s of year equal to tens in MMJJJ (to be stored),
! but only two figures used to convert IYR, so almost OK after 2009!)

MMJJJ=' '
DO WHILE (MMJJJ == ' ' .AND. POINT < BEND)
  CALL NCHTST(POINT,6,ONUM,INUM,OSPACE,OLFCR,BULL)

  IF ((INUM == 6.OR.INUM == 5).AND.OSPACE) THEN
    MMJJJ(1:5)=BULL(POINT:POINT+4)
    IF (INUM == 5) MMJJJ(4:5)=BULL(POINT+2:POINT+3) ! only 4 figs
  END IF

  POINT=POINT+INUM
END DO
IF (POINT >= BEND) RETURN

! Convert year & month & check that they're in valid range

IYR=IVALUE(MMJJJ(4:5))        ! Get 2-figure year ...
IYR=IYR+CENTURY(IYR)          ! ... to use in Y2000 test.
IMON=IVALUE(MMJJJ(1:2))
IF (TEMP .AND. IMON > 50.AND.IMON <= 62) IMON=IMON-50
IF (IMON < 1 .OR. IMON > 12) MMJJJ=' '

! Find how many months' difference between the data & today's date.
! Tolerance of 3 below depends on how many months in on-line data set.
! 3 stores data for the month just ended & the 2 months before that.
! Assume that the report for a month can't come in on the last day
! of the month: the UK manual says rainfall is till 9Z on the 1st.

CALL DATIM(NOW)
MONDIF=NOW(8)*12+NOW(7)-(IYR*12+IMON)
IF (MONDIF > 3.OR.MONDIF <= 0) MMJJJ=' '

! If no month/year group recognised,
! store the data as if it were for the previous month.

IF (MMJJJ == ' ') THEN
  IMON=NOW(7)-1
  IYR=NOW(8)
  IF (IMON == 0) THEN
    IMON=12
    IYR=IYR-1
  END IF
END IF

!**********************************************************************
!
! Store the whole bulletin - it can't easily be split into reports
! String starts CS or CU (skipping ZCZC) & ident starts S or U
! (TTAAii & CCCC from bulletin heading with initial C omitted)
! Use day & hour (current rather than from bulletin heading as
! heading not passed) to make long chains less likely
!
! Data set tags are day/hour with no mention of month, so set day from
! month in such a way that the tag for a block to be reused is never
! for decades!) equal to the new tag.
!
!**********************************************************************

DATIME(1)=IYR
DATIME(2)=IMON
DATIME(3)=14+MOD(IMON,4)
DATIME(4)=0
DATIME(5)=0

IF (OCOR) THEN
  ENTRY(3:11)=TTAAii(3:6)//CHAR(1)//CCCC(1:4)
ELSE
  ENTRY(3:11)=TTAAii(3:6)//CHAR(0)//CCCC(1:4)
END IF
ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)

INC=INDEX(BULL(5:BEND),'C')          ! skip ZCZC!
IDENT=TTAAII(2:4)//YYGGGG(1:4)//CCCC(1:2)
CALL TAFREP(DATIME,ENTRY,BULL(4+INC:BEND),NFT,BLKSIZ,IDENT)

RETURN
END SUBROUTINE CLMBUL
