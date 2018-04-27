SUBROUTINE TAFIND(POINT,BEND,OAMD,OCOR,AMDNUM,CORNUM,TTAAII, &
                  CCCC,YYGGGG,GGGG,ICC,NFT,BLKSIZ,OERR,BULL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : TAFIND
!
! PURPOSE       : TO PASS A TAF OR METAR TO THE APPROPRIATE STORAGE
!                 PROGRAM, COMPLETING ANY 3-LETTER IDENTIFIER AND
!                 STARTING AN INDEX ENTRY (AFTER FURTHER CHECKS
!                 FOR COR, AMD & NIL).
!
! CALLED BY     : METBUL, TAFBUL & SAMBUL
!
! CALLS         : TAFREP, ZPDATE(DATE13,DATE31),DATIM
!
! ARGUMENTS     : (1) POINT    PASSED AS POINTER TO START OF REPORT,
!                              RETURNED AS NEXT CHARACTER TO HANDLE.
!                 (2) BEND     NUMBER OF LAST CHARACTER IN BULLETIN
!                 (3) OAMD     LOGICAL SET IF REPORT AMENDED
!                 (4) OCOR     LOGICAL SET IF REPORT CORRECTED
!                 (5) AMDNUM   NUMBER OF AMENDMENT
!                 (6) CORNUM   NUMBER OF CORRECTION
!                 (7) TTAAII   TT TYPE OF BULLETIN (SA IS METAR)
!                 (8) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (9) YYGGGG   BULLETIN DATE/TIME GROUP
!                (10) GGGG     REPORT TIME GROUP (SPACES IF NONE)
!                (11) ICC      NUMBER OF LETTERS IN STATION NAME
!                (12) NFT      FT NUMBER FOR STORAGE OF THIS DATA
!                (13) BLKSIZ   BLOCK SIZE OF DATA SET FOR THIS DATA
!                (14) OERR     LOGICAL SET IF ERROR IN REPORT (IF 3-
!                               LETTER ID & CCCC NOT 'C...' OR 'K...')
!                (15) BULL     REPORT DATA
!
! REVISION INFO :
!
! $Workfile: tafind.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 19/01/2011 09:40:18$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         19/01/2011 09:40:18    John Norton
!       Updated after rework for MDBSTOR batch 4 done.
!  3    MetDB_Refresh 1.2         07/01/2011 09:46:52    John Norton     Post
!       MDBSTOR batch 4 porting.
!  2    MetDB_Refresh 1.1         07/01/2011 09:42:39    John Norton
!       Original f77 pre-porting version!
!  1    MetDB_Refresh 1.0         10/12/2010 16:42:02    John Norton     After
!       MDBSTOR batch 4 porting.
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

USE datim_mod
USE tafrep_mod
USE zpdate_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) :: POINT !a1
INTEGER,          INTENT(IN)    :: BEND !a2
LOGICAL,          INTENT(IN)    :: OAMD !a3
LOGICAL,          INTENT(IN)    :: OCOR !a4
CHARACTER(LEN=2), INTENT(IN)    :: AMDNUM !a5
CHARACTER(LEN=2), INTENT(IN)    :: CORNUM !a6
CHARACTER(LEN=6), INTENT(IN)    :: TTAAII !a7
CHARACTER(LEN=4), INTENT(IN)    :: CCCC !a8
CHARACTER(LEN=6), INTENT(IN)    :: YYGGGG !a9
CHARACTER(LEN=4), INTENT(INOUT) :: GGGG !a10
INTEGER,          INTENT(IN)    :: ICC !a11
INTEGER,          INTENT(IN)    :: NFT !a12
INTEGER,          INTENT(IN)    :: BLKSIZ !a13
LOGICAL,          INTENT(OUT)   :: OERR !a14
CHARACTER(LEN=*), INTENT(INOUT) :: BULL !a15

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER           ::  BULHOUR
INTEGER           ::  DATIME(5)
CHARACTER(LEN=23) ::  ENTRY
INTEGER           ::  ICOR
INTEGER           ::  NAMD
INTEGER           ::  NCENDY
INTEGER           ::  NCOR
INTEGER           ::  NDAY
INTEGER           ::  NMONTH
INTEGER           ::  NOW(9)
INTEGER           ::  NYEAR
INTEGER           ::  REND
INTEGER           ::  RSTART

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!

!
! SET MISSING POSITION IN INDEX ENTRY: IT WILL BE LOOKED UP IN TAFREP.
!
ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)
OERR=.FALSE.
!**********************************************************************
!                                                                     *
! FIND END OF REPORT (NEXT EQUAL SIGN).  IF NO EQUAL SIGN, RETURN     *
!                                                                     *
!**********************************************************************
RSTART=POINT
REND=RSTART+INDEX(BULL(RSTART:BEND),'=')-2
IF (REND <= RSTART .OR. REND > BEND) THEN
  POINT=BEND
  OERR=.TRUE.
  RETURN
END IF
!**********************************************************************
!                                                                     *
! RETURN IF 'NIL'.  IF 'COR' FOUND ANYWHERE, SET COUNT.               *
! TAFBUL checks for COR in various places, so this check shouldn't
! be needed, though perhaps it does no harm.  A similar check for
! AMD was removed, as it set the flag regardless of context, e.g.
! even from 'no AMD '!
!                                                                     *
!**********************************************************************
IF (INDEX(BULL(RSTART+4:RSTART+16),'NIL') > 0) GO TO 999
!
NAMD=0
IF (AMDNUM > '01') THEN
  NAMD=ICHAR(AMDNUM(2:2))-ICHAR('0')
ELSE IF (OAMD) THEN
  NAMD=1
END IF
!
NCOR=0
IF (.NOT.OCOR) ICOR=INDEX(BULL(RSTART+4:REND),' COR ')
IF (CORNUM > '01') THEN
  NCOR=ICHAR(CORNUM(2:2))-ICHAR('0')
ELSE IF (OCOR .OR. ICOR > 0) THEN
  NCOR=1
END IF
!**********************************************************************
!                                                                     *
! IF THE COLLECTING CENTRE IS C... OR K... (CANADA OR USA),           *
! COMPLETE A 3-LETTER IDENTIFIER WITH C OR K RESPECTIVELY.            *
! IF THE COLLECTING CENTRE IS CROUGHTON (EGWR), ASSUME A 3-LETTER     *
! IDENTIFIER STARTING WITH Y IS CANADIAN, SET ANY OTHER TO USA.       *
! (THIS IS RISKY: CU, CW, CY & CZ ARE CANADIAN REGIONS, KW, KY & KZ   *
!  ARE US REGIONS; BUT CY HAS LOTS OF AIRFIELDS, KY VERY FEW...)      *
!                                                                     *
!**********************************************************************
IFLABEL1: &
IF (ICC == 3) THEN
IFLABEL2: &
  IF (CCCC(1:1) == 'C' .OR. CCCC(1:1) == 'K') THEN
    RSTART=RSTART-1
    BULL(RSTART:RSTART)=CCCC(1:1)
  ELSE IF (BULL(RSTART:RSTART+1) == CCCC(2:3)) THEN
    RSTART=RSTART-1
    BULL(RSTART:RSTART)=CCCC(1:1)
    PRINT *,'Only 3-letter ident:',BULL(RSTART+1:RSTART+3)
    PRINT *,'Assume it should be:',BULL(RSTART:RSTART+3), &
            '(with middle letters as in CCCC)'
    PRINT *,BULL(RSTART:MIN(RSTART+80,BEND))
  ELSE IF (CCCC == 'EGWR') THEN
    RSTART=RSTART-1
    IF (BULL(RSTART+1:RSTART+1) == 'Y') THEN
      BULL(RSTART:RSTART)='C'
    ELSE
      BULL(RSTART:RSTART)='K'
    END IF
  ELSE
    PRINT *,BULL(RSTART:RSTART+2),' - BUT CCCC IS ',CCCC
    OERR=.TRUE.
    GO TO 999
  END IF IFLABEL2
END IF IFLABEL1
!**********************************************************************
!                                                                     *
! PASS REPORT AND ASSOCIATED INFORMATION ON FOR STORING IN DATABASE,  *
! SETTING DATE/TIME FROM YYGGGG, GGGG & CURRENT YEAR/MONTH/DAY AND    *
! MAKING CHARACTER STRING OF TTAA11, AMD/COR & CCCC TO GO IN INDEX.   *
!                                                                     *
! TAKE CURRENT YEAR & MONTH, DAY FROM BULLETIN & TIME FROM REPORT     *
! UNLESS (1) 0Z DATA FOR 1ST BEFORE 0Z (MAY BE NEXT MONTH)            *
!        (2) BULLETIN DAY > CURRENT DAY (0Z OB OR LAST MONTH'S DATA)  *
!        (3) REPORT HOUR > BULLETIN HOUR (REPORT FOR DAY BEFORE)      *
! (ASSUME EARLY 0Z DATA IF DATA TIME <0010 & CURRENT TIME >2330)      *
!                                                                     *
!**********************************************************************
CALL DATIM(NOW)                             ! CURRENT DATE
DATIME(1)=NOW(8)                            ! YEAR
DATIME(2)=NOW(7)                            ! MONTH
!
! CHECK YY FALLS WITHIN A VALID DAY RANGE
!
READ (YYGGGG(1:2),'(I2)') DATIME(3)         ! DAY
IF (DATIME(3) > 31 .OR. DATIME(3) < 1) THEN
  WRITE (6,*) 'BAD TIME GROUP (YY): ',YYGGGG(1:2), &
  BULL(RSTART:MIN(RSTART+50,BEND))
  GO TO 999
END IF
!
! READ TIME (HOURS AND MINUTES) IF NOT ALREADY SET
!
IF (GGGG == '    ') GGGG=YYGGGG(3:6)
!
! CHECK GGGG FALLS WITHIN VALID TIME RANGE
!
READ (GGGG(1:2),'(I2)') DATIME(4)           ! HOUR
READ (GGGG(3:4),'(I2)') DATIME(5)           ! MINUTE
IF (DATIME(4)  >=  24 .OR. DATIME(5)  >=  60) THEN
  PRINT *,'BAD TIME GROUP: ',GGGG,'   ', &
           BULL(RSTART:MIN(RSTART+50,BEND))
  GO TO 999
END IF
!
! If bulletin is for 0Z on 1st & current date is end of month,
! then assume it's next month's data.
!
IFLABEL3: &
IF (YYGGGG(1:4) == '0100' .AND. NOW(6) >= 28) THEN
  CALL DATE31(NOW(6),NOW(7),NOW(8),NCENDY)  ! CURRENT CENTURY-DAY
  CALL DATE13(NCENDY+1,NDAY,NMONTH,NYEAR)   ! TOMORROW'S DATE
  IF (NDAY == 1) THEN                       ! IF IT'S THE FIRST,
    DATIME(2)=NMONTH                        ! NEXT MONTH'S DATA
    DATIME(1)=NYEAR
  END IF
!
! If day of data is greater than current day (and it's not a
! midnight bulletin received just before 00Z), change the month
! to last month to avoid giving TAFREP a date in the future!
! 'Just before 00Z' means after 22Z the previous day. It used to
! be 23:30Z but was changed to avoid rejection of 00Z ATAF data.
!
ELSE IF (DATIME(3) > NOW(6)) THEN
  IF (.NOT.(DATIME(3) == NOW(6)+1.AND.YYGGGG(3:6) <= '0010' &
             .AND. NOW(5) >= 22)) THEN
    DATIME(2)=DATIME(2)-1                ! LAST MONTH'S DATA
    IF (DATIME(2) == 0) THEN
      DATIME(2)=12                       ! DECEMBER
      DATIME(1)=DATIME(1)-1              ! LAST YEAR
    END IF
  END IF

! The check below is for METARs only (GGGG=YYGGGG(3:6) for TAFs).
! If the report hour is greater than the bulletin hour, it might be
! yesterday's data (or it might be a mistake!): say yesterday if 12
! hours or less between report & bulletin, where 12 is arbitrary
! (most METARs are received within a few hours, METARs from 24
! hours ago are very unlikely - beware of bulletin time a few
! minutes before report time!)

ELSE IF (GGGG(1:2) > YYGGGG(3:4)) THEN
  READ (YYGGGG(3:4),'(I2)') BULHOUR
  IF (24-DATIME(4)+BULHOUR < 12) THEN
    CALL DATE31(DATIME(3),NOW(7),NOW(8),NCENDY)
    CALL DATE13(NCENDY-1,DATIME(3),DATIME(2),DATIME(1))
  ELSE
    PRINT *,'TAFIND: METAR time wrong?:',GGGG,' ',YYGGGG,' ', &
            BULL(RSTART:MIN(RSTART+80,BEND))
    OERR=.TRUE.
    GO TO 999
  END IF
END IF IFLABEL3

! Finally reject any METAR more than twenty minutes in the future.
! (only if it's data for today!)
! (Assume SPECIs don't come in before the nominal time.)

IF (TTAAII(1:2) == 'SA' .AND. DATIME(3) == NOW(6) .AND.   &
    DATIME(4)*60+DATIME(5)-(NOW(5)*60+NOW(4)) > 20) THEN
  PRINT *,'TAFIND: METAR in future ',GGGG,' ',YYGGGG,' ', &
            BULL(RSTART:MIN(RSTART+80,BEND))
  OERR=.TRUE.
  GO TO 999
END IF

IF (TTAAII(1:2) == 'SA') THEN
  ENTRY(17:17)=CHAR(0)                         ! METAR
ELSE IF (TTAAII(1:2) == 'FT') THEN
  ENTRY(17:17)=CHAR(1)                         ! LONG TAF
ELSE IF (TTAAII(1:2) == 'FC') THEN
  ENTRY(17:17)=CHAR(2)                         ! SHORT TAF
ELSE IF (TTAAII(1:2) == 'SP') THEN
  ENTRY(17:17)=CHAR(3)                         ! SPECI
END IF
!
ENTRY(3:11)=TTAAII(3:6)//CHAR(NAMD*16+NCOR)//CCCC
CALL TAFREP(DATIME,ENTRY,BULL(RSTART:REND),NFT,BLKSIZ, &
                         BULL(RSTART:RSTART+4))
!
  999 POINT=REND+1
RETURN
END SUBROUTINE TAFIND
