SUBROUTINE METBUL(POINT,BEND,TTAAII,CCCC,YYGGGG,OAMD,AMDNUM,OCOR, &
                  CORNUM,NFT,OERR,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : METBUL
!
! PURPOSE       : TO FIND THE START OF METAR OR SPECI
!                                            REPORTS IN A BULLETIN
!                 AND SEARCH FOR IDENTIFER AND TIME GROUPS IN ORDER
!                 TO CONSTRUCT AN INDEX TO ALLOW STORAGE OF A REPORT.
!                 IF THE REPORT CONTAINS A VALID YYGGGG GROUP THE
!                 INDEX IS CONSTRUCTED USING THE DATE/TIME IN IT.
!                 OTHERWISE THE REPORT IS INDEXED USING THE BULLETIN
!                 DATE/TIME.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : ACHTST, NCHTST, TAFIND
!
! ARGUMENTS     : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) BEND     NUMBER OF LAST CHARACTER IN BULLETIN
!                 (3) TTAAII   TT BULLETIN TYPE (SA METAR, SP SPECI)
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (5) YYGGGG   TIME OF BULLETIN (will be replaced by
!                                YYGGGG in report if there is one)
!                 (6) OAMD     LOGICAL SET IF REPORT AMENDED
!                 (7) AMDNUM   NUMBER OF AMENDMENT
!                 (8) OCOR     LOGICAL SET IF REPORT CORRECTED
!                 (9) CORNUM   NUMBER OF CORRECTION
!                (10) NFT      FT NUMBER FOR METAR STORAGE
!                (11) OERR     LOGICAL SET IF REPORT SPLITTING ERROR
!                              (MAY HAVE BEEN SET BY AMDCOR ON ENTRY)
!                (12) BULL     REPORT DATA
!
! REVISION INFO :
!
! $Workfile: metbul.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 20/12/2010 12:24:31$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         20/12/2010 12:24:31    Stan Kellett
!       declaration of CR and LF changed for MVS
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

USE achtst_mod
USE nchtst_mod
USE tafind_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,          INTENT(INOUT) :: POINT !A1 Current point within repor t.
INTEGER,          INTENT(INOUT) :: BEND !A2 Bulletin end.
CHARACTER(LEN=*), INTENT(IN)    :: TTAAII !A3 Bulletin identifier.
CHARACTER(LEN=*), INTENT(IN)    :: CCCC !A4 Bulletin originating cen    tre.
CHARACTER(LEN=*), INTENT(INOUT) :: YYGGGG !A5 Day and time of bullet    in
LOGICAL,          INTENT(IN)    :: OAMD !A6 Flag set if bulletin is an  amendment.
CHARACTER(LEN=*), INTENT(IN)    :: AMDNUM !A7 Amendment number.
LOGICAL,          INTENT(IN)    :: OCOR !A8 Flag set if bulletin is a c orrection.
CHARACTER(LEN=*), INTENT(IN)    :: CORNUM !A9 Correction number.
INTEGER,          INTENT(INOUT) :: NFT !A10 File allocation number.
LOGICAL,          INTENT(OUT)   :: OERR !A11 Set if report invalid.
CHARACTER(LEN=*), INTENT(INOUT) :: BULL !A12 Bulletin of reports to     be stored.

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

! Declare variables

CHARACTER(LEN=6) ::  BULTIME ! Copy of YYGGGG.
CHARACTER(LEN=1) ::  CR      ! Carriage return.
CHARACTER(LEN=4) ::  GGGG    ! Time of report validity.
CHARACTER(LEN=4) ::  ICAO    ! Identifier (letters only,no Austrian)
CHARACTER(LEN=1) ::  LF      ! Line feed.
CHARACTER(LEN=5) ::  METSPEC ! Set to 'METAR' or 'SPECI'
CHARACTER(LEN=1) ::  SPACE=' ' ! Space.

INTEGER          ::  BLKSIZ=23476 ! Blocksize used for storage.
INTEGER          ::  ICC    ! Number of letters in identifier.
INTEGER          ::  ICHAR  ! Position within a group of first
                            ! non-letter character.
INTEGER          ::  NOEND  ! Position of first two letters of an
                            ! identifier following a previous
                            ! report without an end of report '='
                            ! separator.
INTEGER          ::  NUM    ! Position within a group of first
                            ! non-numeric character.
INTEGER          ::  QUAL   ! Position of end of bad METAR report.
INTEGER          ::  RSTART ! Start position within bulletin of
                            ! report.

LOGICAL          ::  OCHAR  ! Set if all characters are letter.
LOGICAL          ::  OFIRST ! Prevents checking old reports if
                            ! first in bulletin is 'NIL'.
LOGICAL          ::  OLFCR  ! Set if character is either CR or LF.
LOGICAL          ::  ONUM   ! Set if all characters are numeric.
LOGICAL          ::  OSPACE ! Set if first non-numeric or
                            ! letter character is a space.

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! Initialise variables

#if defined (MVS)
CR = CHAR(21)              ! Hexadecimal value for carriage return
LF = CHAR(37)              ! Hexadecimal value for line feed.
#else
CR=CHAR(13)
LF=CHAR(10)
#endif

OFIRST=.TRUE.              ! Set first report flag prior to
                           ! reading each bulletin.
BULTIME=YYGGGG             ! Keep original bulletin time.

IF (TTAAII(1:2)  ==  'SA') THEN
   METSPEC='METAR'
ELSE
   METSPEC='SPECI'
END IF

! Re-initialise variables before each report is stored.

10    GGGG=' '                   ! Null report time hours/minutes
OERR=.FALSE.               ! Assume no errors (yet!)
ICC=0                      ! Set length of identifier to nil.
ICAO=' '                   ! Set letter identifier to nil.
YYGGGG=BULTIME             ! Reset YYGGGG to original bulletin
                           ! time in case it has been changed.

! Check current point is not at or beyond end of bulletin.

20 CONTINUE
IF (POINT  >=  BEND) THEN
  RETURN
END IF

! Bypass carriage returns, line feeds, spaces, solidii or end of report
! characters.

IF (BULL(POINT:POINT)  ==  CR .OR. &
    BULL(POINT:POINT)  ==  LF .OR. &
    BULL(POINT:POINT)  ==  '=' .OR. &
    BULL(POINT:POINT)  ==  '/' .OR. &
    BULL(POINT:POINT)  ==  SPACE) THEN
  POINT=POINT+1
  GOTO 20
END IF

!                       Look for identifier
! 1/ Find how many letters in the first group. If 4, assume identifier
!    (see if identifier is repeated: if so, move pointer past first)
!    unless first four are 'SPEC'.
! 2/ Check second group against multiple possibilities. It could be a
!    misplaced 'AUTO' group ahead of a YYGGgg group. It could be a
!    YYGGgg group or it may be a wind group.

CALL ACHTST(POINT,7,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
IFLABEL1: &
IF (ICHAR  ==  5 .AND. OSPACE) THEN

! Found 4 letters followed by a space.
! If first six are 'SPEC B' or 'SPEC M', it's a UK SPECI;
! move forward and continue checks.

  IF (BULL(POINT:POINT+5)  ==  'SPEC B' .OR.  &
      BULL(POINT:POINT+5)  ==  'SPEC M') POINT=POINT+7

! Check for repetition of identifier.

  IF (BULL(POINT:POINT+4)  ==  BULL(POINT+5:POINT+9)) THEN
    POINT=POINT+5
  END IF

! If no repetition, check group is not 'AUTO', and if not, accept
! group as ICAO identifier. Set report start point and length of id.
! The pointer will be moved to the start of the next group once it is
! established the report is not a 'NIL' report.

  IF (BULL(POINT:POINT+3)  /=  'AUTO') THEN
    ICC=4
    RSTART=POINT
    ICAO=BULL(RSTART:RSTART+3)
  END IF
! Check whether group after identifier is 'NIL'. If not, the report
! contains data so move the pointer to the start of the second group.
! The group may be a YYGGgg group, a wind group or a wrongly placed
! 'AUTO' group.
! Check whether the group is 'AUTO' and skip it, if it is.

IFLABEL2: &
  IF (BULL(POINT+5:POINT+7)  /=  'NIL') THEN
    POINT=POINT+5
    IF (BULL(POINT:POINT+3)  ==  'AUTO') THEN
      POINT=POINT+5
    END IF

! Check whether it is the optional day/time group (YY)GGgg.
! The (YY)GGgg group may contain 4 or 6 numerics and should end with a
! 'Z', however a space is allowed.
! If there is a YY, replace the bulletin YYGGGG by this group
! In any case the GGgg of the report, if available, is used for the
! index. (see TAFIND).
! Move the pointer past the (YY)GGgg group.

    CALL NCHTST(POINT,7,ONUM,NUM,OSPACE,OLFCR,BULL)
    IF ((NUM  ==  5 .OR. NUM  ==  7) .AND. &
        (OSPACE .OR. BULL(POINT+NUM-1:POINT+NUM-1)  ==  'Z')) THEN
      IF (NUM == 7) YYGGGG=BULL(POINT:POINT+5)
      GGGG(1:4)=BULL(POINT+(NUM-5):POINT+3+(NUM-5))
      POINT=POINT+NUM
      IF (BULL(POINT:POINT)  ==  ' ') THEN
        POINT=POINT+1
      END IF
    END IF

! Occasionally the reports may contain additional information which
! is usually found in the bulletin header. This section caters for that
! event.
! If the group consists of 'Cxx or 'Rxx (where xx are differing
! possibilities for a correction or retard) the group is skipped.
! If the group consists of 'NIL' the error flag is set and the report
! skipped.

    CALL ACHTST(POINT,4,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
    IF (ICHAR  ==  4 .AND. OSPACE) THEN
      IF (BULL(POINT:POINT)  ==  'C') THEN
        POINT=POINT+4
      ELSE IF (BULL(POINT:POINT) ==  'R') THEN
        POINT=POINT+4
      ELSE IF (BULL(POINT:POINT+2) ==  'NIL') THEN
        OERR=.TRUE.
      END IF
    END IF

! Lastly check whether the optional 'AUTO' group is in the correct place

    IF (BULL(POINT:POINT+3)  ==  'AUTO') THEN
      POINT=POINT+5
    END IF

! See if the next group is the wind group (dddff or variation).
! If wind group is neither VRBXX or /////, check for 4 figures,
! ignoring any "E" or "W" at start.
! (NOTE: A check is made on 4 figures because the wind group may contain
!  gust data changing the format to ddffGff)

IFLABEL3: &
    IF (BULL(POINT:POINT+2)  /=  'VRB' .AND. &
        BULL(POINT:POINT+4)  /=  '/////') THEN
      IF (BULL(POINT:POINT)  ==  'E') THEN
        POINT=POINT+1
      ELSE IF (BULL(POINT:POINT) ==  'W') THEN
        POINT=POINT+1
      END IF
      CALL NCHTST(POINT,4,ONUM,NUM,OSPACE,OLFCR,BULL)
      IF (.NOT.ONUM) THEN
        OERR=.TRUE.
      END IF
    END IF IFLABEL3

! If the second group is 'NIL' then move POINT past both first and
! second groups and search for a new identifier.

  ELSE IF (BULL(POINT+5:POINT+7) ==  'NIL') THEN
    POINT=POINT+9
    GOTO 10
  END IF IFLABEL2

! If the group consists of 5 letters and no space then there is
! something wrong with it !

ELSE IF (ICHAR ==  5) THEN
  OERR=.TRUE.

! If the group consists of 5 letters (METAR or SPECI) then skip
! it and check whether a time group follows. If it does, skip it as well
! and start afresh at the beginning of the next group.
! If the 5 letters are any combination other than METAR or SPECI then
! something is wrong.

ELSE IF (ICHAR ==  6) THEN
IFLABEL4: &
  IF (BULL(POINT:POINT+4)  ==  'METAR' .OR. &
      BULL(POINT:POINT+4)  ==  'SPECI') THEN
    POINT=POINT+6
    CALL NCHTST(POINT,7,ONUM,NUM,OSPACE,OLFCR,BULL)
    IF ((NUM  ==  5 .OR. NUM  ==  7) .AND. (OSPACE .OR. &
        BULL(POINT+NUM-1:POINT+NUM-1)  ==  'Z')) THEN
      POINT=POINT+NUM+1
    END IF
    GO TO 20
  ELSE
    OERR=.TRUE.
  END IF IFLABEL4

! If the group consists of 3 letters then it could contain a 3 letter
! identifier. It may not however !
! First check the report is not 'NIL'. If it is, check whether it is
! the first report using flag OFIRST. If it is the first report then
! check the non-letter is either an '=', a carriage return, a line feed
! or a space. If one of these is true then return to get a new bulletin
! because this one will not contain any further data.
! If not, check the group is not a 'Rxx or 'Cxx group (retard or
! correction). Skip the group if it is and start afresh.
! Otherwise assume it to be a 3 letter identifier and set the start
! point of the report.

ELSE IF (ICHAR ==  4) THEN
IFLABEL5: &
  IF (BULL(POINT:POINT+2)  ==  'NIL' .AND. OFIRST) THEN
    IF (BULL(POINT+3:POINT+3)  ==  '=' .OR. OLFCR .OR. OSPACE)THEN
      RETURN
    END IF
  ELSE IF (OSPACE) THEN
    IF (BULL(POINT:POINT)  ==  'R' .OR. &
        BULL(POINT:POINT)  ==  'C') THEN
      POINT=POINT+4
      GO TO 20
    ELSE
      ICC=3
      RSTART=POINT
      ICAO(2:4)=BULL(RSTART:RSTART+2)
    END IF
  END IF IFLABEL5

! If group consists of less than 3 letters, try figures.
! If 4 or 6 figures followed by 4 letters, assume time first & skip it.
! Keep the report (YY)GGgg details for indexing. (refer to documentation
! above for more details).
! Set the report start and accept the 4 letter group as the identifier.

ELSE
  CALL NCHTST(POINT,7,ONUM,NUM,OSPACE,OLFCR,BULL)
IFLABEL6: &
  IF ((NUM  ==  5 .OR. NUM  ==  7) .AND. (OSPACE .OR. &
      BULL(POINT+NUM-1:POINT+NUM-1)  ==  'Z')) THEN
    CALL ACHTST(POINT+NUM,1+NUM,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
    IF (ICHAR  ==  NUM .AND. OSPACE) THEN
      IF (NUM  ==  7 .AND. BULL(POINT:POINT+1)  /=  &
          BULTIME(1:2)) THEN
        YYGGGG(1:2)=BULL(POINT:POINT+1)
      END IF
      GGGG(1:4)=BULL(POINT+(NUM-5):POINT+3+(NUM-5))
      ICC=4
      RSTART=POINT+NUM
      ICAO=BULL(RSTART:RSTART+3)
    END IF

! If the group consists of 5 figures and a space, check the first two
! charcaters. If they match '11' then assume an Austrian identifier and
! Set the identifier length and report start point.

  ELSE IF (NUM ==  6 .AND. OSPACE .AND. &
          BULL(POINT:POINT+1)  ==  '11') THEN
    ICC=5
    RSTART=POINT
  END IF IFLABEL6
END IF IFLABEL1

! Some identifers consists of only 3 letters. The first character of the
! originating center, representing the country of origin, is inserted
! in front of the identifier to give a 4 letter ICAO.
! Currently only American and Canadian reports are received in this
! format.
! If the originating center is 'EGWR', a local American center which
! passes data with a UK originating center, add the correct letter to
! the ICAO in the report.
! (This check makes an assumption that all reports whose ICAO 2nd letter
!  is a 'Y' are Canadian )!
! In most cases this is probably true, but it is fallible.

IFLABEL7: &
IF (ICC  ==  3) THEN
IFLABEL8: &
  IF (CCCC(1:1)  ==  'C' .OR. CCCC(1:1)  ==  'K' .OR. &
      ICAO(2:3)  ==  CCCC(2:3)) THEN
    RSTART=RSTART-1
    BULL(RSTART:RSTART)=CCCC(1:1)
    ICC=4
  ELSE IF (CCCC ==  'EGWR') THEN
    RSTART=RSTART-1
    IF (ICAO(3:3)  ==  'Y') THEN
      BULL(RSTART:RSTART)='C'
      ICC=4
    ELSE
      BULL(RSTART:RSTART)='K'
      ICC=4
    END IF
  ELSE
    WRITE (6,*) ICAO(2:4),' - BUT CCCC IS ',CCCC
    OERR=.TRUE.
  END IF IFLABEL8
END IF IFLABEL7

! If the identifier length is not as initialised and the error flag has
! not been set then store the report.
! However,
! some bulletins contain reports without an end of report character,'='.
! In order to prevent storing more than one report as a single report,
! a search is made for an end of report character between the current
! position and the end of the bulletin.
! A second search is then made for a duplicate identifier region between
! these positions. Should a match be found it is assumed that there is
! a missing end of report character and one is added to the end of the
! previous report.
! A check was made prior to the inclusion of this section to ensure that
! a valid ICAO could not be mistaken as METAR content.

IFLABEL9: &
IF (ICC  >  0 .AND. .NOT.OERR) THEN
  QUAL=INDEX(BULL(POINT:BEND),'=')
  NOEND=INDEX(BULL(POINT:POINT+QUAL),ICAO(1:2))
  IF (NOEND  >  0 .AND. ICC   /=  5 .AND. &
      BULL(POINT+NOEND+3:POINT+NOEND+3)  ==  ' ') THEN
    BULL(POINT+NOEND-2:POINT+NOEND-2)='='
  END IF
  CALL TAFIND(RSTART,BEND,OAMD,OCOR,AMDNUM,CORNUM,TTAAII, &
              CCCC,YYGGGG,GGGG,ICC,NFT,BLKSIZ,OERR,BULL)

! Set the OFIRST flag to false to avoid dumping reports within the
! bulletin should the next report encountered be a 'NIL' report.

  OFIRST=.FALSE.
  POINT=RSTART

! If no identifier has been found or the error flag raised, look for
! the end of the report.
! Print out the bad report and move the pointer to the character after
! the '=' ready to check the next report.
! If no '=' is found then reject the bulletin and start afresh with a
! new one. (It would be extremely complicated to find the end of one
! report and the start of the next with a report end symbol).

ELSE
IFLABEL10: &
  IF (POINT  <  BEND) THEN
    QUAL=INDEX(BULL(POINT:BEND),'=')
    IF (QUAL  ==  0) THEN
      RETURN
    ELSE IF (QUAL >  20) THEN
      IF (QUAL  <  120) THEN
        PRINT*,'BAD ',METSPEC,': ', &
        BULL(POINT:MIN(POINT+QUAL,BEND))
      ELSE
        PRINT*,'BAD ',METSPEC,': ', &
        BULL(POINT:MIN(POINT+120,BEND))
      END IF
    END IF
    POINT=POINT+QUAL
  END IF IFLABEL10
END IF IFLABEL9

! See if more data in bulletin, if so go back to start.

IF (BEND-POINT  >  10) THEN
  GOTO 10
END IF

RETURN
END SUBROUTINE METBUL
