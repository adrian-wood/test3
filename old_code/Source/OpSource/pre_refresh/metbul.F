      SUBROUTINE METBUL(POINT,BEND,TTAAII,CCCC,YYGGGG,OAMD,AMDNUM,OCOR,
     &                  CORNUM,NFT,OERR,BULL)

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
! PARAMETERS    : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) BEND     NUMBER OF LAST CHARACTER IN BULLETIN
!                 (3) TTAAII   TT BULLETIN TYPE (SA METAR, SP SPECI)
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN
!                 (5) YYGGGG   TIME OF BULLETIN (will be replaced by
!                                YYGGGG in report if there is one) !1.4
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
! $Revision: 1$
! $Date: 30/01/2006 20:23:21$
! $Source: /home/us0400/mdb/op/lib/source/RCS/metbul.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:21    Sheila Needham  
! $
! Revision 2.2  2001/10/03 15:07:11  usmdb
! Change diagnostic WRITE statement to output message from the
! right start position - S.Cox
!
! Revision 2.1  2001/09/05 09:01:32  usmdb
! Check added to prevent out of bounds error on HP - S.Cox
!
! Revision 2.0  2001/07/03 10:43:37  usmdb
! Separated variable declaration and initialisation. Added copyright
! and modified header - S.Cox
!
! Revision 1.6  2001/04/09  13:02:41  13:02:41  usmdb (Generic MetDB account)
! Changed pre-processor statement to if defined (1 ) else... S.Cox
!
! Revision 1.5  2001/02/06  11:52:04  11:52:04  usmdb (Generic MetDB account)
! Change day 19FEB01    R Hirst
! Enable handling of UK SPECIs with non-standard
! headings (SPEC B and SPEC M).
!
! Revision 1.4  99/01/14  13:56:24  13:56:24  usmdb (Generic MDB account
! 18 Jan 1999    C Long
! 1.4 If report contains YYGGGG, replace bulletin YYGGGG by it.
!
! Revision 1.3  97/07/31  09:30:02  09:30:02  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.2  1997/07/04 14:55:08  uspm
! Replace variable CHAR by variable ICHAR
!
! Revision 1.1  1997/07/04 12:57:45  uspm
! Initial revision
!
! 03/06/96 (A.M) REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY NEW
!                PARAMETER 'BULL'.
!
!  JAN 96 : BULLETIN TIME WAS BEING OVERWRITTEN BY REPORT TIME AND
!           NOT BEING RESET TO ORIGINAL TIME CAUSING STORAGE FAILURE
!           IF A REPORT WITH AN OLD OR INVALID YYGGGG GROUP WAS STORED
!           ADD CODE TO RESET YYGGGG TO ORIGINAL BULLETIN TIME BEFORE
!           EACH REPORT IS STORED.                             !A
!
!  NOV 95 : ADD CHECK FOR 'AUTO' AND ADDITION OF DAY IN YYGGGG GROUPS
!           TO ACCOMODATE CODE CHANGES VALID JAN 1996 ONWARDS.
!           UPDATE DOCUMENTATION AND FORMAT TO CURRENT STANDARDS.
!           MODIFY CHARACTER CHECK FOR LINE FEEDS, CARRIAGE RETURNS
!           AND SPACES TO INCLUDE '=', PREVENTING NEEDLESS LOOPS TO
!           MOVE POINTER PAST RSTART POSITION RETURNED BY TAFIND.
!           ADD CODE TO INSERT AN END OF REPORT '=' BETWEEN TWO
!           REPORTS NOT DELIMITED. SEARCHES FOR DUPLICATE IDENTIFIER
!           REGIONS WITHIN THE CURRENT POSITION AND NEAREST REPORT END
!
!  JUN 94 : ALLOW 'METAR' ON SECOND LINE TO BE FOLLOWED BY TIME
!
!  JUL 93 : ALLOW FOR Z ON END OF TIME (INCREMENT POINTER BY 1 MORE)
!
!  AUG 92 : MADE FROM METPRO
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! Declare variables

      CHARACTER*(*) YYGGGG       ! Day and time of bulletin
      CHARACTER*(*) TTAAII       ! Bulletin identifier.
      CHARACTER*(*) CCCC         ! Bulletin originating centre.
      CHARACTER*(*) BULL         ! Bulletin of reports to be stored.
      CHARACTER*6   BULTIME      ! Copy of YYGGGG.
      CHARACTER*4   GGGG         ! Time of report validity.
      CHARACTER*(*) AMDNUM       ! Amendment number.
      CHARACTER*(*) CORNUM       ! Correction number.
      CHARACTER*1   CR           ! Carriage return.
      CHARACTER*1   LF           ! Line feed.
      CHARACTER*1   SPACE        ! Space.
      CHARACTER*4   ICAO         ! Identifier (letters only,no Austrian)
      CHARACTER*132 HEAD         ! Revision information
      CHARACTER*5   METSPEC      ! Set to 'METAR' or 'SPECI'        !1.5
      INTEGER NOEND              ! Position of first two letters of an
                                 ! identifier following a previous
                                 ! report without an end of report '='
                                 ! separator.
      INTEGER NFT                ! File allocation number.
      INTEGER NUM                ! Position within a group of first
                                 ! non-numeric character.
      INTEGER ICHAR              ! Position within a group of first
                                 ! non-letter character.
      INTEGER QUAL               ! Position of end of bad METAR report.
      INTEGER ICC                ! Number of letters in identifier.
      INTEGER BEND               ! Bulletin end.
      INTEGER POINT              ! Current point within report.
      INTEGER RSTART             ! Start position within bulletin of
                                 ! report.
      INTEGER BLKSIZ             ! Blocksize used for storage.      !2.0

      LOGICAL OAMD               ! Flag set if bulletin is an amendment.
      LOGICAL OCOR               ! Flag set if bulletin is a correction.
      LOGICAL OERR               ! Set if report invalid.
      LOGICAL OFIRST             ! Prevents checking old reports if
                                 ! first in bulletin is 'NIL'.
      LOGICAL OCHAR              ! Set if all characters are letter.
      LOGICAL OLFCR              ! Set if character is either CR or LF.
      LOGICAL ONUM               ! Set if all characters are numeric.
      LOGICAL OSPACE             ! Set if first non-numeric or
                                 ! letter character is a space.

! Initialise variables

      DATA BLKSIZ/23476/         ! Blocksize used for storage.      !2.0
      DATA SPACE/' '/            ! Space                                
#if defined (MVS)
      DATA CR/Z15/               ! Hexadecimal value for carriage return
      DATA LF/Z25/               ! Hexadecimal value for line feed.     
#else
      CR=CHAR(13)
      LF=CHAR(10)
#endif

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/metbul.F,v $
     &'//'$ $Date: 30/01/2006 20:23:21$ $Revision: 1$'

      OFIRST=.TRUE.              ! Set first report flag prior to
                                 ! reading each bulletin.
      BULTIME=YYGGGG             ! Keep original bulletin time.

      IF (TTAAII(1:2) .EQ. 'SA') THEN                               !1.5
         METSPEC='METAR'                                            !1.5
      ELSE                                                          !1.5
         METSPEC='SPECI'                                            !1.5
      ENDIF                                                         !1.5

! Re-initialise variables before each report is stored.

10    GGGG=' '                   ! Null report time hours/minutes
      OERR=.FALSE.               ! Assume no errors (yet!)
      ICC=0                      ! Set length of identifier to nil.
      ICAO=' '                   ! Set letter identifier to nil.
      YYGGGG=BULTIME             ! Reset YYGGGG to original bulletin !A
                                 ! time in case it has been changed. !A

! Check current point is not at or beyond end of bulletin.

 20   IF (POINT .GE. BEND) THEN
        RETURN
      ENDIF

! Bypass carriage returns, line feeds, spaces, solidii or end of report
! characters.

      IF (BULL(POINT:POINT) .EQ. CR .OR.
     &    BULL(POINT:POINT) .EQ. LF .OR.
     &    BULL(POINT:POINT) .EQ. '=' .OR.
     &    BULL(POINT:POINT) .EQ. '/' .OR.
     &    BULL(POINT:POINT) .EQ. SPACE) THEN
        POINT=POINT+1
        GOTO 20
      ENDIF

!                       Look for identifier
! 1/ Find how many letters in the first group. If 4, assume identifier
!    (see if identifier is repeated: if so, move pointer past first)
!    unless first four are 'SPEC'.                                  !1.5
! 2/ Check second group against multiple possibilities. It could be a
!    misplaced 'AUTO' group ahead of a YYGGgg group. It could be a
!    YYGGgg group or it may be a wind group.

      CALL ACHTST(POINT,7,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
      IF (ICHAR .EQ. 5 .AND. OSPACE) THEN

! Found 4 letters followed by a space.
! If first six are 'SPEC B' or 'SPEC M', it's a UK SPECI;           !1.5
! move forward and continue checks.                                 !1.5

        IF (BULL(POINT:POINT+5) .EQ. 'SPEC B' .OR.                  !1.5
     &      BULL(POINT:POINT+5) .EQ. 'SPEC M') POINT=POINT+7        !1.5

! Check for repetition of identifier.

        IF (BULL(POINT:POINT+4) .EQ. BULL(POINT+5:POINT+9)) THEN
          POINT=POINT+5
        ENDIF

! If no repetition, check group is not 'AUTO', and if not, accept
! group as ICAO identifier. Set report start point and length of id.
! The pointer will be moved to the start of the next group once it is
! established the report is not a 'NIL' report.

        IF (BULL(POINT:POINT+3) .NE. 'AUTO') THEN
          ICC=4
          RSTART=POINT
          ICAO=BULL(RSTART:RSTART+3)
        ENDIF
! Check whether group after identifier is 'NIL'. If not, the report
! contains data so move the pointer to the start of the second group.
! The group may be a YYGGgg group, a wind group or a wrongly placed
! 'AUTO' group.
! Check whether the group is 'AUTO' and skip it, if it is.

        IF (BULL(POINT+5:POINT+7) .NE. 'NIL') THEN
          POINT=POINT+5
          IF (BULL(POINT:POINT+3) .EQ. 'AUTO') THEN
            POINT=POINT+5
          ENDIF

! Check whether it is the optional day/time group (YY)GGgg.
! The (YY)GGgg group may contain 4 or 6 numerics and should end with a
! 'Z', however a space is allowed.
! If there is a YY, replace the bulletin YYGGGG by this group       !1.4
! In any case the GGgg of the report, if available, is used for the
! index. (see TAFIND).
! Move the pointer past the (YY)GGgg group.

          CALL NCHTST(POINT,7,ONUM,NUM,OSPACE,OLFCR,BULL)
          IF ((NUM .EQ. 5 .OR. NUM .EQ. 7) .AND.
     &        (OSPACE .OR. BULL(POINT+NUM-1:POINT+NUM-1) .EQ. 'Z')) THEN
            IF (NUM.EQ.7) YYGGGG=BULL(POINT:POINT+5)                !1.4
            GGGG(1:4)=BULL(POINT+(NUM-5):POINT+3+(NUM-5))
            POINT=POINT+NUM
            IF (BULL(POINT:POINT) .EQ. ' ') THEN
              POINT=POINT+1
            ENDIF
          ENDIF

! Occasionally the reports may contain additional information which
! is usually found in the bulletin header. This section caters for that
! event.
! If the group consists of 'Cxx or 'Rxx (where xx are differing
! possibilities for a correction or retard) the group is skipped.
! If the group consists of 'NIL' the error flag is set and the report
! skipped.

          CALL ACHTST(POINT,4,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
          IF (ICHAR .EQ. 4 .AND. OSPACE) THEN
            IF (BULL(POINT:POINT) .EQ. 'C') THEN
              POINT=POINT+4
            ELSEIF (BULL(POINT:POINT) .EQ. 'R') THEN
              POINT=POINT+4
            ELSEIF (BULL(POINT:POINT+2) .EQ. 'NIL') THEN
              OERR=.TRUE.
            ENDIF
          ENDIF

! Lastly check whether the optional 'AUTO' group is in the correct place

          IF (BULL(POINT:POINT+3) .EQ. 'AUTO') THEN
            POINT=POINT+5
          ENDIF

! See if the next group is the wind group (dddff or variation).
! If wind group is neither VRBXX or /////, check for 4 figures,
! ignoring any "E" or "W" at start.
! (NOTE: A check is made on 4 figures because the wind group may contain
!  gust data changing the format to ddffGff)

          IF (BULL(POINT:POINT+2) .NE. 'VRB' .AND.
     &        BULL(POINT:POINT+4) .NE. '/////') THEN
            IF (BULL(POINT:POINT) .EQ. 'E') THEN
              POINT=POINT+1
            ELSEIF (BULL(POINT:POINT) .EQ. 'W') THEN
              POINT=POINT+1
            ENDIF
            CALL NCHTST(POINT,4,ONUM,NUM,OSPACE,OLFCR,BULL)
            IF (.NOT.ONUM) THEN
              OERR=.TRUE.
            ENDIF
          ENDIF

! If the second group is 'NIL' then move POINT past both first and
! second groups and search for a new identifier.

        ELSEIF (BULL(POINT+5:POINT+7) .EQ. 'NIL') THEN
          POINT=POINT+9
          GOTO 10
        ENDIF

! If the group consists of 5 letters and no space then there is
! something wrong with it !

      ELSEIF (ICHAR .EQ. 5) THEN
        OERR=.TRUE.

! If the group consists of 5 letters (METAR or SPECI) then skip
! it and check whether a time group follows. If it does, skip it as well
! and start afresh at the beginning of the next group.
! If the 5 letters are any combination other than METAR or SPECI then
! something is wrong.

      ELSEIF (ICHAR .EQ. 6) THEN
        IF (BULL(POINT:POINT+4) .EQ. 'METAR' .OR.
     &      BULL(POINT:POINT+4) .EQ. 'SPECI') THEN
          POINT=POINT+6
          CALL NCHTST(POINT,7,ONUM,NUM,OSPACE,OLFCR,BULL)
          IF ((NUM .EQ. 5 .OR. NUM .EQ. 7) .AND. (OSPACE .OR.
     &        BULL(POINT+NUM-1:POINT+NUM-1) .EQ. 'Z')) THEN
            POINT=POINT+NUM+1
          ENDIF
          GO TO 20
        ELSE
          OERR=.TRUE.
        ENDIF

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

      ELSEIF (ICHAR .EQ. 4) THEN
        IF (BULL(POINT:POINT+2) .EQ. 'NIL' .AND. OFIRST) THEN
          IF (BULL(POINT+3:POINT+3) .EQ. '=' .OR. OLFCR .OR. OSPACE)THEN
            RETURN
          ENDIF
        ELSEIF (OSPACE) THEN
          IF (BULL(POINT:POINT) .EQ. 'R' .OR.
     &        BULL(POINT:POINT) .EQ. 'C') THEN
            POINT=POINT+4
            GO TO 20
          ELSE
            ICC=3
            RSTART=POINT
            ICAO(2:4)=BULL(RSTART:RSTART+2)
          ENDIF
        ENDIF

! If group consists of less than 3 letters, try figures.
! If 4 or 6 figures followed by 4 letters, assume time first & skip it.
! Keep the report (YY)GGgg details for indexing. (refer to documentation
! above for more details).
! Set the report start and accept the 4 letter group as the identifier.

      ELSE
        CALL NCHTST(POINT,7,ONUM,NUM,OSPACE,OLFCR,BULL)
        IF ((NUM .EQ. 5 .OR. NUM .EQ. 7) .AND. (OSPACE .OR.
     &      BULL(POINT+NUM-1:POINT+NUM-1) .EQ. 'Z')) THEN
          CALL ACHTST(POINT+NUM,1+NUM,OCHAR,ICHAR,OSPACE,OLFCR,BULL)
          IF (ICHAR .EQ. NUM .AND. OSPACE) THEN
            IF (NUM .EQ. 7 .AND. BULL(POINT:POINT+1) .NE.
     &          BULTIME(1:2)) THEN
              YYGGGG(1:2)=BULL(POINT:POINT+1)
            ENDIF
            GGGG(1:4)=BULL(POINT+(NUM-5):POINT+3+(NUM-5))
            ICC=4
            RSTART=POINT+NUM
            ICAO=BULL(RSTART:RSTART+3)
          ENDIF

! If the group consists of 5 figures and a space, check the first two
! charcaters. If they match '11' then assume an Austrian identifier and
! Set the identifier length and report start point.

        ELSEIF (NUM .EQ. 6 .AND. OSPACE .AND.
     &          BULL(POINT:POINT+1) .EQ. '11') THEN
          ICC=5
          RSTART=POINT
        ENDIF
      ENDIF

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

      IF (ICC .EQ. 3) THEN
        IF (CCCC(1:1) .EQ. 'C' .OR. CCCC(1:1) .EQ. 'K' .OR.
     &      ICAO(2:3) .EQ. CCCC(2:3)) THEN
          RSTART=RSTART-1
          BULL(RSTART:RSTART)=CCCC(1:1)
          ICC=4
        ELSEIF (CCCC .EQ. 'EGWR') THEN
          RSTART=RSTART-1
          IF (ICAO(3:3) .EQ. 'Y') THEN
            BULL(RSTART:RSTART)='C'
            ICC=4
          ELSE
            BULL(RSTART:RSTART)='K'
            ICC=4
          ENDIF
        ELSE
          WRITE (6,*) ICAO(2:4),' - BUT CCCC IS ',CCCC
          OERR=.TRUE.
        ENDIF
      ENDIF

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

      IF (ICC .GT. 0 .AND. .NOT.OERR) THEN
        QUAL=INDEX(BULL(POINT:BEND),'=')
        NOEND=INDEX(BULL(POINT:POINT+QUAL),ICAO(1:2))
        IF (NOEND .GT. 0 .AND. ICC .NE. 5 .AND.
     &      BULL(POINT+NOEND+3:POINT+NOEND+3) .EQ. ' ') THEN
          BULL(POINT+NOEND-2:POINT+NOEND-2)='='
        ENDIF
        CALL TAFIND(RSTART,BEND,OAMD,OCOR,AMDNUM,CORNUM,TTAAII,
     &              CCCC,YYGGGG,GGGG,ICC,NFT,BLKSIZ,OERR,BULL)

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
        IF (POINT .LT. BEND) THEN
          QUAL=INDEX(BULL(POINT:BEND),'=')
          IF (QUAL .EQ. 0) THEN
            RETURN
          ELSEIF (QUAL .GT. 20) THEN
            IF (QUAL .LT. 120) THEN
              PRINT*,'BAD ',METSPEC,': ',
     &        BULL(POINT:MIN(POINT+QUAL,BEND))                     !2.2
            ELSE
              PRINT*,'BAD ',METSPEC,': ',
     &        BULL(POINT:MIN(POINT+120,BEND))                      !2.2
            ENDIF
          ENDIF
          POINT=POINT+QUAL
        ENDIF
      ENDIF

! See if more data in bulletin, if so go back to start.

      IF (BEND-POINT .GT. 10) THEN
        GOTO 10
      ENDIF

      RETURN
      END
