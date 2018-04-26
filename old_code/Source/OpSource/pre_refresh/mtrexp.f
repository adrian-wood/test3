      SUBROUTINE MTREXP(REPORT,REPLEN,REXP,CEXP,IFAIL)              !2.0

!-----------------------------------------------------------------------
!
!   PROGRAM      : MTREXP
!
!   PURPOSE      : TO DECODE/EXPAND METAR REPORT DATA INTO AN ARRAY
!                  ALLOWING SPECIFIC ELEMENT RETRIEVAL.
!
!   DESCRIPTION  : THE METAR REPORT CONSISTS OF A SET OF GROUPS. THESE
!                  GROUPS ARE OF NON-UNIFORM LENGTH, SO FIXED CHARACTERS
!                  AND KNOWN GROUP FORMATS ARE USED AS A GUIDELINE TO
!                  ATTEMPT TO ESTABLISH WHICH GROUP IS BEING HANDLED.
!                  PARTS OF THE REPORT ARE NOT EXPANDED, AND SOME GROUPS
!                  ARE 'SPLIT' (IE: THERE ARE SEPARATING SPACES WITHIN
!                  A SINGLE, LARGER, GROUP). OTHER GROUPS IN THE REPORT
!                  ARE TREATED AS A SINGLE BLOCK OF CHARACTERS
!                  (EXCLUDING A SPACE).
!                  THERE IS AN ORDER TO THE GROUPS WITHIN THE REPORT
!                  AND THIS IS REFLECTED IN THE ORDER IN WHICH THE
!                  GROUPS ARE CHECKED.
!                  IN ORDER TO ESTABLISH WHAT PART OF THE REPORT
!                  CONSTITUTES A GROUP, AND THE CONTENT OF THAT GROUP,
!                  TWO ROUTINES ARE USED;
!
!                  MTRLOC (SEARCHES FOR A STRING OF CHARACTERS WITH A
!                         (SPACE AT EACH END)
!                    AND
!                  MTRGRP (EXAMINES THE CONTENTS OF THE GROUP CREATING
!                         (A DUMMY STRING WITH A SYMBOLIC CONTENT OF
!                         (THE GROUP. N'S USED TO REPRESENT NUMERIC
!                         (CHARACTERS AND Y'S TO REPRESENT VALID NON-
!                         (NUMERIC CHARACTERS.)
!
!                  THESE TWO ROUTINES WILL CATER FOR NEARLY ALL GROUPS
!                  WITHIN THE REPORT EXCEPT:
!
!                  WIND SHEAR REPORTS AND VISIBILITIES IN FRACTIONAL
!                  VALUES (USUALLY MEASURED IN STATUE MILES EG: 1 1/2SM)
!
!                  A SPECIFIC CHECK FOR KNOWN CHARACTERS IS MADE PRIOR
!                  TO USING THE TWO ROUTINES TO ESTABLISH GROUP DETAILS.
!                  SHOULD A FORECAST OR TREND OR REMARKS GROUP BE FOUND
!                  NO MORE EXPANSION IS NECESSARY, HOWEVER, UK REPORTS
!                  MAY HAVE A RUNWAY DEPOSIT GROUP APPENDED TO THE END
!                  OF THE REPORT AND A CHECK IS MADE FOR THIS.
!
!                  SOME ELEMENTS DO NOT REQUIRE EXPANSION. THE
!                  INITIALISATION ROUTINE, MTRINT, WILL HAVE ALREADY
!                  EXPANDED THE FOLLOWING ELEMENTS USING THE INDEX.
!
!                  STATION IDENTIFIER, TIME OF OBSERVATION,
!                  ORIGINATING CENTRE, COLLECTING CENTRE,
!                  LATITUDE AND LONGITUDE
!
!                  AN ARRAY IS USED TO 'FLAG' ELEMENTS ALREADY EXPANDED.
!                  THIS ARRAY SERVES NO OTHER PURPOSE.
!
!   CALLED BY    : TFMTRT
!
!   CALLS        : MTRLOC (TO LOCATE A GROUP)
!                  MTRGRP (TO ESTABLISH GROUP CONTENT)
!                  MTRWWR (TO DECODE WEATHER GROUPS)
!                  MTRRWY (TO DECODE RUNWAY VISIBILITY GROUPS)
!                  MTRSHE (TO DEFINE WINDSHEAR GROUP LENGTH)
!                  MTRCCC (TO DECODE CLOUD GROUPS)
!                  MTRDDF (TO DECODE WIND GROUPS)
!
!   PARAMETERS   : 1 REPORT - METAR REPORT TO BE DECODED
!                  2 RLEN   - REPORT LENGTH
!                  3 REXP   - EXPANDED/DECODED VALUES ARRAY
!                  4 CEXP   - EXPANDED/DECODED CHARACTER DATA
!                  5 IFAIL  - ERROR FLAG
!
!
!   (ELEMENT EXPANSION FLAG ARRAY)
!
!    NO.  ELEMENT
!     1   AUTO
!     2   WIND
!     3   VISIBILITY
!     4   VISIBILITY
!     5   CAVOK
!     6   SKC
!     7   NSW
!     8   WEATHER WW
!     9   WEATHER W'W'
!    10   RECENT WEATHER
!    11   VERTICAL VISIBILITY
!    12   PRESSURE
!    13   TEMPERATURE & DEW POINT
!    14   CLOUD
!    15   DEEP CONVECTIVE CLOUD
!    16   RUNWAY VISIBILITY
!
!   (EXPANSION ARRAY ELEMENTS)
!
!    ALL VALUES ARE REAL; WHEN AN ELEMENT IS A CHARACTER (SHOWN BY *)
!     ARRAY MUST HOLD (LENGTH * 65536) + DISPLACEMENT IN CSTR.
!     EG REXP(1)=(65536*4)+1      CSTR(1:4)='EGAA'
!        REXP(2)=(65536*4)+5      CSTR(5:8)='EGGY'
!
!              ICAO IDENTIFIER *                     REXP(1)
!              LATITUDE                              REXP(2)
!              LONGITUDE                             REXP(3)
!              YEAR                                  REXP(4)
!              MONTH                                 REXP(5)
!              DAY                                   REXP(6)
!              HOUR                                  REXP(7)
!              MINUTE                                REXP(8)
!              COLLECTING CENTRE *                   REXP(9)
!              BULLETIN CODE *                       REXP(10)
!              WIND DIRECTION                        REXP(11)
!              WIND SPEED                            REXP(12)
!              MAX GUST                              REXP(13)
!              VARIABLE WIND MAX DIRECTION           REXP(14)
!              VARIABLE WIND MIN DIRECTION           REXP(15)
!              PREVAILING VISIBILITY                 REXP(173) !2.4
!              MIN VIS DIRECTION                     REXP(16)
!              MIN VIS                               REXP(17)
!              MAX VIS DIRECTION                     REXP(18)
!              MAX VIS                               REXP(19)
!              GENERAL WEATHER  (CAVOK,NSW,SKC)      REXP(20)
!
! 2 REPLICATIONS OF
!              RUNWAY ID                             REXP(21/28)
!              RUNWAY PARALLEL ID                    REXP(22/29)
!              RUNWAY VIS TENDENCY                   REXP(23/30)
!              RUNWAY VIS BEYOND INSTRUMENTAL RANGE  REXP(24/31)
!              MIN RUNWAY VIS                        REXP(25/32)
!              RUNWAY VIS BEYOND INSTRUMENTAL RANGE  REXP(26/33)
!              MAX RUNWAY VIS                        REXP(27/34)
!
! 3 REPLICATIONS OF
!               SIG WEATHER INTENSITY                REXP(35/38/41)
!               SIG WEATHER DESCRIPTOR               REXP(36/39/42)
!               SIG WEATHER CODE                     REXP(37/40/43)
!
!               PRESENT WEATHER ID                   REXP(44)
!
! 3 REPLICATIONS OF
!               TOTAL CLOUD AMOUNT                   REXP(45/48/51)
!               CLOUD TYPE                           REXP(46/49/52)
!               CLOUD BASE HEIGHT                    REXP(47/50/53)
!
! 3 REPLICATIONS OF
!               CONVECTIVE CLOUD AMOUNT              REXP(54/57/60)
!               CONVECTIVE CLOUD TYPE                REXP(55/58/61)
!               CONVECTIVE CLOUD BASE HEIGHT         REXP(56/59/62)
!
!               VERTICAL VISIBILITY                  REXP(63)
!               AIR TEMPERATURE                      REXP(64)
!               DEWPOINT                             REXP(65)
!               PRESSURE                             REXP(66)
!               NOT USED (was REWW intensity) !      REXP(67)
!
! 3 REPLICATIONS OF
!               RECENT WEATHER DESCRIPTOR            REXP(68+80/82/84)
!               RECENT WEATHER CODE                  REXP(69+81/83/85)
!
! 2 REPLICATIONS OF
!               RUNWAY NUMBER                        REXP(70/74)
!               PARALLEL RUNWAY                      REXP(71/75)
!               RUNWAY USAGE                         REXP(72/76)
!               NOT USED (was RUNWAY WIND SHEAR)     REXP(73/77)
!
!               RUNWAY STATE *                       REXP(78)
!               AUTOMATIC STATION IDENTIFIER         REXP(79)
!
!Y2K  26.06.1997  MTREXP is Year 2000 compliant.
!
! REVISION INFO :
!
! $Workfile: mtrexp.f$ $Folder: pre_refresh$
! $Revision: 4$ $Date: 18/06/2010 10:23:25$
!
! CHANGE RECORD :
!
! $Log:
!  4    Met_DB_Project 1.3         18/06/2010 10:23:25    Brian Barwell
!       Omitted 'GO TO 1' now added.
!  3    Met_DB_Project 1.2         08/06/2010 11:39:26    Richard Weedon  Elem
!       construct commented out on additional Vis code
!  2    Met_DB_Project 1.1         01/06/2010 14:15:14    Richard Weedon
!       Revisions made to recognise NDV in vis group and NCD / NSC in cloud
!       group.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:33    Sheila Needham  
! $
! Revision 2.4  2004/11/08 11:29:30  usmdb
! Stanley Kellett. 15th November 2004.
! Changes to ICAO code on visibility. Prevailing vis now used but
! requirement to still decode old style obs.
!
! Revision 2.3  2002/10/07  15:49:31  15:49:31  usmdb (MetDB account c/o John C
! Ward)
! 2.3. 21 Oct 2002. Brian Barwell. Change 82/02. Remedy INC043005.
! Rewrite visibility section to decode horizontal visibilities
! expressed in fractions of statute miles.
!
! Revision 2.2  2001/05/09  09:39:05  09:39:05  usmdb (Generic MetDB account)
! 21 May 2001, Stan Kellett, !2.2
!       Comments changed to make easier to understand.
!
! Revision 2.1  2001/04/23  13:24:16  13:24:16  usmdb (Generic MetDB account)
! 23/04/2001; !2.1
! If a numeric group of length 3 is reported a check is made
! to make sure that the pressure is a sensible value.
! If not the value is ignored and pressure stored flag not set.
!                       Stanley Kellett, MetDB team.
!
! Revision 2.0  2001/01/08  11:58:55  11:58:55  usmdb (Generic MetDB account)
! Removed unused argument NCHAR from MTRCCC, MTRDDF and
! MTRRWY calls. Removed unused dummy argument CERR.
! Separated variable declarations and initialisations.
! Added copyright and modified header - S.Cox
!
! Revision 1.7  2000/09/06  10:39:57  10:39:57  usmdb (Generic MDB account)
! 18/9/2000. Revision 1.7.
!       Change to ignore remarks section.
!       Change to decode rest of Metar if more than standard
!       number of groups reported.
!                                          Stanley Kellett
!
! Revision 1.6  2000/03/10  10:11:51  10:11:51  usmdb (Generic MDB account)
! 20/03/2000 Addition of error checking, to check that elements
!            stored, have expected numerical values.
!
! Revision 1.5  99/04/12  11:10:17  11:10:17  usmdb (Generic MDB account)
! 19-04-1999 S.Cox - ref MetDB problem 435
! Addition of a safety check (call to IVALUE rather than internal read) to
! the horizontal visability group character to integer conversion.
! v(G)=98, ev(G)=50
!
! Revision 1.4  98/11/12  14:05:36  14:05:36  usmdb (Generic MDB account)
! 16-11-1998 Clear CHARR by resetting it to blanks. This stops erronous
! decoding of non runway groups such as colour states.
! Jon Lewthwaite
!
! Revision 1.3  98/06/11  13:36:14  13:36:14  usmdb (Generic MDB account)
!  Decode METAR TREND Groups
!
! Revision 1.2  97/08/04  13:15:49  13:15:49  uspm (Pat McCormack)
! First revisioned version for  1  - with Y2K change
!
! Revision 1.1  1997/02/17 11:51:34  uspm
! Initial revision
!
! 03/02/00     Checks put in to make sure that if an integer or
!              real expected from a read, then a Charater has not
!              input by mistake by the observer. If a character
!              is found to be illegal then missing indicator is
!              returned (-9999999.0 for real). S.Kellett
!
! 15/06/98     Decode METAR TREND Groups
!
! 12/04/96     ADDITION OF EXTRA CHECKS FOR FRACTIONAL STATUTE MILE
!              VISIBILITIES AND CHECK TO PREVENT INCORRECT WIND GROUP
!              FROM BEING EXPANDED AS A DIRECTIONAL VISIBILITY.
!
! 28/03/96     CORRECTION TO HEIGHT MULTIPLICATION FACTOR. OBSERVED
!              HEIGHTS ARE REPORTED IN BLOCKS OF 30 METRES. CHANGED
!              VALUE OF METRES MULTIPLICATION VALUE TO REFLECT THIS.
!
! NOV/DEC 95   CHANGES TO ACCOMODATE NEW METAR CODE VALID FROM JAN 1996
!              LED TO A MAJOR OVERHAUL OF WHOLE ROUTINE, INCLUDING;
!
!            1 UPDATE VARIABLE DECLARATION AND INITIALISATION IN LINE
!              WITH LATEST TEAM STANDARDS. ADDITION OF IMPLICIT NONE.
!            3 UPDATE IDENTIFIER RECOGNITION SECTION REMOVING
!              UNNECESSARY CHECKS.
!            4 MODIFY FORECAST RECOGNITION SECTION REMOVING DUPLICATED
!              CODE AND ADDING CHECK FOR REMARKS SECTIONS.
!            5 UPDATING CHECK ON DAY/TIME GROUP TO ONLY COMPARE BLOCK
!              11 REPORT TIMES (OTHERS ARE STORED USING REPORT TIME)
!              REMOVE UNNECESSARY CHECKS AND ALLOW LONGER GROUPS.
!            6 INCLUDE CHECK FOR NEW 'AUTO' GROUP WITH ADDITION OF NEW
!              EXPANSION ELEMENT.
!            7 UPDATE ELEMENT FLAG ARRAY. PREVIOUSLY MANY FLAGS WERE
!              SET BUT NEVER CHECKED, RENDERING FLAG SYSTEM FUTILE.
!              NEW ARRAY ONLY HAS FLAGS ACTUALLY USED.
!            8 CORRECT ERRORS IN VISIBILITY CONVERSIONS AND REMOVE THE
!              NEED FOR MTRVVV.  DD NEW CHECK FOR VISIBILITIES REPORTED
!              IN STATUTE MILES.(NOT CATERED FOR PREVIOUSLY)
!            9 CHANGE AND CORRECT CURRENT AND RECENT WEATHER EXPANSION
!              ROUTINE, REMOVING A LOT OF DUPLICATED CODE.
!           10 CORRECT ERRORS IN HEIGHT CONVERSIONS.
!           11 CORRECT TEMPERATURE GROUP DECODE. PREVIOUSLY A DRY BULB
!              VALUE LOWER THAN THE DEW POINT VALUE WAS ALLOWED !!
!              REMOVED NEED FOR MTRTTD.
!           12 CHANGE CLOUD GROUP EXPANSION, REMOVING THE NEED FOR
!              ROUTINE MTRTCU.
!
!              IN ALL 3 ROUTINES PREVIOUSLY USED, MTRTTD, MTRVVV AND
!              MTRTCU HAVE BEEN MADE REDUNDANT.
!              CHANGES HAVE ALSO BEEN MADE TO MTRSHE, MTRWWR, MTRRWY
!              MTRCCC, MTRDDF AND MTRINT. (MORE DETAILS ARE AVAILABLE
!              IN THE SPECIFIED ROUTINE).
!
! 22/12/92     IMPLEMENTED 12:10PM
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! Declare Character

      CHARACTER CEXP*(*)      ! EXPANDED DATA (CHARACTER)
      CHARACTER CHARR*20      ! TRANSLATED GROUP DATA. NUMERIC CHAR !2.2
                              ! REPRESENTED BY 'N'. OTHERS BY 'Y'. !2.2
      CHARACTER ID*5          ! STATION IDENTIFIER
      CHARACTER REPORT*(*)    ! WHOLE METAR REPORT
      CHARACTER TEXT*4        ! GENERAL VARIABLE USED TO HOLD A PART OF
                              ! THE REPORT IN ORDER TO ALLOW LARGE
                              ! LOGICAL COMPARISONS TO BE MORE READABLE.

!Declare Logical
      LOGICAL LREPFL          ! TRUE INDICATES NO MORE REPORT PROCESSING
      LOGICAL SHBAD           ! WINDSHEAR GROUP SYNTAX ERROR
      LOGICAL WWBAD           ! CRAP SIG WEATHER GROUP
      LOGICAL MoreTrend       ! More Trend info to decode

!Declare Integer
      INTEGER CLDAMT          ! TOTAL CLOUD AMOUNT
      INTEGER TEMP            ! PRECONVERTED TEMPERATURE VALUE
      INTEGER RWYUSE          ! VALUE OF RUNWAY USE (WIND SHEAR GROUPS)
      INTEGER INCHES          ! PRESSURE IN INCHES
      INTEGER VIS             ! VISIBILITY VALUE BEFORE CONVERSION
      INTEGER DIR             ! WIND DIRECTION OF MAX/MIN VISIBILITY
      INTEGER NGRP            ! COUNT OF NUMBER OF EXPANDED GROUPS
      INTEGER SHRGRP          ! NUMBER OF WIND SHEAR GROUPS EXPANDED
      INTEGER DAY             ! VALIDITY DAY OF METAR REPORT
      INTEGER ELEM(16)        ! ELEMENT LIST USED TO EXPAND DATA INTO
                              ! REAL ARRAY
      INTEGER ESTAR           ! STARTING POSITION WITHIN ARRAY ELEM
      INTEGER GRPLEN          ! LENGTH OF GROUP
      INTEGER HRS             ! VALIDITY HOUR OF METAR REPORT
      INTEGER IFAIL           ! ERROR STATUS FLAG
      INTEGER NCHAR           ! NUMBER OF CHARACTERS IN GROUP
      INTEGER POINT1          ! START POSITION OF CURRENT GROUP
      INTEGER POINT2          ! START POSITION OF NEXT GROUP
      INTEGER IPT             ! END POSITION OF CURRENT GROUP       !2.3
      INTEGER I1, I2          ! NUM & DENOM OF FRACTION IN VIS GROUP!2.3
      INTEGER REPLEN          ! METAR REPORT LENGTH
      INTEGER TIME            ! REPORT TIME (EITHER DAY OR HOUR)
      INTEGER METRES          ! HEIGHT MULTIPLICATION FACTOR TO GIVE
                              ! TRUE HEIGHT IN METRES ACCORDING TO
                              ! WMO CODE TABLE 1690.
      INTEGER LOOP            ! GENERAL LOOP VARIABLE
      INTEGER RWYPRL          ! PARALLEL RUNWAY VALUE (WIND SHEAR)
      INTEGER RWYDIR          ! RUNWAY DIRECTION (WIND SHEAR)
      INTEGER CLDTYPE         ! CLOUD TYPE
      INTEGER WW              ! PRESENT WEATHER IDENTIFIER
      INTEGER COUNT
      INTEGER DISP
      INTEGER IVALUE          ! IVALUE FUNCTION                     !1.5

      INTEGER RMKLEN          !1.7 Length of RMK section in REPORT

!Declare Real
      REAL DDD                ! WIND DIRECTION
      REAL RVRDIR             ! RVR GROUP RUNWAY IDENTIFIER
      REAL FFF                ! WIND SPEED
      REAL FMFMFM             ! MAXIMUM GUST
      REAL CLDHT              ! BASE HEIGHT OF CLOUD
      REAL INTOMB             ! INCHES TO MB PRESSURE CONVERSION
      REAL KELVIN             ! COARSE TEMPERATURE CONVERSION VALUE
      REAL RVROPRN            ! RUNWAY VISIBILITY .GT. INSTRUMENT RANGE
      REAL RVROPRX            ! RUNWAY VISIBILITY .GT. INSTRUMENT RANGE
      REAL RVRTEND            ! RUNWAY VISIBILITY TENDENCY
      REAL HVIS               ! HORIZONTAL VISIBILITY               !2.3
      REAL VVIS               ! VERTICAL VISIBILITY
      REAL RVRPRL             ! RVR GROUP PARALLEL RUNWAY ID
      REAL QNH                ! QNH PRESSURE (MILLIBARS)
      REAL RDLOC              ! RUNWAY STATE                        !2.0
      REAL REXP(*)            ! EXPANDED DATA (NUMERIC)
      REAL REWW(6)            ! RECENT WEATHER
      REAL RVRMIN             ! MINIMUM RUNWAY VISIBILITY
      REAL RVRMAX             ! MAXIMUM RUNWAY VISIBILITY
      REAL WDWD(9)            ! SIGNIFICANT WEATHER

      PARAMETER (INTOMB=33.86389)
      PARAMETER (KELVIN=273)
      PARAMETER (METRES=30)

      CHARACTER*80 HEAD                                              !4

! Data statements.

      DATA CHARR/' '/                                               !2.0
      DATA RDLOC/14./                                               !2.0

      HEAD = '$Workfile: mtrexp.f$ ' //
     &       '$Revision: 4$ $Date: 18/06/2010 10:23:25$'

! Initialise variables.

      ID=' '
      DDD=-9999999
      FFF=-9999999
      FMFMFM=-9999999
      QNH=-9999999
      VVIS=-9999999
      WW=-9999999
      CLDAMT=-9999999
      CLDTYPE=-9999999
      CLDHT=-9999999
      RVRDIR=-9999999
      RVROPRN=-9999999
      RVROPRX=-9999999
      RVRPRL=-9999999
      RVRMIN=-9999999
      RVRMAX=-9999999
      RWYDIR=-9999999
      RWYPRL=-9999999
      RWYUSE=-9999999
      RVRTEND=-9999999

      DO LOOP=1,6
        REWW(LOOP)=-9999999
      ENDDO
      DO LOOP=1,9
        WDWD(LOOP)=-9999999
      ENDDO
      DO LOOP=1,16
        ELEM(LOOP)=0
      ENDDO

      IFAIL=0        ! STATUS INDICATOR  - 0 IF OK, 8 IF ERROR
      SHRGRP=0       ! SHEAR RUNWAY GROUP COUNTER
      NGRP=0         ! NUMBER OF GROUPS
      POINT2=1       ! POINTER TO REPORT POSITION WHERE NEXT GROUP IS
      WWBAD=.FALSE.  !
      LREPFL=.FALSE. ! NOT END OF REPORT (YET)
      SHBAD=.FALSE.  ! BAD SHEAR GROUP UNTRUE
      MoreTrend=.TRUE.
      COUNT=0

  1   CONTINUE
      TEXT=' '
      CHARR(:)=' '                                                 !1.4
!----------------------------------------------------------------------
! Check the end of report flag. Exit if reached end of report.
!----------------------------------------------------------------------

      IF (LREPFL) THEN
        GOTO 2
      ENDIF
!----------------------------------------------------------------------
! Keep the current position report.
!----------------------------------------------------------------------

      POINT1=POINT2

!1.7 Ignore Report after RMK is found
      RMKLEN = 0                                               !1.7
      IF (INDEX(REPORT,'RMK').NE.0)  THEN                      !1.7
        RMKLEN = REPLEN - INDEX(REPORT,'RMK') + 1              !1.7
        REPORT(INDEX(REPORT,'RMK')+1:REPLEN)=' '               !1.7
        REPLEN = REPLEN - RMKLEN                               !1.7
      ENDIF                                                    !1.7

!----------------------------------------------------------------------
! Check for METAR forecast groups, trend groups or remarks sections.
! UK reports may have a runway deposit state appended to the end of the
! report. A check is made for the existence of this group for UK reports
! only (albeit a rather crude check).
!----------------------------------------------------------------------

      TEXT=REPORT(POINT1:POINT1+3)
      IF (TEXT .EQ. 'BECM' .OR. TEXT .EQ. 'TEMP' .OR.
     &  TEXT .EQ. 'NOSI') THEN
        DO WHILE (MoreTrend .AND. COUNT .LE. 2)
          DISP=86+(COUNT*29)
          CALL MTRTRD(REPORT,POINT1,REPLEN,REXP,MoreTrend,DISP)
          COUNT=COUNT+1
        ENDDO
! Point to expected start position of runway deposit group.
! Skip any spaces if found. (Note UK stations only).

        IF (ID(1:2) .EQ. 'EG') THEN
          POINT1=REPLEN-8
          IF (REPORT(POINT1:POINT1) .EQ. ' ') THEN
            POINT1=POINT1+1
            GRPLEN=8
            CALL MTRGRP(REPORT(POINT1:),GRPLEN,NCHAR,CHARR)
            IF (NCHAR .EQ. 0 .OR.
     &          REPORT(POINT1+2:POINT1+5) .EQ. 'CLRD') THEN
              CEXP(14:)=REPORT(POINT1:POINT1+7)
              REXP(78)=RDLOC
            ENDIF
          ENDIF
        ENDIF
        GOTO 2
      ENDIF

! Check whether group is a windshear group before MTRLOC locates the
! size of the next group. (Windshear groups consist of multiple smaller
! groups).
! Use MTRSHE to ascertain the real group length.

      IF (REPORT(POINT1:POINT1+1) .EQ. 'WS' .AND. NGRP .GE. 4) THEN
        CALL MTRSHE(REPLEN,REPORT,POINT2,GRPLEN,LREPFL,SHBAD,RWYUSE,
     &              RWYDIR,RWYPRL)

! Provided the maximum number of wind shear groups have not been
! expanded and there are no syntax errors in the group, expand the data
! into the array.

        IF (SHBAD .OR. SHRGRP .GT. 2) THEN
          GOTO 1
        ELSE
          SHRGRP=SHRGRP+1
          REXP(70+(SHRGRP-1)*4)=RWYDIR
          REXP(71+(SHRGRP-1)*4)=RWYPRL
          REXP(72+(SHRGRP-1)*4)=RWYUSE
          GOTO 1
        ENDIF

! Some visibility groups are reported in statute miles and half miles.
! Determine if the group is one of these and set the group length
! bypassing MTRLOC.

      ELSEIF (NGRP .GE. 2 .AND. NGRP .LE. 7 .AND.
     &        REPORT(POINT1+5:POINT1+6) .EQ. 'SM' .AND.
     &        REPORT(POINT1+3:POINT1+3) .EQ. '/') THEN
        GRPLEN=7
        POINT2=POINT1+GRPLEN+1

! If the group is not a wind shear or statute miles visibility group,
! use MTRLOC to get the group length.

      ELSE
        CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
      ENDIF

! Obtain the character content of the group using MTRGRP and increment
! the group counter to total the number of groups found so far.

      CALL MTRGRP(REPORT(POINT1:),GRPLEN,NCHAR,CHARR)
      NGRP=NGRP+1

! Now check on groups expected within the first three groups of a
! METAR report.

      IF (NGRP .LE. 3) THEN

! The first group is assumed to be the station identifier. This can be
! either a 4 letter ICAO (American, Canadian are stored as 4 letters) or
! a 5 figure identifier (Austrian).
! Store the identifier for later use and get a new group.
! In the unlikely event it is not an identifier, the report will not be
! expanded.

        IF (NGRP .EQ. 1) THEN
          IF (GRPLEN .EQ. 4 .AND. NCHAR .EQ. 4) THEN
            ID(1:4)=REPORT(POINT1:POINT1+GRPLEN-1)
          ELSEIF (GRPLEN .EQ. 5 .AND. NCHAR .EQ. 0) THEN
            ID(1:5)=REPORT(POINT1:POINT1+GRPLEN-1)
          ELSE
            GOTO 2
          ENDIF
          GOTO 1
        ENDIF

! Check whether group is a second American identifier and abort
! expansion if it is. (It is very likely the report is unexpandable).
! (Note: only check group number 2 for this).

        IF (NGRP .EQ. 2 .AND. GRPLEN .EQ. 2 .AND. NCHAR .EQ. 2) THEN
          IFAIL=8
          GOTO 2

! Check whether the group is 'AUTO', representing an automatic station.
! Check the expansion flag then set the expansion value and get a new
! group.

        ELSEIF (GRPLEN .EQ. 4 .AND. ELEM(1) .EQ. 0 .AND.
     &          REPORT(POINT1:POINT1+3) .EQ. 'AUTO') THEN
          REXP(79)=0
          ELEM(1)=1
          GOTO 1

! ***** (YY)GGgg(z) GROUP CHECK ***************************************

! Check whether group is a valid (YY)GGgg(Z) format combination.
! Convert REXP(7),HOUR, and REXP(6),DAY, to integer format and check
! report (YY)GGgg, of Austrian (block 11) reports only, against the
! time taken from the bulletin header. Otherwise get a new group.
! A comparison check is already made between the report (YY)GGgg and the
! bulletin time during storage for reports received from other countries

        ELSEIF ((GRPLEN .EQ. 5 .OR. GRPLEN .EQ. 7) .AND. NCHAR .EQ. 1
     &          .AND. REPORT(POINT1+GRPLEN-1:POINT1+GRPLEN-1) .EQ. 'Z'
     &     .OR. (GRPLEN .EQ. 4 .OR. GRPLEN .EQ. 6) .AND. NCHAR .EQ. 0
     &          .AND. (NGRP .EQ. 2 .OR. ELEM(1) .EQ. 1)) THEN
          IF (ID(1:2) .EQ. '11') THEN
            DAY=REXP(6)
            HRS=REXP(7)

! Check (YY)GGgg(Z) combinations.

            IF ((GRPLEN .EQ. 6 .OR. GRPLEN .EQ. 7) .AND.
     &          CHARR(1:4) .EQ. 'NNNN') THEN
              READ (REPORT(POINT1:POINT1+1),'(I2)') TIME
              IF (TIME .NE. DAY) THEN
                REXP(6)=TIME
              ENDIF
              READ (REPORT(POINT1+2:POINT1+3),'(I2)') TIME
              IF (TIME .NE. HRS) THEN
                REXP(7)=TIME
              ENDIF

! Check GGgg(Z) combinations.

            ELSEIF ((GRPLEN .EQ. 4 .OR. GRPLEN .EQ. 5) .AND.
     &              CHARR(1:2) .EQ. 'NN' ) THEN
              READ(REPORT(POINT1:POINT1+1),'(I2)') TIME
              IF (TIME .NE. HRS) THEN
                REXP(7)=TIME
              ENDIF
            ENDIF
          ENDIF
          GOTO 1
        ENDIF
      ENDIF


! ***** WIND GROUP CHECK **********************************************
! Ensure the wind group has not already been expanded.

      IF (ELEM(2) .EQ. 0) THEN

! Check whether the wind is calm.

        IF (GRPLEN .EQ. 5 .AND. NCHAR .EQ. 0 .AND.
     &      REPORT(POINT1:POINT1+4) .EQ. '00000') THEN
          REXP(11)=0.
          REXP(12)=0.
          ELEM(2)=1
          GOTO 1

! Check other possibilities for a valid wind group (except variable).
! Variable winds dealt with within MTRDDF !2.2

        ELSEIF (GRPLEN .GE. 7 .AND.
     &          CHARR(GRPLEN-1:GRPLEN) .EQ. 'YY') THEN
          CALL MTRDDF(REPORT,POINT1,GRPLEN,CHARR,DDD,FFF,FMFMFM)    !2.0
          IF (DDD .GE. 0) THEN
            REXP(11)=DDD
          ENDIF
          IF (FFF .GE. 0) THEN
          REXP(12)=FFF
          ENDIF
          IF (FMFMFM .GE. 0) THEN
            REXP(13)=FMFMFM
          ENDIF
          ELEM(2)=1
          GOTO 1
        ENDIF
      ENDIF

! Check whether wind is variable.

      IF (GRPLEN .EQ. 7 .AND. CHARR(1:7) .EQ. 'NNNYNNN' .AND.
     &    REPORT(POINT1+3:POINT1+3) .EQ. 'V') THEN
!1.6 Check to make sure that characters are of correct type
        REXP(15)=FLOAT(IVALUE(REPORT(POINT1:POINT1+2)))       !1.6
        REXP(14)=FLOAT(IVALUE(REPORT(POINT1+4:POINT1+6)))     !1.6
        GOTO 1
      ENDIF


! ***** HORIZONTAL VISIBILITY GROUP CHECKS ****************************
! Ensure visibility has not already been expanded.

      IF (ELEM(3) .EQ. 0) THEN
        IPT = POINT1 + GRPLEN - 1 ! Position of last char in group  !2.3
        HVIS = -9999999.0         ! Horiz vis not yet decoded       !2.3

! Visibility groups in KM. Can be 3 or 4 characters ('nKM' or 'nnKM').

        IF ((GRPLEN.EQ.3 .OR. GRPLEN.EQ.4) .AND.  ! 3 or 4 chars    !2.3
     &      REPORT(IPT-1:IPT).EQ.'KM') THEN       ! ending 'KM'     !2.3

          VIS = IVALUE(REPORT(POINT1:IPT-2))      ! Whole kms       !2.3
          IF (VIS.GE.0) HVIS = 1000*FLOAT(VIS)    ! Convert to m    !2.3

! Visibility groups in statute miles (SM). Can be from 3 to 7
! characters in various formats (see below) but ending 'SM'.
! Conversion to metres is done just before leaving IF block.

        ELSEIF ((GRPLEN.GE.3 .AND. GRPLEN.LE.7) .AND. ! 3-7 chars   !2.3
     &          REPORT(IPT-1:IPT).EQ.'SM') THEN       ! ending 'SM' !2.3

! Whole miles: Can be 3 or 4 characters ('nSM' or 'nnSM').

          IF (GRPLEN.EQ.3 .OR. GRPLEN.EQ.4) THEN                    !2.3

            VIS = IVALUE(REPORT(POINT1:IPT-2))  ! Whole miles       !2.3
            IF (VIS.GE.0) HVIS = FLOAT(VIS)                         !2.3

! Fraction of mile with or without integer amount (7 or 5 characters
! respectively, i.e. 'n i/jSM' for n + i/j miles or just 'i/jSM' for
! i/j miles).  In these cases, the 4th character from the end is '/'.

          ELSE IF (REPORT(IPT-3:IPT-3).EQ.'/') THEN                 !2.3

            I1 = IVALUE(REPORT(IPT-4:IPT-4))  ! Numerator           !2.3
            I2 = IVALUE(REPORT(IPT-2:IPT-2))  ! Denominator         !2.3

            IF (GRPLEN.EQ.7) THEN                                   !2.3
              VIS = IVALUE(REPORT(POINT1:POINT1)) ! Whole miles     !2.3
            ELSE                                                    !2.3
              VIS = 0  ! No whole miles specified                   !2.3
            END IF                                                  !2.3

            IF (I1.GT.0 .AND. I2.GT.0 .AND. VIS.GE.0)  ! Good data  !2.3
     &          HVIS = FLOAT(VIS) + FLOAT(I1)/FLOAT(I2)  ! Miles    !2.3

! Sixteenths of a mile: 6 characters ('n/16SM').

          ELSE IF (GRPLEN.EQ.6 .AND. REPORT(IPT-4:IPT).EQ.'/16SM') THEN

            I1 = IVALUE(REPORT(POINT1:POINT1))    ! Numerator       !2.3
            IF (I1.GT.0) HVIS = 0.0625*FLOAT(I1)  ! Miles           !2.3
          END IF                                                    !2.3

! Convert horizontal visibilities to metres.
! (1 statute mile = 5280 feet = 5280*0.3048 metres = 1609.344 metres.)

          IF (HVIS.GT.0) HVIS = 1609.344*HVIS                       !2.3

! All numeric 4-character visibility group. No conversion required.
! '9999' represents 10km or more - converted to 10000 metres here.

        ELSEIF (GRPLEN .EQ. 4 .AND. NCHAR .EQ. 0) THEN
          VIS = IVALUE(REPORT(POINT1:POINT1+3))  ! Metres           !1.6
          IF (VIS.EQ.9999) VIS = 10000                              !2.3
          HVIS = FLOAT(VIS)                                         !2.3

! Check for the presence of NDV in the Horz Vis group. See           !2
! FM 15 XIV 15.6.1 for more details                                  !2
                                                                     !2
        ELSEIF (GRPLEN.LE.7.AND.REPORT(IPT-2:IPT).EQ.'NDV') THEN     !2
          VIS=IVALUE(REPORT(POINT1:IPT-3))                           !2
          IF (VIS.EQ.9999) VIS = 10000                               !2
          HVIS = FLOAT(VIS)                                          !2
!
! Set IPT to zero if not a visibility group

        ELSE                                                        !2.3
          IPT = 0  ! Indicates not visibility group                 !2.3
        END IF

! Update REXP and ELEM arrays, and return for next group
! As when there is only one group it is difficult to tell           !2.4
! if this is minimum (before Nov 2004) or prevailing visibility     !2.4
! (after 25th Nov 2004) both are set the same.                      !2.4

        IF (IPT.GT.0) THEN  ! Visibility group was found            !2.3
          REXP(17) = HVIS                                           !2.3
          REXP(173) = HVIS                                          !2.4
          ELEM(3)=1                                                 !2.3
          GOTO 1                                                    !2.3
        ENDIF                                                       !2.3
      ENDIF

! Expand a visibility group with a minimum or maximum direction of
! N, E, S or W.

      IF (GRPLEN .EQ. 5 .AND. CHARR(5:5) .EQ. 'Y' .AND.
     &    NCHAR .EQ. 1) THEN
        IF (ELEM(3) .EQ. 0 .OR. ELEM(4) .EQ. 0) THEN
          VIS = IVALUE(REPORT(POINT1:POINT1+3))            !1.6
          IF (REPORT(POINT1+4:POINT1+4) .EQ. 'N') THEN
            DIR=360
          ELSEIF (REPORT(POINT1+4:POINT1+4) .EQ. 'E') THEN
            DIR=90
          ELSEIF (REPORT(POINT1+4:POINT1+4) .EQ. 'S') THEN
            DIR=180
          ELSEIF (REPORT(POINT1+4:POINT1+4) .EQ. 'W') THEN
            DIR=270
          ENDIF
          IF (ELEM(3) .EQ. 0) THEN
            REXP(16)=DIR
            REXP(17)=VIS
            REXP(173)= -9999999.0                         !2.4
            ELEM(3)=1
          ELSEIF (ELEM(4) .EQ. 0) THEN

! A change 25th November 2005 to the reporting of Metars meant that!2.4
! the first group would always be prevailing visibility, without   !2.4
! a direction, and second group would be the minimum visibility    !2.4
! with a direction. However this routine still needs to decode     !2.4
! metars decoded the old way so a check is to see if a direction   !2.4
! for minimum visibility has already been set. If so then this is  !2.4
! coded the old way and the second visibility group s a maximum.   !2.4
! If a direction had not been set for the first visibility then    !2.4
! it is assumed this metar is coded the new way with the first     !2.4
! group being the prevailing visibility and second group being     !2.4
! minimum visibility.                                              !2.4
            IF (REXP(16).GE.0.0) THEN  ! max vis                   !2.4
              REXP(18)=DIR
              REXP(19)=VIS
            ELSE                       ! min vis                   !2.4
              REXP(16)=DIR                                         !2.4
              REXP(17)=VIS                                         !2.4
            ENDIF                                                  !2.4

            ELEM(4)=1
          ENDIF
        ENDIF
        GOTO 1

! Expand a visibility group with a minimum or maximum direction of
! NE, SE, SW or NW.

      ELSEIF (GRPLEN .EQ. 6 .AND. NCHAR .EQ. 2 .AND.
     &        CHARR(5:6) .EQ. 'YY') THEN
        IF (ELEM(3) .EQ. 0 .OR. ELEM(4) .EQ. 0) THEN  !2.4
          IF (REPORT(POINT1+4:POINT1+5) .EQ. 'NE') THEN
            DIR=45
          ELSEIF (REPORT(POINT1+4:POINT1+5) .EQ. 'SE') THEN
            DIR=135
          ELSEIF (REPORT(POINT1+4:POINT1+5) .EQ. 'SW') THEN
            DIR=225
          ELSEIF (REPORT(POINT1+4:POINT1+5) .EQ. 'NW') THEN
            DIR=315
          ELSE
            GOTO 1
          ENDIF
          VIS = IVALUE(REPORT(POINT1:POINT1+3))                !1.6
          IF (ELEM(3) .EQ. 0) THEN
            REXP(16)=DIR
            REXP(17)=VIS
            REXP(173)= -9999999.0                              !2.4
            ELEM(3)=1
          ELSEIF (ELEM(4) .EQ. 0) THEN

! A change 25th November 2005 to the reporting of Metars meant that!2.4
! the first group would always be prevailing visibility, without   !2.4
! a direction, and second group would be the minimum visibility    !2.4
! with a direction. However this routine still needs to decode     !2.4
! metars decoded the old way so a check is to see if a direction   !2.4
! for minimum visibility has already been set. If so then this is  !2.4
! coded the old way and the second visibility group s a maximum.   !2.4
! If a direction had not been set for the first visibility then    !2.4
! it is assumed this metar is coded the new way with the first     !2.4
! group being the prevailing visibility and second group being     !2.4
! minimum visibility.                                              !2.4

            IF (REXP(16).GE.0.0) THEN  ! max vis                   !2.4
              REXP(18)=DIR
              REXP(19)=VIS
            ELSE                       ! min vis                   !2.4
              REXP(16)=DIR                                         !2.4
              REXP(17)=VIS                                         !2.4
            ENDIF                                                  !2.4

            ELEM(4)=1
          ENDIF
        ENDIF
        GOTO 1


! ***** GENERAL WEATHER GROUP CHECKS **********************************

! Check whether group is 'CAVOK' and no group of this type has already
! been expanded.
! If other general weather already expanded into the array, then add
! the value for 'CAVOK' to the value in the array. Otherwise set value
! for 'CAVOK'.

      ELSEIF (GRPLEN .EQ. 5 .AND. ELEM(5) .EQ. 0 .AND.
     &        REPORT(POINT1:POINT1+4) .EQ. 'CAVOK') THEN
        IF (REXP(20) .LT. 0) THEN
          REXP(20)=2
        ELSE
          REXP(20)=REXP(20)+2
        ENDIF
        ELEM(5)=1
        GOTO 1

! Check whether group is 'SKC' and no group of this type has already
! been expanded.
! If other general weather already expanded into the array, then add
! the value for 'SKC' to the value in the array. Otherwise set value
! for 'SKC'.

      ELSEIF (GRPLEN .EQ. 3 .AND. ELEM(6) .EQ. 0 .AND.
     &        REPORT(POINT1:POINT1+2) .EQ. 'SKC') THEN
        IF (REXP(20) .LT. 0) THEN
          REXP(20)=4
        ELSE
          REXP(20)=REXP(20)+4
        ENDIF
        ELEM(6)=1
        GOTO 1

! Check whether group is 'NSW' and no group of this type has already
! been expanded.
! If other general weather already expanded into the array, then add
! the value for 'NSW' to the value in the array. Otherwise set value
! for 'NSW'.

      ELSEIF (GRPLEN .EQ. 3 .AND. ELEM(7) .EQ. 0 .AND.
     &        REPORT(POINT1:POINT1+2) .EQ. 'NSW') THEN
        IF (REXP(20) .LT. 0) THEN
          REXP(20)=8
        ELSE
          REXP(20)=REXP(20)+8
        ENDIF
        ELEM(7)=1
        GOTO 1


! ***** CURRENT AND RECENT WEATHER GROUP CHECKS ***********************

! Check for all types of weather group W'W'WW, WWW'W', W'W' and REWW

      ELSEIF (((GRPLEN .GE. 4 .AND. NCHAR .EQ. GRPLEN-2 .AND.
     &          CHARR(GRPLEN-1:GRPLEN) .EQ. 'NN') .OR.
     &         (GRPLEN .GE. 4 .AND. NCHAR .EQ. GRPLEN-2 .AND.
     &          CHARR(1:4) .EQ. 'NNYY') .OR.
     &         (GRPLEN .EQ. NCHAR .AND. GRPLEN .GE. 2 .AND.
     &          CHARR(GRPLEN-1:GRPLEN) .NE. '//') .OR.
     &         (GRPLEN .EQ. 2 .AND. NCHAR .EQ. 0)) .AND.
     &          REPORT(POINT1:POINT1+1) .NE. 'WS') THEN

! Read numeric weather code value for W'W'WW groups.

        IF (CHARR(GRPLEN-1:GRPLEN) .EQ. 'NN') THEN
          WW = IVALUE(REPORT(POINT1+GRPLEN-2:POINT1+GRPLEN-1))   !1.6
          GRPLEN=GRPLEN-2

! Read numeric weather code value for WWW'W' groups.
! Move pointer past WW ready for call to MTRWWR.

        ELSEIF (CHARR(1:4) .EQ. 'NNYY') THEN
          WW = IVALUE(REPORT(POINT1:POINT1+1))                   !1.6
          POINT1=POINT1+2
          GRPLEN=GRPLEN-2
        ENDIF

! Otherwise the default is for W'W' groups.

! Check whether numeric value already expanded into array. If not, then
! copy value to array and set flag.

        IF (ELEM(8) .EQ. 0) THEN
          REXP(44)=WW
          ELEM(8)=1
        ENDIF

! Set starting value in flag array.

        ESTAR=9

! Call metar ww location and decoding routine to expand W'W' without
! WW, if applicable.

        CALL MTRWWR(POINT1,REPORT,GRPLEN,ELEM,ESTAR,WDWD,REWW,WWBAD)

! If valid weather group, then insert weather into expansion array.

        IF (.NOT.WWBAD) THEN

! Insert present weather into expansion array.

          IF (ELEM(9) .GT. 0) THEN
            IF (WW .LT. 0) THEN
              REXP(35+(ELEM(9)-1)*3)=WDWD(1+(ELEM(9)-1)*3)
            ENDIF
            REXP(36+(ELEM(9)-1)*3)=WDWD(2+(ELEM(9)-1)*3)
            REXP(37+(ELEM(9)-1)*3)=WDWD(ELEM(9)*3)
          ENDIF

! Insert recent weather information into expansion array.
! The real array values are not contigously displaced because the code
! was changed since the original software was written.

          IF (ELEM(10) .GT. 0) THEN
            REXP(80+(ELEM(10)-1)*2)=REWW(1+(ELEM(10)-1)*2)
            REXP(81+(ELEM(10)-1)*2)=REWW(2+(ELEM(10)-1)*2)
            IF (ELEM(10) .EQ. 1) THEN
              REXP(68)=REXP(80)
              REXP(69)=REXP(81)
            ENDIF
          ENDIF
          GOTO 1
        ENDIF


! ***** VERTICAL VISIBILITY GROUP CHECKS ******************************
! If the vertical visibility height is missing and no cloud groups are
! reported, then the value returned will remain as missing. Ensure
! the vertical visibility has not already been expanded.

      ELSEIF (REPORT(POINT1:POINT1+1) .EQ. 'VV' .AND. GRPLEN .EQ. 5
     &        .AND. (CHARR(3:5) .EQ. '///' .OR. CHARR(3:5) .EQ. 'NNN')
     &        .AND. ELEM(11) .EQ. 0) THEN
        IF (CHARR(3:5) .EQ. '///') THEN
          IF (ELEM(14) .GT. 0 .OR. ELEM(15) .GT. 0) THEN
            GOTO 1
          ELSE
            REXP(45)=9
            ELEM(14)=ELEM(14)+1
            GOTO 1
          ENDIF
        ENDIF

!1.6 Check if numeric values
        REXP(63)=FLOAT(IVALUE(REPORT(POINT1+2:POINT1+4)))        !1.6
        IF (REXP(63).NE.-9999999.0) THEN                         !1.6
          REXP(63)=REXP(63)*METRES                               !1.6
        ENDIF                                                    !1.6
        ELEM(11)=1
        GOTO 1
      ENDIF

! ***** PRESSURE GROUP CHECKS *****************************************
! Ensure the pressure group has not already been expanded.

      IF (ELEM(12) .EQ. 0) THEN

! There are various possible formats for a valid pressure group. Before
! expansion, the pressure in either millibars orinches is converted
! to pascals.
! The group may consist of either a 'Q' or an 'A' and 4 numerics.

        IF (GRPLEN .EQ. 5 .AND. NCHAR .EQ. 1 .AND.
     &      (REPORT(POINT1:POINT1) .EQ. 'Q' .OR.
     &       REPORT(POINT1:POINT1) .EQ. 'A')) THEN
          IF (REPORT(POINT1:POINT1) .EQ. 'Q') THEN

!1.6 Check that values are numeric
            QNH = FLOAT(IVALUE(REPORT(POINT1+1:POINT1+4)))     !1.6
            IF (QNH.NE.-9999999.0) THEN                        !1.6
               QNH=QNH*100.                                    !1.6
            ENDIF                                             !1.6

          ELSEIF (REPORT(POINT1:POINT1+1) .EQ. 'A2' .OR.
     &            REPORT(POINT1:POINT1+1) .EQ. 'A3') THEN

!1.6 check that all numerics where expected.
              INCHES = IVALUE(REPORT(POINT1+1:POINT1+2))   !1.6
              IF (INCHES.EQ.-9999999) THEN                 !1.6
                QNH = -9999999.0                           !1.6
              ELSE                                         !1.6
                QNH=INCHES*100                             !1.6
                INCHES = IVALUE(REPORT(POINT1+3:POINT1+4)) !1.6
                IF (INCHES.EQ.-9999999) THEN               !1.6
                  QNH = -9999999.0                         !1.6
                ELSE                                       !1.6
                  QNH=(QNH+INCHES)*INTOMB                  !1.6
                ENDIF                                      !1.6
              ENDIF                                        !1.6
          ENDIF
          REXP(66)=QNH
          ELEM(12)=1
          GOTO 1

! Or the group may consist of all numerics.

        ELSEIF (GRPLEN .EQ. 4 .AND. NCHAR .EQ. 0) THEN
!1.6 Check that REPORT(POINT1:PPOINT1+3) contains numerics
          QNH = FLOAT(IVALUE(REPORT(POINT1:POINT1+3)))          !1.6
          IF (QNH.NE.-9999999.0) THEN                           !1.6
            REXP(66)=QNH*100                                    !1.6
          ELSE                                                  !1.6
            REXP(66) = QNH                                      !1.6
          ENDIF                                                 !1.6

          ELEM(12)=1
          GOTO 1

! Or the group may consist of a 'Q' and 3 numerics.

        ELSEIF (GRPLEN .EQ. 4 .AND. REPORT(POINT1:POINT1) .EQ. 'Q'
     &          .AND. NCHAR .EQ. 1) THEN
!1.6 Check that REPORT(POINT1+1:POINT1+3) is indeed numeric
          QNH = FLOAT(IVALUE(REPORT(POINT1+1:POINT1+3)))       !1.6
          IF (QNH.NE.-9999999.0) THEN                          !1.6
            REXP(66)=QNH*100                                   !1.6
          ELSE                                                 !1.6
            REXP(66) = QNH                                     !1.6
          ENDIF                                                !1.6
          ELEM(12)=1
          GOTO 1

! Or the group may consist of 3 numerics.

        ELSEIF (GRPLEN .EQ. 3 .AND. NCHAR .EQ. 0) THEN
!1.6 Check that REPORT(POINT1:POINT1+2)does include numeric values
          QNH = FLOAT(IVALUE(REPORT(POINT1:POINT1+2)))     !1.6
          IF ((QNH.NE.-9999999.0).AND.                     !2.1
     &        (QNH.GE.900.0)) THEN                         !2.1
            REXP(66)=QNH*100                               !1.6
            ELEM(12)=1
          ELSE                                             !1.6
            REXP(66)= -9999999.0                           !2.1
          ENDIF                                            !1.6
          GOTO 1
        ENDIF
      ENDIF

! ***** TEMPERATURE & DEW POINT GROUP CHECKS **************************
! Ensure temperature group has not already been expanded.

      IF (ELEM(13) .EQ. 0) THEN

! If the dew point is missing it's value in the report should be
! replaced by '//'.

! Check 5 figure temperature group. Both dry bulb and dew point are
! positive.

        IF (GRPLEN .EQ. 5 .AND. (CHARR(1:5) .EQ. 'NN/NN' .OR.
     &      CHARR(1:5) .EQ. 'NN///')) THEN
          TEMP = IVALUE(REPORT(POINT1:POINT1+1))                !1.6
          IF (TEMP.EQ.-9999999) THEN                            !1.6
            REXP(64)=TEMP                                       !1.6
          ELSE                                                  !1.6
            REXP(64)=TEMP+KELVIN                                !1.6
          ENDIF                                                 !1.6

          IF (CHARR(4:5) .EQ. 'NN') THEN
            TEMP = IVALUE(REPORT(POINT1+3:POINT1+4))               !1.6
            IF (TEMP.EQ.-9999999) THEN                             !1.6
              REXP(65) = TEMP                                      !1.6
            ELSE                                                   !1.6
              REXP(65)=TEMP+KELVIN                                 !1.6
            ENDIF                                                  !1.6

            ELEM(13)=1
            GOTO 1
          ENDIF

! Check 6 figure temperature group. Dry bulb is positive but dew point
! is negative.

        ELSEIF (GRPLEN .EQ. 6 .AND. CHARR(1:6) .EQ. 'NN/YNN') THEN
          TEMP = IVALUE(REPORT(POINT1:POINT1+1))            !1.6
          IF (TEMP.EQ.-9999999) THEN                        !1.6
            REXP(64)=TEMP                                   !1.6
          ELSE                                              !1.6
            REXP(64)=TEMP+KELVIN                            !1.6
            TEMP = IVALUE(REPORT(POINT1+4:POINT1+5))        !1.6
            IF (TEMP.EQ.-9999999) THEN                      !1.6
              REXP(65)=TEMP                                 !1.6
            ELSE                                            !1.6
              REXP(65)=-TEMP+KELVIN                         !1.6
            ENDIF                                           !1.6
          ENDIF                                             !1.6

          ELEM(13)=1
          GOTO 1

! Check 7 figure temperature group. Both dry bulb and dew point are
! negative.

        ELSEIF (GRPLEN .EQ. 7 .AND. CHARR(1:7) .EQ. 'YNN/YNN') THEN
          TEMP = IVALUE(REPORT(POINT1+1:POINT1+2))               !1.6
          IF (TEMP.EQ.-9999999) THEN                             !1.6
             REXP(64)=TEMP                                       !1.6
          ELSE                                                   !1.6
            REXP(64)=-TEMP+KELVIN                                !1.6
            TEMP = IVALUE(REPORT(POINT1+5:POINT1+6))             !1.6
            IF (TEMP.EQ.-9999999) THEN                           !1.6
              REXP(65)=TEMP                                      !1.6
            ELSE                                                 !1.6
              REXP(65)=-TEMP+KELVIN                              !1.6
            ENDIF                                                !1.6
          ENDIF                                                  !1.6

          ELEM(13)=1
          GOTO 1
        ENDIF
      ENDIF


! ***** RUNWAY VISUAL RANGE GROUPS ************************************
! Ensure the maximum number of RVR's to be expanded is not exceeded.

      IF (NCHAR .LT. GRPLEN .AND. REPORT(POINT1:POINT1) .EQ. 'R'
     &    .AND. ELEM(16) .LE. 1) THEN
        CALL MTRRWY(POINT1,REPORT,GRPLEN,CHARR,RVRDIR,RVRTEND,      !2.0
     &              RVRPRL,RVROPRN,RVRMIN,RVROPRX,RVRMAX)
        ELEM(16)=ELEM(16)+1
        REXP(21+(ELEM(16)-1)*7)=RVRDIR
        REXP(22+(ELEM(16)-1)*7)=RVRPRL
        REXP(23+(ELEM(16)-1)*7)=RVRTEND
        REXP(24+(ELEM(16)-1)*7)=RVROPRN
        REXP(25+(ELEM(16)-1)*7)=RVRMIN
        REXP(26+(ELEM(16)-1)*7)=RVROPRX
        REXP(27+(ELEM(16)-1)*7)=RVRMAX
        GOTO 1


! ***** CLOUD GROUP CHECKS ********************************************
! Ensure that the maximum number of cloud groups to be expanded is not
! exceeded.

      ELSEIF (GRPLEN .EQ. 6 .AND. NCHAR .LT. GRPLEN) THEN
        IF (ELEM(14) .LE. 3 .OR. ELEM(15) .LE. 3) THEN

! New cloud group processing starts here.

          IF (CHARR(1:3) .EQ. 'YYY' .AND. ELEM(14) .LE. 3) THEN
            ESTAR=14
            CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,      !2.0
     &                  CLDAMT,CLDTYPE,CLDHT,VVIS)
            REXP(45+(ELEM(ESTAR)-1)*3)=CLDAMT
            REXP(46+(ELEM(ESTAR)-1)*3)=CLDTYPE
            REXP(47+(ELEM(ESTAR)-1)*3)=CLDHT
            REXP(63)=VVIS
            GOTO 1

! Old cloud group processing starts here.

          ELSEIF (CHARR(1:3) .EQ. 'NYY' .OR. CHARR(1:3) .EQ. 'N//') THEN
            IF (REPORT(POINT1+1:POINT1+2) .EQ. 'CB' .AND.
     &          ELEM(15) .LE. 3) THEN
              ESTAR=15
              CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,    !2.0
     &                    CLDAMT,CLDTYPE,CLDHT,VVIS)
              REXP(54+(ELEM(ESTAR)-1)*3)=CLDAMT
              REXP(55+(ELEM(ESTAR)-1)*3)=CLDTYPE
              REXP(56+(ELEM(ESTAR)-1)*3)=CLDHT
              REXP(63)=VVIS
              GOTO 1
            ELSE
              ESTAR=14
              CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,    !2.0
     &                    CLDAMT,CLDTYPE,CLDHT,VVIS)
              REXP(45+(ELEM(ESTAR)-1)*3)=CLDAMT
              REXP(46+(ELEM(ESTAR)-1)*3)=CLDTYPE
              REXP(47+(ELEM(ESTAR)-1)*3)=CLDHT
              REXP(63)=VVIS
              GOTO 1
            ENDIF
          ENDIF
!1.7 Else if another cloud reported that should not be then
!1.7      move onto the next group.
        ELSEIF (CHARR(1:3).EQ.'YYY'.AND.                  !1.7
     &          (ELEM(14).GT.3.OR.ELEM(15).GT.3)) THEN    !1.7
          GOTO 1                                          !1.7
        ENDIF

! Search for cumulus cloud groups in the new code. Towering cumulus or
! cumulonimbus can be reported, by addition of 'TCU' or 'CB' behind the
! normal group of 6 characters.

      ELSEIF (GRPLEN .GE. 8 .AND. ELEM(15) .LE. 3 .AND.
     &       (REPORT(POINT1+GRPLEN-3:POINT1+GRPLEN-1) .EQ. 'TCU' .OR.
     &        REPORT(POINT1+GRPLEN-2:POINT1+GRPLEN-1) .EQ. 'CB')) THEN
        ESTAR=15
        CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,CLDAMT,   !2.0
     &              CLDTYPE,CLDHT,VVIS)
        REXP(54+(ELEM(ESTAR)-1)*3)=CLDAMT
        REXP(55+(ELEM(ESTAR)-1)*3)=CLDTYPE
        REXP(56+(ELEM(ESTAR)-1)*3)=CLDHT
        GOTO 1

! *********************************************************************
! Capture the NCD and / or NSC groups.                               !2
! *********************************************************************
      ELSEIF(GRPLEN.EQ.3.AND.ELEM(14).EQ.0.AND.ELEM(15).EQ.0) THEN   !2
       IF(REPORT(POINT1:POINT1+GRPLEN).EQ.'NSC'.OR.                  !2
     &   REPORT(POINT1:POINT1+GRPLEN).EQ.'NCD') THEN                 !2
          ESTAR=14                                                   !2
          CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,CLDAMT,  !2
     &                CLDTYPE,CLDHT,VVIS)                            !2
          REXP(45+(ELEM(ESTAR)-1)*3)=CLDAMT                          !2
          REXP(46+(ELEM(ESTAR)-1)*3)=CLDTYPE                         !2
          REXP(47+(ELEM(ESTAR)-1)*3)=CLDHT                           !2
       ENDIF                                                         !2
       GOTO 1                                                        !4
! ***** RUNWAY DEPOSIT GROUP ******************************************

! Search for the runway deposit group of 8 figures which is reported in
! the old code for UK stations and may be reported in the new code as
! an 8-figure group, no characters.

      ELSEIF (GRPLEN .EQ. 8 .AND. (NCHAR .EQ. 0 .OR.
     &        REPORT(POINT1+2:POINT1+5) .EQ. 'CLRD')) THEN
        CEXP(14:)=REPORT(POINT1:POINT1+7)
        REXP(78)=RDLOC
        GOTO 1
      ELSE
        GOTO 1
      ENDIF

 2    RETURN
      END
