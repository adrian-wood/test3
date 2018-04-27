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
!   ARGUMENTS    : 1 REPORT - METAR REPORT TO BE DECODED
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
! $Workfile: mtrexp.f90$ $Folder: OpSource$
! $Revision: 8$ $Date: 20/06/2011 10:29:06$
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         20/06/2011 10:29:06    Sheila Needham  Fixed
!       following review
!  7    MetDB_Refresh 1.6         19/06/2011 10:27:06    Sheila Needham  Add
!       explicit string lengths
!  6    MetDB_Refresh 1.5         06/04/2011 15:48:23    Alison Weir     Amend
!       IF to avoid out of range error
!  5    MetDB_Refresh 1.4         26/11/2010 09:39:49    Brian Barwell
!       Missing USE statement for mtrtrd_mod added.
!  4    MetDB_Refresh 1.3         25/11/2010 17:17:49    Brian Barwell
!       REXP(*) changed to REXP(:).
!  3    MetDB_Refresh 1.2         22/11/2010 17:47:15    Stan Kellett    IVALUE
!        function declaration removed as declared in mod file
!       removed old revision info
!  2    MetDB_Refresh 1.1         18/11/2010 10:47:32    Sheila Needham  USE
!       statements; add check on GRPLENs for IF tests
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
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

USE ivalue_mod
USE mtrccc_mod
USE mtrddf_mod
USE mtrgrp_mod
USE mtrloc_mod
USE mtrshe_mod
USE mtrtrd_mod
USE mtrwwr_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(INOUT) ::  REPORT ! WHOLE METAR REPORT
INTEGER,      INTENT(INOUT) ::  REPLEN ! METAR REPORT LENGTH
REAL,         INTENT(INOUT) ::  REXP(:) ! EXPANDED DATA (NUMERIC)
CHARACTER(*), INTENT(INOUT) ::  CEXP ! EXPANDED DATA (CHARACTER)
INTEGER,      INTENT(INOUT) ::  IFAIL ! ERROR STATUS FLAG

! Local declarations:

REAL,        PARAMETER ::  INTOMB = 33.86389 ! INCHES TO MB PRESSURE CONVERSION
REAL,        PARAMETER ::  KELVIN = 273 ! COARSE TEMPERATURE CONVERSION VALUE
INTEGER,     PARAMETER ::  METRES = 30 ! HEIGHT MULTIPLICATION FACTOR TO GIVE
                                       ! TRUE HEIGHT IN METRES ACCORDING TO
                                       ! WMO CODE TABLE 1690.

! Declare Character

CHARACTER(20) ::  CHARR = ' ' ! TRANSLATED GROUP DATA. NUMERIC CHAR
                        ! REPRESENTED BY 'N'. OTHERS BY 'Y'.
CHARACTER(5)  ::  ID    ! STATION IDENTIFIER
CHARACTER(4)  ::  TEXT  ! GENERAL VARIABLE USED TO HOLD A PART OF
                        ! THE REPORT IN ORDER TO ALLOW LARGE
                        ! LOGICAL COMPARISONS TO BE MORE READAB

!Declare Logical
LOGICAL      ::  LREPFL ! TRUE INDICATES NO MORE REPORT PROCESSING
LOGICAL      ::  MoreTrend ! More Trend info to decode
LOGICAL      ::  SHBAD  ! WINDSHEAR GROUP SYNTAX ERROR
LOGICAL      ::  WWBAD  ! CRAP SIG WEATHER GROUP

!Declare Integer
INTEGER      ::  CLDAMT ! TOTAL CLOUD AMOUNT
INTEGER      ::  TEMP   ! PRECONVERTED TEMPERATURE VALUE
INTEGER      ::  RWYUSE ! VALUE OF RUNWAY USE (WIND SHEAR GROUPS)
INTEGER      ::  INCHES ! PRESSURE IN INCHES
INTEGER      ::  VIS    ! VISIBILITY VALUE BEFORE CONVERSION
INTEGER      ::  DIR    ! WIND DIRECTION OF MAX/MIN VISIBILITY
INTEGER      ::  NGRP   ! COUNT OF NUMBER OF EXPANDED GROUPS
INTEGER      ::  SHRGRP ! NUMBER OF WIND SHEAR GROUPS EXPANDED
INTEGER      ::  DAY    ! VALIDITY DAY OF METAR REPORT
INTEGER      ::  ELEM(16) ! ELEMENT LIST USED TO EXPAND DATA INTO
                        ! REAL ARRAY
INTEGER      ::  ESTAR  ! STARTING POSITION WITHIN ARRAY ELEM
INTEGER      ::  GRPLEN ! LENGTH OF GROUP
INTEGER      ::  HRS    ! VALIDITY HOUR OF METAR REPORT
INTEGER      ::  NCHAR  ! NUMBER OF CHARACTERS IN GROUP
INTEGER      ::  POINT1 ! START POSITION OF CURRENT GROUP
INTEGER      ::  POINT2 ! START POSITION OF NEXT GROUP
INTEGER      ::  IPT    ! END POSITION OF CURRENT GROUP
INTEGER      ::  I1, I2 ! NUM & DENOM OF FRACTION IN VIS GROUP
INTEGER      ::  TIME   ! REPORT TIME (EITHER DAY OR HOUR)
INTEGER      ::  LOOP   ! GENERAL LOOP VARIABLE
INTEGER      ::  RWYPRL ! PARALLEL RUNWAY VALUE (WIND SHEAR)
INTEGER      ::  RWYDIR ! RUNWAY DIRECTION (WIND SHEAR)
INTEGER      ::  CLDTYPE ! CLOUD TYPE
INTEGER      ::  WW     ! PRESENT WEATHER IDENTIFIER
INTEGER      ::  COUNT
INTEGER      ::  DISP

INTEGER      ::  RMKLEN !1.7 Length of RMK section in REPORT
INTEGER      ::  RMKPNT !Start of RMK section in REPORT

!Declare Real
REAL         ::  DDD    ! WIND DIRECTION
REAL         ::  RVRDIR ! RVR GROUP RUNWAY IDENTIFIER
REAL         ::  FFF    ! WIND SPEED
REAL         ::  FMFMFM ! MAXIMUM GUST
REAL         ::  CLDHT  ! BASE HEIGHT OF CLOUD
REAL         ::  RVROPRN ! RUNWAY VISIBILITY >  INSTRUMENT RANGE
REAL         ::  RVROPRX ! RUNWAY VISIBILITY >  INSTRUMENT RANGE
REAL         ::  RVRTEND ! RUNWAY VISIBILITY TENDENCY
REAL         ::  HVIS   ! HORIZONTAL VISIBILITY
REAL         ::  VVIS   ! VERTICAL VISIBILITY
REAL         ::  RVRPRL ! RVR GROUP PARALLEL RUNWAY ID
REAL         ::  QNH    ! QNH PRESSURE (MILLIBARS)
REAL         ::  RDLOC = 14. ! RUNWAY STATE
REAL         ::  REWW(6) ! RECENT WEATHER
REAL         ::  RVRMIN ! MINIMUM RUNWAY VISIBILITY
REAL         ::  RVRMAX ! MAXIMUM RUNWAY VISIBILITY
REAL         ::  WDWD(9) ! SIGNIFICANT WEATHER


!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------


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
END DO
DO LOOP=1,9
  WDWD(LOOP)=-9999999
END DO
DO LOOP=1,16
  ELEM(LOOP)=0
END DO

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
TEXT(1:4)=' '
CHARR(1:20)=' '
!----------------------------------------------------------------------
! Check the end of report flag. Exit if reached end of report.
!----------------------------------------------------------------------

IF (LREPFL) THEN
  GOTO 2
END IF
!----------------------------------------------------------------------
! Keep the current position report.
!----------------------------------------------------------------------

POINT1=POINT2

!1.7 Ignore Report after RMK is found
RMKLEN = 0
RMKPNT = INDEX(REPORT(1:REPLEN),'RMK')

IF (RMKPNT /= 0)  THEN
  RMKLEN = REPLEN - RMKPNT + 1
  REPORT(RMKPNT+1:REPLEN)=' '
  REPLEN = REPLEN - RMKLEN
END IF

!----------------------------------------------------------------------
! CHECK FOR METAR FORECAST GROUPS, TREND GROUPS OR REMARKS SECTIONS.
! UK REPORTS MAY HAVE A RUNWAY DEPOSIT STATE APPENDED TO THE END OF THE
! REPORT. A CHECK IS MADE FOR THE EXISTENCE OF THIS GROUP FOR UK REPORTS
! ONLY (ALBEIT A RATHER CRUDE CHECK).
!----------------------------------------------------------------------

TEXT=REPORT(POINT1:POINT1+3)
IFLABEL1: &
IF (TEXT  ==  'BECM' .OR. TEXT  ==  'TEMP' .OR. &
  TEXT  ==  'NOSI') THEN
  DO WHILE (MORETREND .AND. COUNT  <=  2)
    DISP=86+(COUNT*29)
    CALL MTRTRD(REPORT,POINT1,REPLEN,REXP,MORETREND,DISP)
    COUNT=COUNT+1
  END DO
! POINT TO EXPECTED START POSITION OF RUNWAY DEPOSIT GROUP.
! SKIP ANY SPACES IF FOUND. (NOTE UK STATIONS ONLY).

IFLABEL2: &
  IF (ID(1:2)  ==  'EG') THEN
    POINT1=REPLEN-8
    IF (REPORT(POINT1:POINT1)  ==  ' ') THEN
      POINT1=POINT1+1
      GRPLEN=8
      CALL MTRGRP(REPORT(POINT1:),GRPLEN,NCHAR,CHARR)
      IF (NCHAR  ==  0 .OR. &
          REPORT(POINT1+2:POINT1+5)  ==  'CLRD') THEN
        CEXP(14:)=REPORT(POINT1:POINT1+7)
        REXP(78)=RDLOC
      END IF
    END IF
  END IF IFLABEL2
  GOTO 2
END IF IFLABEL1

! CHECK WHETHER GROUP IS A WINDSHEAR GROUP BEFORE MTRLOC LOCATES THE
! SIZE OF THE NEXT GROUP. (WINDSHEAR GROUPS CONSIST OF MULTIPLE SMALLER
! GROUPS).
! USE MTRSHE TO ASCERTAIN THE REAL GROUP LENGTH.

IFLABEL3: &
IF (REPORT(POINT1:POINT1+1)  ==  'WS' .AND. NGRP  >=  4) THEN
  CALL MTRSHE(REPLEN,REPORT,POINT2,GRPLEN,LREPFL,SHBAD,RWYUSE, &
              RWYDIR,RWYPRL)

! PROVIDED THE MAXIMUM NUMBER OF WIND SHEAR GROUPS HAVE NOT BEEN
! EXPANDED AND THERE ARE NO SYNTAX ERRORS IN THE GROUP, EXPAND THE DATA
! INTO THE ARRAY.

  IF (SHBAD .OR. SHRGRP  >  2) THEN
    GOTO 1
  ELSE
    SHRGRP=SHRGRP+1
    REXP(70+(SHRGRP-1)*4)=RWYDIR
    REXP(71+(SHRGRP-1)*4)=RWYPRL
    REXP(72+(SHRGRP-1)*4)=RWYUSE
    GOTO 1
  END IF

! SOME VISIBILITY GROUPS ARE REPORTED IN STATUTE MILES AND HALF MILES.
! DETERMINE IF THE GROUP IS ONE OF THESE AND SET THE GROUP LENGTH
! BYPASSING MTRLOC.

ELSE IF (NGRP >=  2 .AND. NGRP  <=  7 .AND. &
        REPORT(POINT1+5:POINT1+6)  ==  'SM' .AND. &
        REPORT(POINT1+3:POINT1+3)  ==  '/') THEN
  GRPLEN=7
  POINT2=POINT1+GRPLEN+1

! IF THE GROUP IS NOT A WIND SHEAR OR STATUTE MILES VISIBILITY GROUP,
! USE MTRLOC TO GET THE GROUP LENGTH.

ELSE
  CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
END IF IFLABEL3

! OBTAIN THE CHARACTER CONTENT OF THE GROUP USING MTRGRP AND INCREMENT
! THE GROUP COUNTER TO TOTAL THE NUMBER OF GROUPS FOUND SO FAR.

CALL MTRGRP(REPORT(POINT1:),GRPLEN,NCHAR,CHARR)
NGRP=NGRP+1

! NOW CHECK ON GROUPS EXPECTED WITHIN THE FIRST THREE GROUPS OF A
! METAR REPORT.

IFLABEL4: &
IF (NGRP  <=  3) THEN

! THE FIRST GROUP IS ASSUMED TO BE THE STATION IDENTIFIER. THIS CAN BE
! EITHER A 4 LETTER ICAO (AMERICAN, CANADIAN ARE STORED AS 4 LETTERS) OR
! A 5 FIGURE IDENTIFIER (AUSTRIAN).
! STORE THE IDENTIFIER FOR LATER USE AND GET A NEW GROUP.
! IN THE UNLIKELY EVENT IT IS NOT AN IDENTIFIER, THE REPORT WILL NOT BE
! EXPANDED.

IFLABEL5: &
  IF (NGRP  ==  1) THEN
    IF (GRPLEN  ==  4 .AND. NCHAR  ==  4) THEN
      ID(1:4)=REPORT(POINT1:POINT1+GRPLEN-1)
    ELSE IF (GRPLEN ==  5 .AND. NCHAR  ==  0) THEN
      ID(1:5)=REPORT(POINT1:POINT1+GRPLEN-1)
    ELSE
      GOTO 2
    END IF
    GOTO 1
  END IF IFLABEL5

! CHECK WHETHER GROUP IS A SECOND AMERICAN IDENTIFIER AND ABORT
! EXPANSION IF IT IS. (IT IS VERY LIKELY THE REPORT IS UNEXPANDABLE).
! (NOTE: ONLY CHECK GROUP NUMBER 2 FOR THIS).

IFLABEL6: &
  IF (NGRP  ==  2 .AND. GRPLEN  ==  2 .AND. NCHAR  ==  2) THEN
    IFAIL=8
    GOTO 2

! CHECK WHETHER THE GROUP IS 'AUTO', REPRESENTING AN AUTOMATIC STATION.
! CHECK THE EXPANSION FLAG THEN SET THE EXPANSION VALUE AND GET A NEW
! GROUP.

  ELSE IF (GRPLEN ==  4 .AND. ELEM(1)  ==  0 .AND. &
          REPORT(POINT1:POINT1+3)  ==  'AUTO') THEN
    REXP(79)=0
    ELEM(1)=1
    GOTO 1

! ***** (YY)GGGG(Z) GROUP CHECK ***************************************

! CHECK WHETHER GROUP IS A VALID (YY)GGGG(Z) FORMAT COMBINATION.
! CONVERT REXP(7),HOUR, AND REXP(6),DAY, TO INTEGER FORMAT AND CHECK
! REPORT (YY)GGGG, OF AUSTRIAN (BLOCK 11) REPORTS ONLY, AGAINST THE
! TIME TAKEN FROM THE BULLETIN HEADER. OTHERWISE GET A NEW GROUP.
! A COMPARISON CHECK IS ALREADY MADE BETWEEN THE REPORT (YY)GGGG AND THE
! BULLETIN TIME DURING STORAGE FOR REPORTS RECEIVED FROM OTHER COUNTRIES

  ELSE IF ((GRPLEN ==  5 .OR. GRPLEN  ==  7) .AND. NCHAR  ==  1 &
          .AND. REPORT(POINT1+GRPLEN-1:POINT1+GRPLEN-1)  ==  'Z' &
     .OR. (GRPLEN  ==  4 .OR. GRPLEN  ==  6) .AND. NCHAR  ==  0 &
          .AND. (NGRP  ==  2 .OR. ELEM(1)  ==  1)) THEN
IFLABEL7: &
    IF (ID(1:2)  ==  '11') THEN
      DAY=REXP(6)
      HRS=REXP(7)

! CHECK (YY)GGGG(Z) COMBINATIONS.

IFLABEL8: &
      IF ((GRPLEN  ==  6 .OR. GRPLEN  ==  7) .AND. &
          CHARR(1:4)  ==  'NNNN') THEN
        READ (REPORT(POINT1:POINT1+1),'(I2)') TIME
        IF (TIME  /=  DAY) THEN
          REXP(6)=TIME
        END IF
        READ (REPORT(POINT1+2:POINT1+3),'(I2)') TIME
        IF (TIME  /=  HRS) THEN
          REXP(7)=TIME
        END IF

! CHECK GGGG(Z) COMBINATIONS.

      ELSE IF ((GRPLEN ==  4 .OR. GRPLEN  ==  5) .AND. &
              CHARR(1:2)  ==  'NN' ) THEN
        READ(REPORT(POINT1:POINT1+1),'(I2)') TIME
        IF (TIME  /=  HRS) THEN
          REXP(7)=TIME
        END IF
      END IF IFLABEL8
    END IF IFLABEL7
    GOTO 1
  END IF IFLABEL6
END IF IFLABEL4

! ***** WIND GROUP CHECK **********************************************
! ENSURE THE WIND GROUP HAS NOT ALREADY BEEN EXPANDED.

IFLABEL9: &
IF (ELEM(2)  ==  0) THEN

! CHECK WHETHER THE WIND IS CALM.

IFLABEL10: &
  IF (GRPLEN  ==  5 .AND. NCHAR  ==  0 .AND. &
      REPORT(POINT1:POINT1+4)  ==  '00000') THEN
    REXP(11)=0.
    REXP(12)=0.
    ELEM(2)=1
    GOTO 1

! CHECK OTHER POSSIBILITIES FOR A VALID WIND GROUP (EXCEPT VARIABLE).
! VARIABLE WINDS DEALT WITH WITHIN MTRDDF !2.2

  ELSE IF (GRPLEN >=  7) THEN
    IF (CHARR(GRPLEN-1:GRPLEN)  ==  'YY') THEN
      CALL MTRDDF(REPORT,POINT1,GRPLEN,CHARR,DDD,FFF,FMFMFM)
      IF (DDD  >=  0) THEN
        REXP(11)=DDD
      END IF
      IF (FFF  >=  0) THEN
      REXP(12)=FFF
      END IF
      IF (FMFMFM  >=  0) THEN
        REXP(13)=FMFMFM
      END IF
      ELEM(2)=1
      GOTO 1
    END IF
  END IF IFLABEL10
END IF IFLABEL9

! CHECK WHETHER WIND IS VARIABLE.

IF (GRPLEN  ==  7 .AND. CHARR(1:7)  ==  'NNNYNNN' .AND. &
    REPORT(POINT1+3:POINT1+3)  ==  'V') THEN
!1.6 CHECK TO MAKE SURE THAT CHARACTERS ARE OF CORRECT TYPE
  REXP(15)=FLOAT(IVALUE(REPORT(POINT1:POINT1+2)))
  REXP(14)=FLOAT(IVALUE(REPORT(POINT1+4:POINT1+6)))
  GOTO 1
END IF

! ***** HORIZONTAL VISIBILITY GROUP CHECKS ****************************
! ENSURE VISIBILITY HAS NOT ALREADY BEEN EXPANDED.

IFLABEL11: &
IF (ELEM(3)  ==  0) THEN
  IPT = POINT1 + GRPLEN - 1 ! POSITION OF LAST CHAR IN GROUP
  HVIS = -9999999.0         ! HORIZ VIS NOT YET DECODED

! VISIBILITY GROUPS IN KM. CAN BE 3 OR 4 CHARACTERS ('NKM' OR 'NNKM').

IFLABEL12: &
  IF ((GRPLEN == 3 .OR. GRPLEN == 4) .AND. & ! 3 OR 4 CHARS
      REPORT(IPT-1:IPT) == 'KM') THEN       ! ENDING 'KM'

    VIS = IVALUE(REPORT(POINT1:IPT-2))      ! WHOLE KMS
    IF (VIS >= 0) HVIS = 1000*FLOAT(VIS)    ! CONVERT TO M

! VISIBILITY GROUPS IN STATUTE MILES (SM). CAN BE FROM 3 TO 7
! CHARACTERS IN VARIOUS FORMATS (SEE BELOW) BUT ENDING 'SM'.
! CONVERSION TO METRES IS DONE JUST BEFORE LEAVING IF BLOCK.

  ELSE IF ((GRPLEN >= 3 .AND. GRPLEN <= 7) .AND. & ! 3-7 CHARS
          REPORT(IPT-1:IPT) == 'SM') THEN         ! ENDING 'SM'

! WHOLE MILES: CAN BE 3 OR 4 CHARACTERS ('NSM' OR 'NNSM').

IFLABEL13: &
    IF (GRPLEN == 3 .OR. GRPLEN == 4) THEN

      VIS = IVALUE(REPORT(POINT1:IPT-2))  ! WHOLE MILES
      IF (VIS >= 0) HVIS = FLOAT(VIS)
! FRACTION OF MILE WITH OR WITHOUT INTEGER AMOUNT (7 OR 5 CHARACTERS
! RESPECTIVELY, I.E. 'N I/JSM' FOR N + I/J MILES OR JUST 'I/JSM' FOR
! I/J MILES).  IN THESE CASES, THE 4TH CHARACTER FROM THE END IS '/'.

    ELSE IF (REPORT(IPT-3:IPT-3) == '/') THEN

      I1 = IVALUE(REPORT(IPT-4:IPT-4))  ! NUMERATOR
      I2 = IVALUE(REPORT(IPT-2:IPT-2))  ! DENOMINATOR

      IF (GRPLEN == 7) THEN
        VIS = IVALUE(REPORT(POINT1:POINT1)) ! WHOLE MILES
      ELSE
        VIS = 0  ! NO WHOLE MILES SPECIFIED
      END IF

      IF (I1 > 0 .AND. I2 > 0 .AND. VIS >= 0) & ! GOOD DATA
          HVIS = FLOAT(VIS) + FLOAT(I1)/FLOAT(I2)  ! MILES

! SIXTEENTHS OF A MILE: 6 CHARACTERS ('N/16SM').

    ELSE IF (GRPLEN == 6 .AND. REPORT(IPT-4:IPT) == '/16SM') THEN

      I1 = IVALUE(REPORT(POINT1:POINT1))    ! NUMERATOR
      IF (I1 > 0) HVIS = 0.0625*FLOAT(I1)   ! MILES
    END IF IFLABEL13

! CONVERT HORIZONTAL VISIBILITIES TO METRES.
! (1 STATUTE MILE = 5280 FEET = 5280*0.3048 METRES = 1609.344 METRES.)

    IF (HVIS > 0) HVIS = 1609.344*HVIS

! ALL NUMERIC 4-CHARACTER VISIBILITY GROUP. NO CONVERSION REQUIRED.
! '9999' REPRESENTS 10KM OR MORE - CONVERTED TO 10000 METRES HERE.

  ELSE IF (GRPLEN ==  4 .AND. NCHAR  ==  0) THEN
    VIS = IVALUE(REPORT(POINT1:POINT1+3))  ! METRES
    IF (VIS == 9999) VIS = 10000
    HVIS = FLOAT(VIS)

! CHECK FOR THE PRESENCE OF NDV IN THE HORZ VIS GROUP. SEE
! FM 15 XIV 15.6.1 FOR MORE DETAILS

  ELSE IF (GRPLEN <= 7.AND.REPORT(IPT-2:IPT) == 'NDV') THEN
    VIS=IVALUE(REPORT(POINT1:IPT-3))
    IF (VIS == 9999) VIS = 10000
    HVIS = FLOAT(VIS)
!
! SET IPT TO ZERO IF NOT A VISIBILITY GROUP

  ELSE
    IPT = 0  ! INDICATES NOT VISIBILITY GROUP
  END IF IFLABEL12

! UPDATE REXP AND ELEM ARRAYS, AND RETURN FOR NEXT GROUP
! AS WHEN THERE IS ONLY ONE GROUP IT IS DIFFICULT TO TELL
! IF THIS IS MINIMUM (BEFORE NOV 2004) OR PREVAILING VISIBILITY
! (AFTER 25TH NOV 2004) BOTH ARE SET THE SAME.

  IF (IPT > 0) THEN   ! VISIBILITY GROUP WAS FOUND
    REXP(17) = HVIS
    REXP(173) = HVIS
    ELEM(3)=1
    GOTO 1
  END IF
END IF IFLABEL11

! EXPAND A VISIBILITY GROUP WITH A MINIMUM OR MAXIMUM DIRECTION OF
! N, E, S OR W.

IFLABEL14: &
IF (GRPLEN  ==  5 .AND. CHARR(5:5)  ==  'Y' .AND. &
    NCHAR  ==  1) THEN
IFLABEL15: &
  IF (ELEM(3)  ==  0 .OR. ELEM(4)  ==  0) THEN
    VIS = IVALUE(REPORT(POINT1:POINT1+3))
    IF (REPORT(POINT1+4:POINT1+4)  ==  'N') THEN
      DIR=360
    ELSE IF (REPORT(POINT1+4:POINT1+4) ==  'E') THEN
      DIR=90
    ELSE IF (REPORT(POINT1+4:POINT1+4) ==  'S') THEN
      DIR=180
    ELSE IF (REPORT(POINT1+4:POINT1+4) ==  'W') THEN
      DIR=270
    END IF
IFLABEL16: &
    IF (ELEM(3)  ==  0) THEN
      REXP(16)=DIR
      REXP(17)=VIS
      REXP(173)= -9999999.0
      ELEM(3)=1
    ELSE IF (ELEM(4) ==  0) THEN

! A change 25th November 2005 to the reporting of Metars meant that
! the first group would always be prevailing visibility, without
! a direction, and second group would be the minimum visibility
! with a direction. However this routine still needs to decode
! metars decoded the old way so a check is to see if a direction
! for minimum visibility has already been set. If so then this is
! coded the old way and the second visibility group s a maximum.
! If a direction had not been set for the first visibility then
! it is assumed this metar is coded the new way with the first
! group being the prevailing visibility and second group being
! minimum visibility.
      IF (REXP(16) >= 0.0) THEN  ! max vis
        REXP(18)=DIR
        REXP(19)=VIS
      ELSE                       ! min vis
        REXP(16)=DIR
        REXP(17)=VIS
      END IF

      ELEM(4)=1
    END IF IFLABEL16
  END IF IFLABEL15
  GOTO 1

! Expand a visibility group with a minimum or maximum direction of
! NE, SE, SW or NW.

ELSE IF (GRPLEN ==  6 .AND. NCHAR  ==  2 .AND. &
        CHARR(5:6)  ==  'YY') THEN
IFLABEL17: &
  IF (ELEM(3)  ==  0 .OR. ELEM(4)  ==  0) THEN  !2.4
    IF (REPORT(POINT1+4:POINT1+5)  ==  'NE') THEN
      DIR=45
    ELSE IF (REPORT(POINT1+4:POINT1+5) ==  'SE') THEN
      DIR=135
    ELSE IF (REPORT(POINT1+4:POINT1+5) ==  'SW') THEN
      DIR=225
    ELSE IF (REPORT(POINT1+4:POINT1+5) ==  'NW') THEN
      DIR=315
    ELSE
      GOTO 1
    END IF
    VIS = IVALUE(REPORT(POINT1:POINT1+3))
IFLABEL18: &
    IF (ELEM(3)  ==  0) THEN
      REXP(16)=DIR
      REXP(17)=VIS
      REXP(173)= -9999999.0
      ELEM(3)=1
    ELSE IF (ELEM(4) ==  0) THEN

! A change 25th November 2005 to the reporting of Metars meant that
! the first group would always be prevailing visibility, without
! a direction, and second group would be the minimum visibility
! with a direction. However this routine still needs to decode
! metars decoded the old way so a check is to see if a direction
! for minimum visibility has already been set. If so then this is
! coded the old way and the second visibility group s a maximum.
! If a direction had not been set for the first visibility then
! it is assumed this metar is coded the new way with the first
! group being the prevailing visibility and second group being
! minimum visibility.

      IF (REXP(16) >= 0.0) THEN  ! max vis
        REXP(18)=DIR
        REXP(19)=VIS
      ELSE                       ! min vis
        REXP(16)=DIR
        REXP(17)=VIS
      END IF

      ELEM(4)=1
    END IF IFLABEL18
  END IF IFLABEL17
  GOTO 1

! ***** GENERAL WEATHER GROUP CHECKS **********************************

! Check whether group is 'CAVOK' and no group of this type has already
! been expanded.
! If other general weather already expanded into the array, then add
! the value for 'CAVOK' to the value in the array. Otherwise set value
! for 'CAVOK'.

ELSE IF (GRPLEN ==  5 .AND. ELEM(5)  ==  0 .AND. &
        REPORT(POINT1:POINT1+4)  ==  'CAVOK') THEN
  IF (REXP(20)  <  0) THEN
    REXP(20)=2
  ELSE
    REXP(20)=REXP(20)+2
  END IF
  ELEM(5)=1
  GOTO 1

! Check whether group is 'SKC' and no group of this type has already
! been expanded.
! If other general weather already expanded into the array, then add
! the value for 'SKC' to the value in the array. Otherwise set value
! for 'SKC'.

ELSE IF (GRPLEN ==  3 .AND. ELEM(6)  ==  0 .AND. &
        REPORT(POINT1:POINT1+2)  ==  'SKC') THEN
  IF (REXP(20)  <  0) THEN
    REXP(20)=4
  ELSE
    REXP(20)=REXP(20)+4
  END IF
  ELEM(6)=1
  GOTO 1

! Check whether group is 'NSW' and no group of this type has already
! been expanded.
! If other general weather already expanded into the array, then add
! the value for 'NSW' to the value in the array. Otherwise set value
! for 'NSW'.

ELSE IF (GRPLEN ==  3 .AND. ELEM(7)  ==  0 .AND. &
        REPORT(POINT1:POINT1+2)  ==  'NSW') THEN
  IF (REXP(20)  <  0) THEN
    REXP(20)=8
  ELSE
    REXP(20)=REXP(20)+8
  END IF
  ELEM(7)=1
  GOTO 1

! ***** CURRENT AND RECENT WEATHER GROUP CHECKS ***********************

! First check for GRPLEN < 2 to avoid run time errors when checking
! CHARR(GRPLEN-1:GRPLEN) in the ELSE IF statement which follows

 ELSE IF (GRPLEN.LT.2) THEN
        CONTINUE

! Check for all types of weather group W'W'WW, WWW'W', W'W' and REWW

ELSE IF (((GRPLEN >=  4 .AND. NCHAR  ==  GRPLEN-2 .AND.      &
          CHARR(GRPLEN-1:GRPLEN)  ==  'NN') .OR.             &
         (GRPLEN  >=  4 .AND. NCHAR  ==  GRPLEN-2 .AND.      &
          CHARR(1:4)  ==  'NNYY') .OR.                       &
         (GRPLEN  ==  NCHAR .AND. GRPLEN  >=  2 .AND.        &
          CHARR(GRPLEN-1:GRPLEN)  /=  '//') .OR.             &
         (GRPLEN  ==  2 .AND. NCHAR  ==  0)) .AND.           &
          REPORT(POINT1:POINT1+1)  /=  'WS') THEN
! Read numeric weather code value for W'W'WW groups.

IFLABEL19: &
  IF (CHARR(GRPLEN-1:GRPLEN)  ==  'NN') THEN
    WW = IVALUE(REPORT(POINT1+GRPLEN-2:POINT1+GRPLEN-1))
    GRPLEN=GRPLEN-2

! Read numeric weather code value for WWW'W' groups.
! Move pointer past WW ready for call to MTRWWR.

  ELSE IF (CHARR(1:4) ==  'NNYY') THEN
    WW = IVALUE(REPORT(POINT1:POINT1+1))
    POINT1=POINT1+2
    GRPLEN=GRPLEN-2
  END IF IFLABEL19

! Otherwise the default is for W'W' groups.

! Check whether numeric value already expanded into array. If not, then
! copy value to array and set flag.

  IF (ELEM(8)  ==  0) THEN
    REXP(44)=WW
    ELEM(8)=1
  END IF

! Set starting value in flag array.

  ESTAR=9

! Call metar ww location and decoding routine to expand W'W' without
! WW, if applicable.

  CALL MTRWWR(POINT1,REPORT,GRPLEN,ELEM,ESTAR,WDWD,REWW,WWBAD)

! If valid weather group, then insert weather into expansion array.

IFLABEL20: &
  IF (.NOT.WWBAD) THEN

! Insert present weather into expansion array.

    IF (ELEM(9)  >  0) THEN
      IF (WW  <  0) THEN
        REXP(35+(ELEM(9)-1)*3)=WDWD(1+(ELEM(9)-1)*3)
      END IF
      REXP(36+(ELEM(9)-1)*3)=WDWD(2+(ELEM(9)-1)*3)
      REXP(37+(ELEM(9)-1)*3)=WDWD(ELEM(9)*3)
    END IF

! Insert recent weather information into expansion array.
! The real array values are not contigously displaced because the code
! was changed since the original software was written.

    IF (ELEM(10)  >  0) THEN
      REXP(80+(ELEM(10)-1)*2)=REWW(1+(ELEM(10)-1)*2)
      REXP(81+(ELEM(10)-1)*2)=REWW(2+(ELEM(10)-1)*2)
      IF (ELEM(10)  ==  1) THEN
        REXP(68)=REXP(80)
        REXP(69)=REXP(81)
      END IF
    END IF
    GOTO 1
  END IF IFLABEL20

! ***** VERTICAL VISIBILITY GROUP CHECKS ******************************
! If the vertical visibility height is missing and no cloud groups are
! reported, then the value returned will remain as missing. Ensure
! the vertical visibility has not already been expanded.

ELSE IF (REPORT(POINT1:POINT1+1) ==  'VV' .AND. GRPLEN  ==  5 &
        .AND. (CHARR(3:5)  ==  '///' .OR. CHARR(3:5)  ==  'NNN') &
        .AND. ELEM(11)  ==  0) THEN
  IF (CHARR(3:5)  ==  '///') THEN
    IF (ELEM(14)  >  0 .OR. ELEM(15)  >  0) THEN
      GOTO 1
    ELSE
      REXP(45)=9
      ELEM(14)=ELEM(14)+1
      GOTO 1
    END IF
  END IF

!1.6 Check if numeric values
  REXP(63)=FLOAT(IVALUE(REPORT(POINT1+2:POINT1+4)))
  IF (REXP(63) /= -9999999.0) THEN
    REXP(63)=REXP(63)*METRES
  END IF
  ELEM(11)=1
  GOTO 1
END IF IFLABEL14

! ***** PRESSURE GROUP CHECKS *****************************************
! Ensure the pressure group has not already been expanded.

IFLABEL21: &
IF (ELEM(12)  ==  0) THEN

! There are various possible formats for a valid pressure group. Before
! expansion, the pressure in either millibars orinches is converted
! to pascals.
! The group may consist of either a 'Q' or an 'A' and 4 numerics.

IFLABEL22: &
  IF (GRPLEN  ==  5 .AND. NCHAR  ==  1 .AND. &
      (REPORT(POINT1:POINT1)  ==  'Q' .OR. &
       REPORT(POINT1:POINT1)  ==  'A')) THEN
IFLABEL23: &
    IF (REPORT(POINT1:POINT1)  ==  'Q') THEN

!1.6 Check that values are numeric
      QNH = FLOAT(IVALUE(REPORT(POINT1+1:POINT1+4)))
      IF (QNH /= -9999999.0) THEN
         QNH=QNH*100.
      END IF

    ELSE IF (REPORT(POINT1:POINT1+1) ==  'A2' .OR. &
            REPORT(POINT1:POINT1+1)  ==  'A3') THEN

!1.6 check that all numerics where expected.
        INCHES = IVALUE(REPORT(POINT1+1:POINT1+2))
        IF (INCHES == -9999999) THEN
          QNH = -9999999.0
        ELSE
          QNH=INCHES*100
          INCHES = IVALUE(REPORT(POINT1+3:POINT1+4))
          IF (INCHES == -9999999) THEN
            QNH = -9999999.0
          ELSE
            QNH=(QNH+INCHES)*INTOMB
          END IF
        END IF
    END IF IFLABEL23
    REXP(66)=QNH
    ELEM(12)=1
    GOTO 1

! Or the group may consist of all numerics.

  ELSE IF (GRPLEN ==  4 .AND. NCHAR  ==  0) THEN
!1.6 Check that REPORT(POINT1:PPOINT1+3) contains numerics
    QNH = FLOAT(IVALUE(REPORT(POINT1:POINT1+3)))
    IF (QNH /= -9999999.0) THEN
      REXP(66)=QNH*100
    ELSE
      REXP(66) = QNH
    END IF

    ELEM(12)=1
    GOTO 1

! Or the group may consist of a 'Q' and 3 numerics.

  ELSE IF (GRPLEN ==  4 .AND. REPORT(POINT1:POINT1)  ==  'Q' &
          .AND. NCHAR  ==  1) THEN
!1.6 Check that REPORT(POINT1+1:POINT1+3) is indeed numeric
    QNH = FLOAT(IVALUE(REPORT(POINT1+1:POINT1+3)))
    IF (QNH /= -9999999.0) THEN
      REXP(66)=QNH*100
    ELSE
      REXP(66) = QNH
    END IF
    ELEM(12)=1
    GOTO 1

! Or the group may consist of 3 numerics.

  ELSE IF (GRPLEN ==  3 .AND. NCHAR  ==  0) THEN
!1.6 Check that REPORT(POINT1:POINT1+2)does include numeric values
    QNH = FLOAT(IVALUE(REPORT(POINT1:POINT1+2)))
    IF ((QNH /= -9999999.0).AND.  &
        (QNH >= 900.0)) THEN
      REXP(66)=QNH*100
      ELEM(12)=1
    ELSE
      REXP(66)= -9999999.0
    END IF
    GOTO 1
  END IF IFLABEL22
END IF IFLABEL21

! ***** TEMPERATURE & DEW POINT GROUP CHECKS **************************
! Ensure temperature group has not already been expanded.

IFLABEL24: &
IF (ELEM(13)  ==  0) THEN

! If the dew point is missing it's value in the report should be
! replaced by '//'.

! Check 5 figure temperature group. Both dry bulb and dew point are
! positive.

IFLABEL25: &
  IF (GRPLEN  ==  5 .AND. (CHARR(1:5)  ==  'NN/NN' .OR. &
      CHARR(1:5)  ==  'NN///')) THEN
    TEMP = IVALUE(REPORT(POINT1:POINT1+1))
    IF (TEMP == -9999999) THEN
      REXP(64)=TEMP
    ELSE
      REXP(64)=TEMP+KELVIN
    END IF

    IF (CHARR(4:5)  ==  'NN') THEN
      TEMP = IVALUE(REPORT(POINT1+3:POINT1+4))
      IF (TEMP == -9999999) THEN
        REXP(65) = TEMP
      ELSE
        REXP(65)=TEMP+KELVIN
      END IF

      ELEM(13)=1
      GOTO 1
    END IF

! Check 6 figure temperature group. Dry bulb is positive but dew point
! is negative.

  ELSE IF (GRPLEN ==  6 .AND. CHARR(1:6)  ==  'NN/YNN') THEN
    TEMP = IVALUE(REPORT(POINT1:POINT1+1))
    IF (TEMP == -9999999) THEN
      REXP(64)=TEMP
    ELSE
      REXP(64)=TEMP+KELVIN
      TEMP = IVALUE(REPORT(POINT1+4:POINT1+5))
      IF (TEMP == -9999999) THEN
        REXP(65)=TEMP
      ELSE
        REXP(65)=-TEMP+KELVIN
      END IF
    END IF

    ELEM(13)=1
    GOTO 1

! Check 7 figure temperature group. Both dry bulb and dew point are
! negative.

  ELSE IF (GRPLEN ==  7 .AND. CHARR(1:7)  ==  'YNN/YNN') THEN
    TEMP = IVALUE(REPORT(POINT1+1:POINT1+2))
    IF (TEMP == -9999999) THEN
       REXP(64)=TEMP
    ELSE
      REXP(64)=-TEMP+KELVIN
      TEMP = IVALUE(REPORT(POINT1+5:POINT1+6))
      IF (TEMP == -9999999) THEN
        REXP(65)=TEMP
      ELSE
        REXP(65)=-TEMP+KELVIN
      END IF
    END IF

    ELEM(13)=1
    GOTO 1
  END IF IFLABEL25
END IF IFLABEL24

! ***** RUNWAY VISUAL RANGE GROUPS ************************************
! Ensure the maximum number of RVR's to be expanded is not exceeded.

IFLABEL26: &
IF (NCHAR  <  GRPLEN .AND. REPORT(POINT1:POINT1)  ==  'R' &
    .AND. ELEM(16)  <=  1) THEN
  CALL MTRRWY(POINT1,REPORT,GRPLEN,CHARR,RVRDIR,RVRTEND,   &
              RVRPRL,RVROPRN,RVRMIN,RVROPRX,RVRMAX)
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

ELSE IF (GRPLEN ==  6 .AND. NCHAR  <  GRPLEN) THEN
IFLABEL27: &
  IF (ELEM(14)  <=  3 .OR. ELEM(15)  <=  3) THEN

! New cloud group processing starts here.

IFLABEL28: &
    IF (CHARR(1:3)  ==  'YYY' .AND. ELEM(14)  <=  3) THEN
      ESTAR=14
      CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,  &
                  CLDAMT,CLDTYPE,CLDHT,VVIS)
      REXP(45+(ELEM(ESTAR)-1)*3)=CLDAMT
      REXP(46+(ELEM(ESTAR)-1)*3)=CLDTYPE
      REXP(47+(ELEM(ESTAR)-1)*3)=CLDHT
      REXP(63)=VVIS
      GOTO 1

! Old cloud group processing starts here.

    ELSE IF (CHARR(1:3) ==  'NYY' .OR. CHARR(1:3)  ==  'N//') THEN
IFLABEL29: &
      IF (REPORT(POINT1+1:POINT1+2)  ==  'CB' .AND. &
          ELEM(15)  <=  3) THEN
        ESTAR=15
        CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,  &
                    CLDAMT,CLDTYPE,CLDHT,VVIS)
        REXP(54+(ELEM(ESTAR)-1)*3)=CLDAMT
        REXP(55+(ELEM(ESTAR)-1)*3)=CLDTYPE
        REXP(56+(ELEM(ESTAR)-1)*3)=CLDHT
        REXP(63)=VVIS
        GOTO 1
      ELSE
        ESTAR=14
        CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,  &
                    CLDAMT,CLDTYPE,CLDHT,VVIS)
        REXP(45+(ELEM(ESTAR)-1)*3)=CLDAMT
        REXP(46+(ELEM(ESTAR)-1)*3)=CLDTYPE
        REXP(47+(ELEM(ESTAR)-1)*3)=CLDHT
        REXP(63)=VVIS
        GOTO 1
      END IF IFLABEL29
    END IF IFLABEL28
!1.7 Else if another cloud reported that should not be then
!1.7      move onto the next group.
  ELSE IF (CHARR(1:3) == 'YYY'.AND. &
          (ELEM(14) > 3.OR.ELEM(15) > 3)) THEN
    GOTO 1
  END IF IFLABEL27

! Search for cumulus cloud groups in the new code. Towering cumulus or
! cumulonimbus can be reported, by addition of 'TCU' or 'CB' behind the
! normal group of 6 characters.

ELSE IF (GRPLEN >=  8 .AND. ELEM(15)  <=  3 .AND. &
       (REPORT(POINT1+GRPLEN-3:POINT1+GRPLEN-1)  ==  'TCU' .OR. &
        REPORT(POINT1+GRPLEN-2:POINT1+GRPLEN-1)  ==  'CB')) THEN
  ESTAR=15
  CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,CLDAMT, &
              CLDTYPE,CLDHT,VVIS)
  REXP(54+(ELEM(ESTAR)-1)*3)=CLDAMT
  REXP(55+(ELEM(ESTAR)-1)*3)=CLDTYPE
  REXP(56+(ELEM(ESTAR)-1)*3)=CLDHT
  GOTO 1

! *********************************************************************
! Capture the NCD and / or NSC groups.
! *********************************************************************
ELSE IF(GRPLEN == 3.AND.ELEM(14) == 0.AND.ELEM(15) == 0) THEN
 IF(REPORT(POINT1:POINT1+GRPLEN) == 'NSC'.OR. &
   REPORT(POINT1:POINT1+GRPLEN) == 'NCD') THEN
    ESTAR=14
    CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,CLDAMT, &
                CLDTYPE,CLDHT,VVIS)
    REXP(45+(ELEM(ESTAR)-1)*3)=CLDAMT
    REXP(46+(ELEM(ESTAR)-1)*3)=CLDTYPE
    REXP(47+(ELEM(ESTAR)-1)*3)=CLDHT
 END IF
 GOTO 1
! ***** RUNWAY DEPOSIT GROUP ******************************************

! Search for the runway deposit group of 8 figures which is reported in
! the old code for UK stations and may be reported in the new code as
! an 8-figure group, no characters.

ELSE IF (GRPLEN ==  8 .AND. (NCHAR  ==  0 .OR. &
        REPORT(POINT1+2:POINT1+5)  ==  'CLRD')) THEN
  CEXP(14:)=REPORT(POINT1:POINT1+7)
  REXP(78)=RDLOC
  GOTO 1
ELSE
  GOTO 1
END IF IFLABEL26


2 CONTINUE


! 2   RETURN
RETURN
END SUBROUTINE MTREXP
