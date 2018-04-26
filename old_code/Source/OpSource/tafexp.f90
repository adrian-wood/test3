SUBROUTINE TAFEXP(REPORT,REPLEN,REXP,NAMD,FAIL)          !2.0,9

!-----------------------------------------------------------------------
!
!   PROGRAM     : TAFEXP
!
!   PURPOSE     : To decode a TAF report.
!                  Look for identifier, 6issue time,Y period & then
!                 groups in any order within sections, calling TAFSECT
!                 to recognise the start of a new change section.
!                  Stop expansion if a group which should occur only
!                 once in a section is repeated, assuming that a
!                 section indicator has been missed, or if two
!                 consecutive groups are unrecognised (& can't be '
!                 joined to make up a split group).  Allow 1 Cb & 3
!                 non-Cb cloud groups per section.  Skip any single
!                 unrecognised group such as QNH.
!
!   DESCRIPTION : Groups delimited by spaces are found by MTRLOC.
!                 (But visiblities in statute miles have spaces
!                 between the whole miles & the fraction, so need
!                 special treatment.)
!                 MTRGRP then sees which characters are figures,
!                 and recognition is based on group length,
!                 initial & final letters, and figure positions.
!
!  CALLED BY    : TFMRET
!
!  CALLS        : MTRLOC  to delimit a group
!                 MTRGRP  to find letter/figure pattern of group
!                 TAFCNL  to find cancellation groups
!                 TAFSECT to delimit change sections
!                 MTRWWR  to decode weather groups
!                 MTRDDF  to decode wind groups
!                 MTRCCC  to decode cloud groups
!                 TAFVISM to convert vis in statute miles
!                 IVALUE  to convert figures to a number
!
!  ARGUMENTS    : 1 REPORT - TAF REPORT TO BE DECODED
!                    (in string long enough to keep copy at end in case
!                     report is changed when a group is not recognised)
!                 2 REPLEN - REPORT LENGTH
!                 3 REXP   - EXPANDED/DECODED VALUES ARRAY
!                 4 NAMD   - AMENDMENT COUNT                       !9
!                 5 FAIL   - ERROR FLAG
!                       =4 if two consecutive groups not recognised
!                       =8 if start or start of change section corrupted
!                      =12 if >1 e.g. wind group in section
!                      =16 of >3 non-Cb cloud groups in section
!                      =20 if clouds not in order FEW, SCT, BKN, OVC
!
! Expansion monitoring array: set or incremented when element found.
! (ELEM(1) & ELEM(2) can be >1 - used to calculate subscripts)
!     1   weather (w'w')
!     2   cloud (excluding Cb & tCu)
!     3   convective cloud (Cb & tCu)
!     4   horizontal visibility
!     5   wind
!    (6-7 not used)
!     8   icing
!     9   turbulence
!    10   air temperature
!    11   vertical visibility
!    12   forecast period
!    13   time of origin
!
! Expansion array (REXP) layout:
! (60 values plus 30 per change section; 60+30*5=210)
!     1      ICAO identifier (set by TAFINT)
!     2-3    latitude & longitude
!     4-8    year, month, day, hour, minute
!     12     day of forecast period
!  (14,15,18 bulletin details set by TAFINT)
!    19-20   start & end hours of forecast period
!    21-23   wind direction & speed (m/s), max gust (m/s)
!     24     general weather (CAVOK,NSW,SKC,NSC)
!    25-33   weather (intensity/descriptor/code)*3
!    34-42   cloud (amount/type/base height)*3
!    43-45   convective cloud amount/type/base
!     46     Horizontal Visibility
!     47     minimum vertical visibility
!    48-49   air temperature preceded by hour
!    50-52   icing: height, depth & severity
!    53-56   turbulence: severity, height, depth & frequency
!     57     present weather
!    58-59   minimum air temperature preceded by hour
!     60     number of change sections
!     61     Forecast End Day
!     62  Forecast Temp Start Day
!     63     Forecast Temp end Day
!     64     Cancellation Indicator
!     65     Amendment Indicator
! Then 40 elements for each 5 change section: 66 - 105,106 - 145 and so on
! The layout of the first change section is:
!     66     Change period flags
!     67  Change period start hour
!     68  Change period end hour
!     69     probability of change
!     70     wind direction
!     71     wind speed
!     72     wind gust
!     73     general weather if >0, vertical vis if <0
!    74-82   weather (intensity/descriptor/code)*3
!    83-91   cloud (amount/type/base height)*3
!    92-94   convective cloud amount/type/base
!     95     minimum horizontal visibility
!     96  Change period start day
!     97  Change period end day
!     98     Forecast Air Temp Start Day
!     99     Forecast Air Temp end Day
!     100    Change Period Start Minute
!     101    Change Period Stop Minute
!     102 - 105 Not Used
! REVISION INFO :
!
! $Workfile: tafexp.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 06/04/2011 15:48:51$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         06/04/2011 15:48:51    Alison Weir     Amend
!       IF to avoid out of range error – added HVISTEST
!  4    MetDB_Refresh 1.3         23/11/2010 09:51:05    Stan Kellett
!       Removed IVALUE declaration as function declared in mod file.
!  3    MetDB_Refresh 1.2         18/11/2010 16:26:58    Brian Barwell
!       Extensive changes to IF tests and other improvements after testing.
!  2    MetDB_Refresh 1.1         10/11/2010 14:57:35    John Norton     MetDB
!       Refresh: Updated after review comments 
!  1    MetDB_Refresh 1.0         04/11/2010 13:25:18    John Norton     MetDB
!       Refresh batch 8 before reviewing.
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

USE mtrloc_mod
USE mtrgrp_mod
USE tafcnl_mod
USE tafsect_mod
USE mtrwwr_mod
USE mtrddf_mod
USE mtrccc_mod
USE tafvism_mod
USE ivalue_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(INOUT) ::  REPORT ! The report to be expanded/d    ecoded.
INTEGER,      INTENT(INOUT) ::  REPLEN ! Total length of the report.
REAL,         INTENT(INOUT) ::  REXP(:) ! Expansion array.
INTEGER,      INTENT(INOUT) ::  NAMD ! Number of amendment   !9
INTEGER,      INTENT(INOUT) ::  FAIL ! Error value returned to MDB.

! Local declarations:

REAL,        PARAMETER ::  KELVIN = 273 ! Value used for temperature conversion.
                                        ! Temperature of 0C in Kelvin.
INTEGER,     PARAMETER ::  METRES = 30  ! Hundreds of feet to metres conversion
                                        ! Height multiplication factor.

INTEGER      ::  BADRUN   ! Number of consecutive bad groups
INTEGER      ::  BADLEN   ! Length of first bad group
INTEGER      ::  C_RC     ! TAF AMD Indicator
INTEGER      ::  CLDAMT   ! Amount of cloud
INTEGER      ::  CLDTYPE  ! Cloud identity.
INTEGER      ::  DAY      ! Forecast Temp Day
INTEGER      ::  ELEM(13) ! Element array and counter.
INTEGER      ::  ESTAR    ! Starting position within array ELEM.
INTEGER      ::  FACT     ! Expansion array factor
INTEGER      ::  GRPLEN   ! Length of group being expanded.
INTEGER      ::  HOUR     ! Hour of origin or forecast start/end
INTEGER      ::  ICELV    ! Height of icing.
INTEGER      ::  LOOP     ! General loop variable.
INTEGER      ::  NCHAR    ! Number of non-figures in group
INTEGER      ::  OFFSET   ! To calculate array subscripts
INTEGER      ::  OKTAS(0:13) ! Equivalents of CLDAMT from 8 to 13
INTEGER      ::  OLDAMT   ! Previous CLDAMT in section
INTEGER      ::  POINT1   ! Pointer to current group in report.
INTEGER      ::  POINT2   ! Pointer to next group in report.
INTEGER      ::  RC       ! Return code from TAFSECT
INTEGER      ::  SECTION  ! Section of report being expanded
INTEGER      ::  SWITCH   ! TAF Format indicator
INTEGER      ::  TEMP     ! Temperature
INTEGER      ::  TURBIN   ! Turbulence severity.
INTEGER      ::  TURBLV   ! Height of turbulence.
INTEGER      ::  WXFLAG   ! Power of 2 corresponding to CAVOK etc

CHARACTER(20) :: CHARR ! Set by MTRGRP: N if figure, Y if not.
CHARACTER(20) :: GROUP ! Group from report for checks
CHARACTER(4)  :: ID     ! Station ID character string.
CHARACTER(62) :: PAIRS ! Pairs of letters used in weather group

LOGICAL      ::  CHANGE   ! Flag set if valid trend group found.
LOGICAL      ::  LBAD     ! Bad significant weather group.
LOGICAL      ::  LREPFL   ! Set (by MTRLOC) if end of report
LOGICAL      ::  CLOUDTEST! Flag for testing for Cb cloud group
LOGICAL      ::  HVISTEST ! Flag for testing for horizontal vis group
LOGICAL      ::  TIMETEST ! Flag for testing for date/time group
LOGICAL      ::  WINDTEST ! Flag for testing for wind/gust group

REAL         ::  CLDHT    ! Cloud base height (in metres)
REAL         ::  DDD      ! Wind direction.
REAL         ::  FFF      ! Wind force
REAL         ::  FMFMFM   ! Gust force
REAL         ::  REWW(6)  ! Not used (for consistency with METARs)
REAL         ::  VVVV     ! Horizontal visibility.
REAL         ::  VVIS     ! Vertical visibility.
REAL         ::  WDWD(9)  ! Present weather array for MTRWWR

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! The list of leter pairs is as in MTRWWR with RE, VC & XX at start.
!     (REvcXXmiBCdrBLshTSfzPRdzRAsnSGicPLgrGSbrFGfuVAduSAhzPOsqFCssDS)

DATA PAIRS/ &
'REVCXXMIBCDRBLSHTSFZPRDZRASNSGICPLGRGSBRFGFUVADUSAHZPOSQFCSSDS'/
DATA OKTAS/8*0,8,0,0,2,4,1/  ! oktas for CLDAMT=...8,,,11,12,13

FACT=40                   ! Expansion factor
FAIL=0                    ! no error
ID=' '
LBAD=.FALSE.              ! Bad present weather group flag.
LREPFL=.FALSE.            ! End of report flag.
SECTION=0
SWITCH=0                  ! Expansion section, (5 Maximum).
POINT2=1                  ! Position of current group in report.
REXP(60)=0                ! Set number of sections to zero   !9

! Keep copy of report at end of long string passed to TAFEXP       !2.1
! (unless report is more than half length of string!)              !2.1

IF (REPLEN < LEN(REPORT)/2) THEN                             !2.1
  REPORT(LEN(REPORT)-REPLEN+1:)=REPORT(:REPLEN)              !2.1
END IF                                                       !2.1

! Initialise expansion monitoring array to missing.

DO LOOP=1,13
  ELEM(LOOP)=0
END DO
CLDAMT=0
OLDAMT=0

! ********************* IDENTIFIER (first group) **********************

! See if first group is identifier (4 letters).  If not, give up.
! MTRLOC delimits a group, MTRGRP sees how many non-figures.
! (The ident is only skipped here; it's already in the expansion.)

POINT1=POINT2
CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
CALL MTRGRP(REPORT(POINT1:),GRPLEN,NCHAR,CHARR)

IF (GRPLEN == 4 .AND. NCHAR == 4) THEN
  ID=REPORT(POINT1:POINT1+3)
ELSE
  FAIL=8
  RETURN
END IF

! ********************* TIME OF ISSUE & PERIOD ************************

! The following group(s) must be time of issue and/or period.
! Assume time of issue is always 6 figures (day, hour, minute) with or
! without a final Z.  Period is 4 or 6 figures, 2 hours with or without
! a preceding day.  The first hour's range is 0-23, the second's 1-24.

POINT1=POINT2
CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
GROUP=REPORT(POINT1:POINT1+GRPLEN-1)
CALL MTRGRP(REPORT(POINT1:),GRPLEN,NCHAR,CHARR)

! Set amendment (AMD) and cancellation indicators (CNL).           !9
                                                             !9
IFLABEL1: &
IF (NAMD > 0) THEN                                           !9
  REXP(65)=1                                                 !9
  CALL TAFCNL(REPORT,REPLEN,C_RC)                            !9
  IF (C_RC == 1) THEN                                        !9
    REXP(64)=1                                               !9
  ELSE                                                       !9
    REXP(64)=0                                               !9
  END IF                                                     !9
ELSE                                                         !9
  REXP(64)=0                                                 !9
  REXP(65)=0                                                 !9
END IF IFLABEL1                                              !9

! Checks for the Time of Issue. If the group is 6 figures without
! any chars assume that the format to be DDHHMM.Alternativly
! assume the format to be DDHHZIF

IFLABEL2: &
IF ((GRPLEN == 5 .AND. NCHAR == 1 .AND. GROUP(5:5) == 'Z') .OR. &
    (GRPLEN == 7 .AND. NCHAR == 1 .AND. GROUP(7:7) == 'Z')) THEN
  IF (GRPLEN == 7 .AND. GROUP(1:2) <= '31' .AND. &
      GROUP(3:4) < '24' .AND. GROUP(5:6) < '60') THEN
     REXP(6)=IVALUE(GROUP(1:2))        ! day
     REXP(7)=IVALUE(GROUP(3:4))        ! hour
     REXP(8)=IVALUE(GROUP(5:6))        ! minute
     POINT1=POINT2
     CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
     GROUP=REPORT(POINT1:POINT1+GRPLEN-1)
     CALL MTRGRP(GROUP,GRPLEN,NCHAR,CHARR)
     ELEM(13)=1
  END IF

  IF (GRPLEN == 5 .AND. GROUP(1:2) <= '31'.AND. &
      GROUP(3:4) < '24') THEN
     REXP(6)=IVALUE(GROUP(1:2))      ! Day
     REXP(7)=IVALUE(GROUP(3:4))        ! Hour
     POINT1=POINT2
     CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
     GROUP=REPORT(POINT1:POINT1+GRPLEN-1)
     CALL MTRGRP(GROUP,GRPLEN,NCHAR,CHARR)
     ELEM(13)=1
  END IF
ELSE
  FAIL=8
  RETURN
END IF IFLABEL2

! As a result of the changes introduced as ICAO Annex 3
! Amendment 74, the date / time format of the TAF validity
! period have changed. This procedure will be expected to
! process bulletins in both the new and old format.

! If a 4 or 6 or 9 figure group follows the issue group assume
! it is the validity period. The forecast Validity period
! will be of the format HH HH or DD HH HH or DD HH / DD HH.

IFLABEL3: &
IF (GRPLEN == 4.OR.GRPLEN == 6.OR. &
    GRPLEN == 9.AND.ELEM(12) == 0) THEN

IFLABEL4: &
  IF (GRPLEN == 4.AND.GROUP(1:2) <= '24'.AND. &
      GROUP(3:4) <= '24') THEN
     REXP(19)=IVALUE(GROUP(1:2))    !START HOUR
     REXP(20)=IVALUE(GROUP(3:4))    !END HOUR
     ELEM(12)=1
  ELSE IF (GRPLEN == 6.AND.GROUP(1:2) <= '31'.AND. &
           GROUP(3:4) <= '24'.AND.GROUP(5:6) <= '24' &
           .AND.ELEM(13) == 0) THEN
     REXP(12)=IVALUE(GROUP(1:2))    !START DAY
     REXP(19)=IVALUE(GROUP(3:4))    !START HOUR
     REXP(20)=IVALUE(GROUP(5:6))    !END HOUR
     ELEM(12)=1
     REXP(6)=-9999999
     REXP(7)=-9999999
     REXP(8)=-999999
  ELSE IF (GRPLEN == 9.AND.GROUP(1:2) <= '31'.AND. &
           GROUP(3:4) <= '24'.AND.GROUP(5:5) == '/' ) THEN
     REXP(12)=IVALUE(GROUP(1:2))    !START DAY
     REXP(61)=IVALUE(GROUP(6:7))    !END DAY
     REXP(19)=IVALUE(GROUP(3:4))    !START HOUR
     REXP(20)=IVALUE(GROUP(8:9))    !END HOUR
     ELEM(12)=1
     SWITCH=1

! If the first group encountered after the Station identifier
! is a six figure group without a 'Z' char and no period
! group has been encountered it will be safe to assume
! that the
  ELSE IF (GRPLEN == 6.AND.GROUP(1:2) <= '31'.AND. &
           GROUP(3:4) <= '24'.AND.GROUP(5:6) <= '24' &
           .AND.ELEM(13) == 1 )THEN
     REXP(12)=IVALUE(GROUP(1:2))    !START DAY
     REXP(19)=IVALUE(GROUP(3:4))    !START HOUR
     REXP(20)=IVALUE(GROUP(5:6))    !END HOUR
     ELEM(12)=1
  ELSE
    FAIL=8
    RETURN
  END IF IFLABEL4
 GROUP=REPORT(POINT1:POINT1+GRPLEN-1)
END IF IFLABEL3

! ******** Loop round remaining groups regardless of position, ********
! *********** checking pattern & flags set if already found ***********

BADRUN=0
DOLABEL1: &
DO WHILE (.NOT.LREPFL)

! Point to the next group, keeping it in GROUP for checks.
! (Unless BADRUN=2, 2 bad groups already run together in GROUP)

IFLABEL5: &
  IF (BADRUN /= 2) THEN
    POINT1=POINT2
    CHANGE=.FALSE.

! Delimit the group.  (Visibility can be reported in statute miles &
! quarters, with a space before the fraction.  If so, bypass MTRLOC.)

    IF (POINT1+6 <= REPLEN) THEN
      IF(REPORT(POINT1+5:POINT1+6) == 'SM'.AND. &
         REPORT(POINT1+3:POINT1+3) == '/') THEN
        GRPLEN=7
        POINT2=POINT1+GRPLEN+1   ! point to start of next group
      ELSE
        CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
      END IF
    ELSE
      CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
    END IF
  END IF IFLABEL5

! See where the figures are in the group.

  GROUP=REPORT(POINT1:POINT1+GRPLEN-1)
  CALL MTRGRP(GROUP,GRPLEN,NCHAR,CHARR)

! See if this group starts a new change section and, if so, set the
! corresponding period & on return reinitialise the arrays needed to
! see which slots have already been filled (not period, so LOOP=1,11)
! Also set an offset for use in REXP subscripts, to be added to sub-
! scripts in the first 30-value change section (61-90).  Give up if
! what looks like a change section start isn't followed by a time.

  CALL TAFSECT(REPORT(1:REPLEN),POINT1,REXP,SECTION,RC,FACT)

! Set some group test flags here to avoid implied references to things
! like CHARR(-1:0) which cause run time failures in Fortran90.

!                                      for date/time group
  TIMETEST = .FALSE.
  IF (GROUP(GRPLEN:GRPLEN) == 'Z') THEN
    IF (GRPLEN >= 9) THEN
      TIMETEST = CHARR(GRPLEN-7:GRPLEN-3) == 'NN/NN'
    ELSE IF (GRPLEN >= 6) THEN
      TIMETEST = CHARR(GRPLEN-5:GRPLEN-1) == 'NN/NN'
    ENDIF
  END IF
!                                      for wind/gust group
  WINDTEST = .FALSE.
  IF (GRPLEN >= 7 .AND.                                           &
      (CHARR(1:3) == 'NNN' .OR. GROUP(1:3) == 'VRB')) THEN
    WINDTEST = GROUP(GRPLEN-1:GRPLEN) == 'KT'  .OR.               &
               GROUP(GRPLEN-2:GRPLEN) == 'MPS' .OR.               &
               GROUP(GRPLEN-2:GRPLEN) == 'KMH'
  END IF
!                                      for Cb cloud group
  CLOUDTEST = .FALSE.
  IF (GRPLEN >= 8 .AND. (CHARR(1:6) == 'YYYNNN')) THEN
    CLOUDTEST = GROUP(GRPLEN-1:GRPLEN) == 'CB'  .OR.              &
                GROUP(GRPLEN-2:GRPLEN) == 'TCU'
  END IF

!                                      for horizontal visibility group
  HVISTEST = .FALSE.
  IF (GRPLEN > 1) THEN
    HVISTEST = (GRPLEN == 4 .AND. NCHAR == 0) .OR.                &
                GROUP(GRPLEN-1:GRPLEN) == 'SM'
  END IF

IFLABEL6: &
  IF (RC < 0) THEN
    FAIL=8
    RETURN
  ELSE IF (RC > 0) THEN
    REXP(60)=SECTION
    DO LOOP=1,11
      ELEM(LOOP)=0
    END DO
    CLDAMT=0
    OLDAMT=0
    OFFSET=(SECTION-1)*FACT
    POINT2=POINT1
    BADRUN=0

! **************************** WIND ***********************************

! See if the wind is calm: if so, there may be no units to follow!

  ELSE IF (GRPLEN == 5 .AND. GROUP(1:5) == '00000') THEN
    IF (SECTION == 0) THEN
      REXP(21)=0
      REXP(22)=0
    ELSE
      REXP(70+OFFSET)=0
      REXP(71+OFFSET)=0
    END IF
    BADRUN=0

! In general a wind group ends with KT (or other units) & starts with
! either 3 figures or VRB.  MTRDDF accepts 2 or 3 figures for speed
! and (optional) gust after G or /.

  ELSE IF (WINDTEST) THEN
    DDD=-9999999          ! MTRDDF doesn't init DDD & FFF to zero!
    FFF=-9999999
    FMFMFM=-9999999
    CALL MTRDDF(REPORT,POINT1,GRPLEN,CHARR,DDD,FFF,FMFMFM)    !2.0

IFLABEL7: &
    IF (ELEM(5) == 0) THEN
      IF (SECTION == 0) THEN
        IF (DDD > 0) REXP(21)=DDD
        IF (FFF > 0) REXP(22)=FFF
        IF (FMFMFM > 0) REXP(23)=FMFMFM
      ELSE
        IF (DDD > 0) REXP(70+OFFSET)=DDD
        IF (FFF > 0) REXP(71+OFFSET)=FFF
        IF (FMFMFM > 0) REXP(72+OFFSET)=FMFMFM
      END IF
    ELSE
      FAIL=12
      RETURN
    END IF IFLABEL7
    ELEM(5)=1
    BADRUN=0

! ********************** HORIZONTAL VISIBILITY ************************

! Either a 4-figure group (metres) or a group ending SM (statute miles)
! Only certain multiples of 50, 100 or 1000 are allowed in the 4 figures

  ELSE IF (HVISTEST) THEN
    IF (ELEM(4) > 0) THEN
      FAIL=12
      RETURN
    END IF

    VVVV=-9999999.
IFLABEL8: &
    IF (NCHAR == 0) THEN
      IF ((GROUP(1:2) < '05' .AND. GROUP(3:4) == '50') .OR. &
          (GROUP(1:2) < '50' .AND. GROUP(3:4) == '00') .OR. &
           GROUP(2:4) == '000') THEN
        READ (GROUP(1:4),'(F4.0)') VVVV
      ELSE IF (GROUP(1:4) == '9999') THEN
        VVVV=10000
      END IF
    ELSE IF (GROUP(GRPLEN-2:GRPLEN-2) >= '0' .AND. &
             GROUP(GRPLEN-2:GRPLEN-2) <= '9') THEN
      CALL TAFVISM(GROUP(1:GRPLEN),VVVV)
    END IF IFLABEL8

    IF (VVVV > 0) THEN
      IF (SECTION == 0) THEN
        REXP(46)=VVVV
      ELSE
        REXP(95+OFFSET)=VVVV
      END IF
      ELEM(4)=1
      BADRUN=0
    END IF

! ********************** WEATHER (CAVOK etc) **************************

! Expand CAVOK, NSC, SKC or NSW, setting corresponding flag.

  ELSE IF ((GRPLEN == 5 .AND. GROUP(1:5) == 'CAVOK')      &
      .OR. (GRPLEN == 3 .AND. (GROUP(1:3) == 'NSC' .OR.   &
      GROUP(1:3) == 'SKC' .OR. GROUP(1:3) == 'NSW'))) THEN
    IF (GROUP(1:3) == 'NSC') WXFLAG=1
    IF (GROUP(1:3) == 'CAV') WXFLAG=2
    IF (GROUP(1:3) == 'SKC') WXFLAG=4
    IF (GROUP(1:3) == 'NSW') WXFLAG=8

IFLABEL9: &
    IF (SECTION == 0) THEN
      IF (REXP(24) < 0) THEN
        REXP(24)=WXFLAG
      ELSE
        REXP(24)=REXP(24)+WXFLAG
      END IF
    ELSE
      IF (REXP(73+OFFSET) < 0) THEN
        REXP(73+OFFSET)=WXFLAG
      ELSE
        REXP(73+OFFSET)=REXP(73+OFFSET)+WXFLAG
      END IF
    END IF IFLABEL9
    BADRUN=0

! ********************* WEATHER (snow, fog etc) ***********************

! A group with no figures (even number of letters, maybe + or - at
! start) may be weather.
! The IF below reads: if the group has no figures, and length up to 9,
! and either there is an even number of letters with the first
! (& second, if any) pairs both listed, or the group starts + or -
! and the first pair is listed - then it MIGHT be a weather group!
!   (The index operation could find a "pair" consisting of the last
! letter of one pair & the first of the next - but MTRWWR won't...)

  ELSE IF (GRPLEN == NCHAR .AND. GRPLEN <= 9                    &
          .AND. ((GRPLEN >= 2 .AND. MOD(GRPLEN,2) == 0          &
          .AND. INDEX(PAIRS,GROUP(1:2)) > 0                     &
          .AND. (GRPLEN == 2 .OR. INDEX(PAIRS,GROUP(3:4)) > 0)) &
         .OR. ((GROUP(1:1) == '+' .OR. GROUP(1:1) == '-')       &
          .AND. INDEX(PAIRS,GROUP(2:3)) > 0))) THEN
    IF (ELEM(1) > 2) THEN
      FAIL=12
      RETURN
    END IF

    DO LOOP=1,9
      WDWD(LOOP)=-9999999
    END DO
    ESTAR=1               ! always points to ELEM(1) for TAFs
    CALL MTRWWR(POINT1,REPORT,GRPLEN,ELEM,ESTAR,WDWD,REWW,LBAD)

! If there's no error, set the values returned in either the main
! section or a change section.

    IF (ELEM(1) > 0 .AND. ELEM(1) <= 3 .AND. .NOT.LBAD) THEN
      IF (SECTION == 0) THEN
        REXP(25+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3-2)
        REXP(26+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3-1)
        REXP(27+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3)
      ELSE
        REXP(74+OFFSET+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3-2)
        REXP(75+OFFSET+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3-1)
        REXP(76+OFFSET+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3)
      END IF
    END IF
    IF (.NOT.LBAD) BADRUN=0

! ********************** VERTICAL VISIBILITY **************************

! Expand vertical visibility. If no vertical visibility & no cloud is
! reported, set sky obscured in the cloud slot.  For change sections
! the first cloud base slot is used, negative values being vertical
! vis - the two elements are mutually exclusive.

  ELSE IF (GRPLEN == 5 .AND. GROUP(1:2) == 'VV' &
     .AND. (CHARR(3:5) == '///' .OR. CHARR(3:5) == 'NNN')) THEN
    IF (ELEM(11) > 0) THEN
      FAIL=12
      RETURN
    END IF

IFLABEL10: &
    IF (SECTION == 0) THEN
      IF (CHARR(3:5) == '///') THEN
        REXP(34)=9
      ELSE
        REXP(47)=IVALUE(GROUP(3:5))*METRES
      END IF
    ELSE
      IF (CHARR(3:5) == '///') THEN
        REXP(83+OFFSET)=9
      ELSE
        REXP(83+OFFSET)=-9999999.
        REXP(85+OFFSET)=-IVALUE(GROUP(3:5))*METRES
      END IF
    END IF IFLABEL10
    ELEM(11)=1
    BADRUN=0

! ******************************* CLOUD *******************************

! Expand up to 3 cloud groups.  Each group is 3 letters & 3 figures.
! The letters are FEW, SCT (scattered), BKN (broken) or OVC (overcast).
! Groups should be in that order (non-decreasing amounts); so oktas
! checked in case a change section start not recognised.

  ELSE IF (GRPLEN == 6 .AND. CHARR(1:6) == 'YYYNNN' .AND.       &
           (GROUP(1:3) == 'FEW' .OR. GROUP(1:3) == 'SCT' .OR.   &
            GROUP(1:3) == 'BKN' .OR. GROUP(1:3) == 'OVC')) THEN
    IF (ELEM(2) > 2) THEN
      FAIL=16
      RETURN
    END IF

    IF (CLDAMT > 0) OLDAMT=CLDAMT
    ESTAR=2
    CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR, &    !2.0
                CLDAMT,CLDTYPE,CLDHT,VVIS)
IFLABEL11: &
    IF (CLDAMT >= 8) THEN
      IF (OLDAMT >= 8 .AND. OKTAS(CLDAMT) < OKTAS(OLDAMT)) THEN
        FAIL=20
        RETURN
      END IF

      IF (SECTION == 0) THEN
        REXP(34+(ELEM(ESTAR)-1)*3)=CLDAMT
        REXP(35+(ELEM(ESTAR)-1)*3)=CLDTYPE
        REXP(36+(ELEM(ESTAR)-1)*3)=CLDHT
        REXP(47)=VVIS
      ELSE
        REXP(83+OFFSET+(ELEM(ESTAR)-1)*3)=CLDAMT
        REXP(84+OFFSET+(ELEM(ESTAR)-1)*3)=CLDTYPE
        REXP(85+OFFSET+(ELEM(ESTAR)-1)*3)=CLDHT
      END IF
    END IF IFLABEL11
    BADRUN=0

! Towering cumulus or cumulonimbus (CB or TCU on end of normal group)
! (Only one such group allowed in a section)

  ELSE IF (CLOUDTEST) THEN
    IF (ELEM(3) > 0) THEN
      FAIL=12
      RETURN
    END IF

    ESTAR=3
    CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR, &      !2.0
                CLDAMT,CLDTYPE,CLDHT,VVIS)
    IF (SECTION == 0) THEN
      REXP(43)=CLDAMT
      REXP(44)=CLDTYPE
      REXP(45)=CLDHT
    ELSE
      REXP(92+OFFSET)=CLDAMT
      REXP(93+OFFSET)=CLDTYPE
      REXP(94+OFFSET)=CLDHT
    END IF
    BADRUN=0

! *********** FORECAST TEMPERATURE (T?6MYTfTf/GfGfZ) ******************

! T may be followed by X (max) or N (min); M before figures if T<0.

  ELSE IF (TIMETEST .AND. GROUP(1:2) == 'TX') THEN

    IF (ELEM(10) >= 2) THEN
      FAIL=12
      RETURN
    END IF

    IF (GRPLEN >= 10) THEN
      IF (CHARR(GRPLEN-7:GRPLEN-1) == 'NN/NNNN') THEN
        HOUR=IVALUE(GROUP(GRPLEN-2:GRPLEN-1))
        DAY=IVALUE(GROUP(GRPLEN-4:GRPLEN-3))
        TEMP=IVALUE(GROUP(GRPLEN-7:GRPLEN-6))
      END IF
    END IF

    IF (GRPLEN >= 8) THEN
      IF (CHARR(GRPLEN-5:GRPLEN-1) == 'NN/NN') THEN
        HOUR=IVALUE(GROUP(GRPLEN-2:GRPLEN-1))
        TEMP=IVALUE(GROUP(GRPLEN-5:GRPLEN-4))
      END IF
    END IF

    IF (GRPLEN >= 7) THEN
      IF (GROUP(GRPLEN-6:GRPLEN-6) == 'M') THEN
        TEMP = -TEMP
      ELSE IF (GRPLEN >= 9) THEN
        IF (GROUP(GRPLEN-8:GRPLEN-8) == 'M') TEMP = -TEMP
      END IF
    END IF

    IF (GROUP(2:2) == 'X') THEN
      REXP(62)=DAY
      REXP(48)=HOUR
      REXP(49)=TEMP+KELVIN
    END IF

   ELSE IF (TIMETEST .AND. GROUP(1:2) == 'TN') THEN

     IF (ELEM(10) >= 2) THEN
       FAIL=12
       RETURN
     END IF

     IF (GRPLEN >= 10) THEN
       IF (CHARR(GRPLEN-7:GRPLEN-1) == 'NN/NNN') THEN
         HOUR=IVALUE(GROUP(GRPLEN-2:GRPLEN-1))
         DAY=IVALUE(GROUP(GRPLEN-4:GRPLEN-3))
         TEMP=IVALUE(GROUP(GRPLEN-7:GRPLEN-6))
       END IF
     END IF

     IF (GRPLEN >= 8) THEN
       IF (CHARR(GRPLEN-5:GRPLEN-1) == 'NN/NN') THEN
         HOUR=IVALUE(GROUP(GRPLEN-2:GRPLEN-1))
         TEMP=IVALUE(GROUP(GRPLEN-5:GRPLEN-4))
       END IF
     END IF

     IF (GRPLEN >= 7) THEN
       IF (GROUP(GRPLEN-6:GRPLEN-6) == 'M') THEN
         TEMP = -TEMP
       ELSE IF (GRPLEN >= 9) THEN
         IF (GROUP(GRPLEN-8:GRPLEN-8) == 'M') TEMP = -TEMP
       END IF
     END IF

     IF (GROUP(2:2) == 'N') THEN
       REXP(63)=DAY
       REXP(58)=HOUR
       REXP(59)=TEMP+KELVIN
     END IF

     ELEM(10)=ELEM(10)+1
     BADRUN=0

! ********************* ICING (6IchihihitL) ***************************
! (The icing group is from the old TAF code, but still used (in 2002)
!  by some American airfields)

! Icing type/level/thickness (6 figures starting with '6')

   ELSE IF (GRPLEN == 6 .AND. NCHAR == 0 &
              .AND. GROUP(1:1) == '6') THEN
     IF (ELEM(8) > 0) THEN
       FAIL=12
       RETURN
     END IF

! Expand icing severity Ic

IFLABEL12: &
     IF (SECTION == 0) THEN
       REXP(52)=IVALUE(GROUP(2:2))

! Expand icing level hihihi (multiple of 30m)

       READ (GROUP(3:5),'(I3)') ICELV
       IF (ICELV == 999) THEN
         ICELV=30000
       ELSE
         ICELV=ICELV*30
       END IF
       REXP(50)=ICELV

! Expand icing depth tL (single-figure multiple of 300m)

       REXP(51)=IVALUE(GROUP(6:6))*300
       ELEM(8)=1
     END IF IFLABEL12

     BADRUN=0

! ********************* TURBULENCE (5BhBhBhBtL) ***********************
! (The turbulence group is from the old TAF code, but still used (in
!  2002) by 4 UK RAF airfields & some American airfields)

! See if this is a turbulence group (6 figures starting with '5').
! More than one 5-group can be reported in a section, usually one
! at the surface and one aloft; keep the first group.
! There is no slot for turbulence in change sections.

   ELSE IF (GRPLEN == 6 .AND. NCHAR == 0 &
           .AND. GROUP(1:1) == '5') THEN

! Expand turbulence severity (B) & set frequency to 1 for occasional
! if B is even, 2 for frequent if B is odd.

IFLABEL13: &
     IF (SECTION == 0 .AND. ELEM(9) == 0) THEN
       READ (GROUP(2:2),'(I1)') TURBIN
       REXP(53)=TURBIN
       IF (TURBIN >= 2) REXP(56)=MOD(TURBIN,2)+1

! Expand turbulence height hBhBhB (multiple of 30m)

       READ (GROUP(3:5),'(I3)') TURBLV
       IF (TURBLV == 999) THEN
         TURBLV=30000
       ELSE
         TURBLV=TURBLV*30
       END IF
       REXP(54)=TURBLV

! Expand turbulence thickness tL (single-figure multiple of 300m)

       REXP(55)=IVALUE(GROUP(6:6))*300
       ELEM(9)=1
     END IF IFLABEL13
     BADRUN=0

! **************** Plain language? Unrecognised group? ****************

! Assume RMK is the start of plain language & stop expansion.
! If this group is not recognised, join it up with next & try again.
! (Use GROUP as work area: it will be reset straight away)

   ELSE IF (GROUP(1:4) == 'RMK ') THEN
       LREPFL=.TRUE.
   ELSE                          ! Group unrecognised
    BADRUN=BADRUN+1
     IF (BADRUN == 1) THEN       ! First such group?
      BADLEN=GRPLEN
     ELSE IF (BADRUN == 2) THEN  ! If next group unrecognised too,
      GROUP=REPORT(POINT1-BADLEN-1:POINT1-2)
      REPORT(POINT1-BADLEN:POINT1-1)=GROUP
      POINT1=POINT1-BADLEN
      GRPLEN=GRPLEN+BADLEN      ! try again with groups joined up.
     ELSE                        ! If retry fails, give up.
      FAIL=4
      LREPFL=.TRUE.
     END IF
   END IF IFLABEL6
END DO DOLABEL1

! Copy the original report back to the start of the string if a    !2.1
! group has remained unrecognised after groups run together.       !2.1

IF (REPLEN < LEN(REPORT)/2 .AND. FAIL >= 4) THEN             !2.1
  REPORT(:REPLEN)=REPORT(LEN(REPORT)-REPLEN+1:)              !2.1
END IF                                                       !2.1
RETURN
END SUBROUTINE TAFEXP
