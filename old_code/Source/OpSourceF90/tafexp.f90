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
!  PARAMETERS   : 1 REPORT - TAF REPORT TO BE DECODED
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
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Author: Richard Weedon$
! $Folder: OpSourceF90$
! $Workfile: tafexp.f90$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2002/10/07 15:03:20  usmdb
! 21 Oct 2002     C Long
! 2.1  Rewrite with clearer structure, calling new subroutines
!      TAFSECT to recognise change section starts & TAFVISM to
!      cope with vis in SM, & trying to join up split groups.
!
! Revision 2.0  2001/01/08  15:29:11  15:29:11  usmdb (Generic MetDB acc
! Removed unused argument NCHAR from calls to MTRCCC and
! MTRDDF. Removed unused dummy argument CERROR. Added copyright
! and modified header - S.Cox
!
! Revision 1.3  98/07/23  08:48:41  08:48:41  usmdb (Generic MDB account
! correct assignment of the NSC group to the correct displacement
!
! Revision 1.2  97/08/04  13:35:36  13:35:36  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.1  1997/02/17 11:57:34  uspm
! Initial revision
!
! 20/07/98  Correct the assignment of NSC groups to the correct
!           displacement. Jon Lewthwaite !A
!
! 28/03/96  CORRECT HEIGHT MULTIPLICATION FACTOR. OBSERVED
!           HEIGHTS ARE REPORTED IN UNITS OF 30 METRES.
!           CHANGED VALUE OF 'METRES' TO REFLECT THIS.
!           CORRECTION TO EXPANSION OF FORECAST CHANGE PERIOD
!           BEGIN AND END HOURS. ARRAY DISPLACEMENT (62) WAS
!           BEING USED FOR BOTH START AND END TIMES. CHANGED
!           END TIME ARRAY EXPANSION TO (63).
!
! DEC 95    REMOVE CALLS TO MTRTCU,MTRVVV AND TAFINI. ROUTINES
!           INCORPORATED INTO MAIN CODE OR NO LONGER REQUIRED.
!           REMOVE 2 DIMENSIONAL ARRAY FOR SUBPERIOD EXPANSION
!           AND REPLACE WITH SINGLE ARRAY NOW TREATING SUB
!           PERIODS LIKE ANY OTHER CHANGE SECTION. CORRECT
!           GROUP RECOGNITION TO ALLOW MORE REPORT DETAIL TO
!           BE EXPANDED AND REDUCE SIZE OF ELEMENT FLAG ARRAY.
!           PREVIOUS SIZE OF 30 ELEMENTS WAS UNNECESSARY.
!           IMPROVE DOCUMENTATION AND CODE TO CURRENT STANDARD.
!           THE GROUP RECOGNITION STRUCTURE HAS BEEN ALTERED TO
!           ATTEMPT TO MINIMALISE THE NUMBER OF LOGICAL CHECKS
!           REQUIRED BEFORE A GROUP IS IDENTIFIED.
!           VISIBILITIES REPORTED IN STATUTE MILES CAN NOW BE
!           EXPANDED.
!
! 07/12/95  CHANGE PARAMETERS IN CALLS TO MTRWWR, MTRDDF AND
!           MTRCCC. CHANGE VARIABLE TYPE OF N,NT,CC AND CCT
!           TO PATCH CHANGES IN CALLS. ADD TEMPORARY FIXES TO
!           SUBPERIOD RECOGNITION AND FORECAST VALIDITY TIME
!           RECOGNITION SECTIONS BEFORE MAJOR OVERHAUL OF
!           WHOLE ROUTINE.
!
! 23/06/93  INCREASE NUMBER OF CHANGE GROUPS THAT CAN BE
!           EXPANDED FROM 3 TO 5. ALSO CORRECT HANDLING OF
!           DATA GROUP AFTER TIME GROUP IN NEW FORM OF PROB
!           TEMPO GROUP.
!
! 19/01/93  IMPLEMENTED 10:10PM
!
! 05/06/08  AS A RESULT OF THE INTRODUCTION OF THIRTY HOUR TAFS
!           THE VALIDITY DATE TIME GROUPS HAVE CHANGED. THIS
!           HAS A DIRECT IMPACT ON THE VALIDITY TIME USED FOR THE
!           FORECAST AND CHANGE VALIDITY PERIODS. IN ADDITION THE
!           FORECAST TEMPERATURE PERIODS ALSO REQUIRED CHANGE.
!
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

INTEGER       BADRUN      ! Number of consecutive bad groups
INTEGER       BADLEN      ! Length of first bad group
INTEGER       C_RC        ! TAF AMD Indicator
INTEGER       CLDAMT      ! Amount of cloud
INTEGER       CLDTYPE     ! Cloud identity.
INTEGER       DAY         ! Forecast Temp Day
INTEGER       FACT        ! Expansion array factor
INTEGER       ELEM(13)    ! Element array and counter.
INTEGER       ESTAR       ! Starting position within array ELEM.
INTEGER       FAIL        ! Error value returned to MDB.
INTEGER       GRPLEN      ! Length of group being expanded.
INTEGER       HOUR        ! Hour of origin or forecast start/end
INTEGER       ICELV       ! Height of icing.
INTEGER       IVALUE      ! Figure to number conversion
INTEGER       KEEPNTR     ! To keep a pointer to reset if nec.
INTEGER       LOOP        ! General loop variable.
INTEGER       METRES      ! Hundreds of feet to metres conversion
INTEGER       NAMD        ! Number of amendment              !9
INTEGER       NCHAR       ! Number of non-figures in group
INTEGER       OFFSET      ! To calculate array subscripts
INTEGER       OKTAS(0:13) ! Equivalents of CLDAMT from 8 to 13
INTEGER       OLDAMT      ! Previous CLDAMT in section
INTEGER       POINT1      ! Pointer to current group in report.
INTEGER       POINT2      ! Pointer to next group in report.
INTEGER       RC          ! Return code from TAFSECT
INTEGER       REPLEN      ! Total length of the report.
INTEGER       SECTION     ! Section of report being expanded
INTEGER       SWITCH      ! TAF Format indicator
INTEGER       TEMP        ! Temperature
INTEGER       TURBIN      ! Turbulence severity.
INTEGER       TURBLV      ! Height of turbulence.
INTEGER       WXFLAG      ! Power of 2 corresponding to CAVOK etc

CHARACTER*20  CHARR       ! Set by MTRGRP: N if figure, Y if not.
CHARACTER*4   ID          ! Station ID character string.
CHARACTER*(*) REPORT      ! The report to be expanded/decoded.
CHARACTER*20  GROUP       ! Group from report for checks
CHARACTER*132 HEAD


CHARACTER*62  PAIRS       ! Pairs of letters used in weather group

LOGICAL       CHANGE      ! Flag set if valid trend group found.
LOGICAL       LBAD        ! Bad significant weather group.
LOGICAL       LREPFL      ! Set (by MTRLOC) if end of report
LOGICAL       HEADSET

REAL          CLDHT       ! Cloud base height (in metres)
REAL          DDD         ! Wind direction.
REAL          FFF         ! Wind force
REAL          FMFMFM      ! Gust force
REAL          KELVIN      ! Value used for temperature conversion.
REAL          REXP(*)     ! Expansion array.
REAL          REWW(6)     ! Not used (for consistency with METARs)
REAL          VVVV        ! Horizontal visibility.
REAL          VVIS        ! Vertical visibility.
REAL          WDWD(9)     ! Present weather array for MTRWWR

! Initialise variables.

PARAMETER (KELVIN=273)    ! Temperature of 0C in Kelvin.      1.4
PARAMETER (METRES=30)     ! Height multiplication factor.

! The list of leter pairs is as in MTRWWR with RE, VC & XX at start.
!     (REvcXXmiBCdrBLshTSfzPRdzRAsnSGicPLgrGSbrFGfuVAduSAhzPOsqFCssDS)

DATA PAIRS/&
&'REVCXXMIBCDRBLSHTSFZPRDZRASNSGICPLGRGSBRFGFUVADUSAHZPOSQFCSSDS'/
DATA OKTAS/8*0,8,0,0,2,4,1/  ! oktas for CLDAMT=...8,,,11,12,13

DATA HEADSET/.FALSE./

IF (.NOT.HEADSET) THEN
  HEAD='$Workfile: tafexp.f90$ ' //&
& '$Revision: 1$ $Date: 26/01/2010 10:18:13$'
  HEADSET=.TRUE.
ENDIF

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

IF (REPLEN.LT.LEN(REPORT)/2) THEN                            !2.1
  REPORT(LEN(REPORT)-REPLEN+1:)=REPORT(:REPLEN)              !2.1
ENDIF                                                        !2.1

! Initialise expansion monitoring array to missing.

DO LOOP=1,13
  ELEM(LOOP)=0
ENDDO
CLDAMT=0
OLDAMT=0


! ********************* IDENTIFIER (first group) **********************

! See if first group is identifier (4 letters).  If not, give up.
! MTRLOC delimits a group, MTRGRP sees how many non-figures.
! (The ident is only skipped here; it's already in the expansion.)

POINT1=POINT2
CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
CALL MTRGRP(REPORT(POINT1:),GRPLEN,NCHAR,CHARR)

IF (GRPLEN.EQ.4 .AND. NCHAR.EQ.4) THEN
  ID=REPORT(POINT1:POINT1+3)
ELSE
  FAIL=8
  RETURN
ENDIF

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
IF (NAMD.GT.0) THEN                                          !9
  REXP(65)=1                                                 !9
  CALL TAFCNL(REPORT,REPLEN,C_RC)                            !9
  IF (C_RC.EQ.1) THEN                                        !9
    REXP(64)=1                                               !9
  ELSE                                                       !9
    REXP(64)=0                                               !9
  ENDIF                                                      !9
ELSE                                                         !9
  REXP(64)=0                                                 !9
  REXP(65)=0                                                 !9
ENDIF                                                        !9


! Checks for the Time of Issue. If the group is 6 figures without
! any chars assume that the format to be DDHHMM.Alternativly
! assume the format to be DDHHZIF

IF ((GRPLEN.EQ.5 .AND. NCHAR.EQ.1 .AND. GROUP(5:5).EQ.'Z') .OR.&
   &(GRPLEN.EQ.7 .AND. NCHAR.EQ.1 .AND. GROUP(7:7).EQ.'Z')) THEN
  IF (GRPLEN.EQ.7 .AND. GROUP(1:2).LE.'31' .AND.&
     &GROUP(3:4).LT.'24' .AND. GROUP(5:6).LT.'60') THEN
     REXP(6)=IVALUE(GROUP(1:2))        ! day
     REXP(7)=IVALUE(GROUP(3:4))        ! hour
     REXP(8)=IVALUE(GROUP(5:6))        ! minute
     POINT1=POINT2
     CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
     GROUP=REPORT(POINT1:POINT1+GRPLEN-1)
     CALL MTRGRP(GROUP,GRPLEN,NCHAR,CHARR)
     ELEM(13)=1
  ENDIF

  IF (GRPLEN.EQ.5 .AND. GROUP(1:2).LE.'31'.AND.&
     &GROUP(3:4).LT.'24') THEN
     REXP(6)=IVALUE(GROUP(1:2))      ! Day
     REXP(7)=IVALUE(GROUP(3:4))        ! Hour
     POINT1=POINT2
     CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
     GROUP=REPORT(POINT1:POINT1+GRPLEN-1)
     CALL MTRGRP(GROUP,GRPLEN,NCHAR,CHARR)
     ELEM(13)=1
  ENDIF
ELSE
  FAIL=8
  RETURN
ENDIF

! As a result of the changes introduced as ICAO Annex 3
! Amendment 74, the date / time format of the TAF validity
! period have changed. This procedure will be expected to
! process bulletins in both the new and old format.

! If a 4 or 6 or 9 figure group follows the issue group assume
! it is the validity period. The forecast Validity period
! will be of the format HH HH or DD HH HH or DD HH / DD HH.

IF (GRPLEN.EQ.4.OR.GRPLEN.EQ.6.OR.&
   &GRPLEN.EQ.9.AND.ELEM(12).EQ.0) THEN

  IF (GRPLEN.EQ.4.AND.GROUP(1:2).LE.'24'.AND.&
     &GROUP(3:4).LE.'24') THEN
     REXP(19)=IVALUE(GROUP(1:2))    !START HOUR
     REXP(20)=IVALUE(GROUP(3:4))    !END HOUR
     ELEM(12)=1
  ELSE IF (GRPLEN.EQ.6.AND.GROUP(1:2).LE.'31'.AND.&
          &GROUP(3:4).LE.'24'.AND.GROUP(5:6).LE.'24'&
          &.AND.ELEM(13).EQ.0) THEN
     REXP(12)=IVALUE(GROUP(1:2))    !START DAY
     REXP(19)=IVALUE(GROUP(3:4))    !START HOUR
     REXP(20)=IVALUE(GROUP(5:6))    !END HOUR
     ELEM(12)=1
     REXP(6)=-9999999
     REXP(7)=-9999999
     REXP(8)=-999999
  ELSE IF (GRPLEN.EQ.9.AND.GROUP(1:2).LE.'31'.AND.&
          &GROUP(3:4).LE.'24'.AND.GROUP(5:5).EQ.'/' ) THEN
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
  ELSE IF (GRPLEN.EQ.6.AND.GROUP(1:2).LE.'31'.AND.&
          &GROUP(3:4).LE.'24'.AND.GROUP(5:6).LE.'24'&
          &.AND.ELEM(13).EQ.1 )THEN
     REXP(12)=IVALUE(GROUP(1:2))    !START DAY
     REXP(19)=IVALUE(GROUP(3:4))    !START HOUR
     REXP(20)=IVALUE(GROUP(5:6))    !END HOUR
     ELEM(12)=1
  ELSE
    FAIL=8
    RETURN
  ENDIF
 GROUP=REPORT(POINT1:POINT1+GRPLEN-1)
ENDIF


! ******** Loop round remaining groups regardless of position, ********
! *********** checking pattern & flags set if already found ***********

BADRUN=0
DO WHILE (.NOT.LREPFL)

! Point to the next group, keeping it in GROUP for checks.
! (Unless BADRUN=2, 2 bad groups already run together in GROUP)

  IF (BADRUN.NE.2) THEN
    POINT1=POINT2
    CHANGE=.FALSE.

! Delimit the group.  (Visibility can be reported in statute miles &
! quarters, with a space before the fraction.  If so, bypass MTRLOC.)

    IF (POINT1+6.LE.REPLEN) THEN
      IF(REPORT(POINT1+5:POINT1+6).EQ.'SM'.AND.&
        &REPORT(POINT1+3:POINT1+3).EQ.'/') THEN
        GRPLEN=7
        POINT2=POINT1+GRPLEN+1   ! point to start of next group
      ELSE
        CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
      ENDIF
    ELSE
      CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
    ENDIF
  ENDIF

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


  IF (RC.LT.0) THEN
    FAIL=8
    RETURN
  ELSE IF (RC.GT.0) THEN
    REXP(60)=SECTION
    DO LOOP=1,11
      ELEM(LOOP)=0
    ENDDO
    CLDAMT=0
    OLDAMT=0
    OFFSET=(SECTION-1)*FACT
    POINT2=POINT1
    BADRUN=0

! **************************** WIND ***********************************

! See if the wind is calm: if so, there may be no units to follow!

  ELSE IF (GRPLEN.EQ.5 .AND. GROUP(1:5).EQ.'00000') THEN
    IF (SECTION.EQ.0) THEN
      REXP(21)=0
      REXP(22)=0
    ELSE
      REXP(70+OFFSET)=0
      REXP(71+OFFSET)=0
    ENDIF
    BADRUN=0

! In general a wind group ends with KT (or other units) & starts with
! either 3 figures or VRB.  MTRDDF accepts 2 or 3 figures for speed
! and (optional) gust after G or /.

  ELSE IF (GRPLEN.GE.7 .AND. (CHARR(1:3).EQ.'NNN' .OR.&
       &GROUP(1:3).EQ.'VRB') .AND.&
      &(GROUP(GRPLEN-1:GRPLEN).EQ.'KT' .OR.&
      & GROUP(GRPLEN-2:GRPLEN).EQ.'MPS' .OR.&
       &GROUP(GRPLEN-2:GRPLEN).EQ.'KMH')) THEN
    DDD=-9999999          ! MTRDDF doesn't init DDD & FFF to zero!
    FFF=-9999999
    FMFMFM=-9999999
    CALL MTRDDF(REPORT,POINT1,GRPLEN,CHARR,DDD,FFF,FMFMFM)    !2.0

    IF (ELEM(5).EQ.0) THEN
      IF (SECTION.EQ.0) THEN
        IF (DDD.GT.0) REXP(21)=DDD
        IF (FFF.GT.0) REXP(22)=FFF
        IF (FMFMFM.GT.0) REXP(23)=FMFMFM
      ELSE
        IF (DDD.GT.0) REXP(70+OFFSET)=DDD
        IF (FFF.GT.0) REXP(71+OFFSET)=FFF
        IF (FMFMFM.GT.0) REXP(72+OFFSET)=FMFMFM
      ENDIF
    ELSE
      FAIL=12
      RETURN
    ENDIF
    ELEM(5)=1
    BADRUN=0

! ********************** HORIZONTAL VISIBILITY ************************

! Either a 4-figure group (metres) or a group ending SM (statute miles)
! Only certain multiples of 50, 100 or 1000 are allowed in the 4 figures

  ELSE IF ((GRPLEN.EQ.4 .AND. NCHAR.EQ.0)&
    &.OR. GROUP(GRPLEN-1:GRPLEN).EQ.'SM') THEN
    IF (ELEM(4).GT.0) THEN
      FAIL=12
      RETURN
    ENDIF

    VVVV=-9999999.
    IF (NCHAR.EQ.0) THEN
      IF ((GROUP(1:2).LT.'05' .AND. GROUP(3:4).EQ.'50') .OR.&
         &(GROUP(1:2).LT.'50' .AND. GROUP(3:4).EQ.'00') .OR.&
         &GROUP(2:4).EQ.'000') THEN
        READ (GROUP(1:4),'(F4.0)') VVVV
      ELSE IF (GROUP(1:4).EQ.'9999') THEN
        VVVV=10000
      ENDIF
    ELSE IF (GROUP(GRPLEN-2:GRPLEN-2).GE.'0' .AND.&
            &GROUP(GRPLEN-2:GRPLEN-2).LE.'9') THEN
      CALL TAFVISM(GROUP(1:GRPLEN),VVVV)
    ENDIF

    IF (VVVV.GT.0) THEN
IF (SECTION.EQ.0) THEN
  REXP(46)=VVVV
ELSE
  REXP(95+OFFSET)=VVVV
      ENDIF
      ELEM(4)=1
      BADRUN=0
    ENDIF

! ********************** WEATHER (CAVOK etc) **************************

! Expand CAVOK, NSC, SKC or NSW, setting corresponding flag.

  ELSE IF ((GRPLEN.EQ.5 .AND. GROUP(1:5).EQ.'CAVOK')&
     &.OR. (GRPLEN.EQ.3 .AND. (GROUP(1:3).EQ.'NSC' .OR.&
     &GROUP(1:3).EQ.'SKC' .OR. GROUP(1:3).EQ.'NSW'))) THEN
    IF (GROUP(1:3).EQ.'NSC') WXFLAG=1
    IF (GROUP(1:3).EQ.'CAV') WXFLAG=2
    IF (GROUP(1:3).EQ.'SKC') WXFLAG=4
    IF (GROUP(1:3).EQ.'NSW') WXFLAG=8

    IF (SECTION.EQ.0) THEN
      IF (REXP(24).LT.0) THEN
        REXP(24)=WXFLAG
      ELSE
        REXP(24)=REXP(24)+WXFLAG
      ENDIF
    ELSE
      IF (REXP(73+OFFSET).LT.0) THEN
        REXP(73+OFFSET)=WXFLAG
      ELSE
        REXP(73+OFFSET)=REXP(73+OFFSET)+WXFLAG
      ENDIF
    ENDIF
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

  ELSE IF (GRPLEN.EQ.NCHAR .AND. GRPLEN.LE.9&
         &.AND. ((GRPLEN.GE.2 .AND. MOD(GRPLEN,2).EQ.0&
         &.AND. INDEX(PAIRS,GROUP(1:2)).GT.0&
         &.AND. (GRPLEN.EQ.2 .OR. INDEX(PAIRS,GROUP(3:4)).GT.0))&
         &.OR. ((GROUP(1:1).EQ.'+' .OR. GROUP(1:1).EQ.'-')&
         & .AND. INDEX(PAIRS,GROUP(2:3)).GT.0))) THEN
    IF (ELEM(1).GT.2) THEN
      FAIL=12
      RETURN
    ENDIF

    DO LOOP=1,9
      WDWD(LOOP)=-9999999
    ENDDO
    ESTAR=1               ! always points to ELEM(1) for TAFs
    CALL MTRWWR(POINT1,REPORT,GRPLEN,ELEM,ESTAR,WDWD,REWW,LBAD)

! If there's no error, set the values returned in either the main
! section or a change section.

    IF (ELEM(1).GT.0 .AND. ELEM(1).LE.3 .AND. .NOT.LBAD) THEN
      IF (SECTION.EQ.0) THEN
        REXP(25+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3-2)
        REXP(26+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3-1)
        REXP(27+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3)
      ELSE
        REXP(74+OFFSET+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3-2)
        REXP(75+OFFSET+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3-1)
        REXP(76+OFFSET+(ELEM(1)-1)*3)=WDWD(ELEM(1)*3)
      ENDIF
    ENDIF
    IF (.NOT.LBAD) BADRUN=0

! ********************** VERTICAL VISIBILITY **************************

! Expand vertical visibility. If no vertical visibility & no cloud is
! reported, set sky obscured in the cloud slot.  For change sections
! the first cloud base slot is used, negative values being vertical
! vis - the two elements are mutually exclusive.

  ELSE IF (GRPLEN.EQ.5 .AND. GROUP(1:2).EQ.'VV'&
    &.AND. (CHARR(3:5).EQ.'///' .OR. CHARR(3:5).EQ.'NNN')) THEN
    IF (ELEM(11).GT.0) THEN
      FAIL=12
      RETURN
    ENDIF

    IF (SECTION.EQ.0) THEN
      IF (CHARR(3:5).EQ.'///') THEN
        REXP(34)=9
      ELSE
        REXP(47)=IVALUE(GROUP(3:5))*METRES
      ENDIF
    ELSE
      IF (CHARR(3:5).EQ.'///') THEN
        REXP(83+OFFSET)=9
      ELSE
        REXP(83+OFFSET)=-9999999.
        REXP(85+OFFSET)=-IVALUE(GROUP(3:5))*METRES
      ENDIF
    ENDIF
    ELEM(11)=1
    BADRUN=0

! ******************************* CLOUD *******************************

! Expand up to 3 cloud groups.  Each group is 3 letters & 3 figures.
! The letters are FEW, SCT (scattered), BKN (broken) or OVC (overcast).
! Groups should be in that order (non-decreasing amounts); so oktas
! checked in case a change section start not recognised.

  ELSE IF (GRPLEN.EQ.6 .AND. CHARR(1:6).EQ.'YYYNNN' .AND.&
          &(GROUP(1:3).EQ.'FEW' .OR. GROUP(1:3).EQ.'SCT' .OR.&
          &GROUP(1:3).EQ.'BKN' .OR. GROUP(1:3).EQ.'OVC')) THEN
    IF (ELEM(2).GT.2) THEN
      FAIL=16
      RETURN
    ENDIF

    IF (CLDAMT.GT.0) OLDAMT=CLDAMT
    ESTAR=2
    CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,&     !2.0
               &CLDAMT,CLDTYPE,CLDHT,VVIS)
    IF (CLDAMT.GE.8) THEN
      IF (OLDAMT.GE.8 .AND. OKTAS(CLDAMT).LT.OKTAS(OLDAMT)) THEN
        FAIL=20
        RETURN
      ENDIF

      IF (SECTION.EQ.0) THEN
        REXP(34+(ELEM(ESTAR)-1)*3)=CLDAMT
        REXP(35+(ELEM(ESTAR)-1)*3)=CLDTYPE
        REXP(36+(ELEM(ESTAR)-1)*3)=CLDHT
        REXP(47)=VVIS
      ELSE
        REXP(83+OFFSET+(ELEM(ESTAR)-1)*3)=CLDAMT
        REXP(84+OFFSET+(ELEM(ESTAR)-1)*3)=CLDTYPE
        REXP(85+OFFSET+(ELEM(ESTAR)-1)*3)=CLDHT
      ENDIF
    ENDIF
    BADRUN=0

! Towering cumulus or cumulonimbus (CB or TCU on end of normal group)
! (Only one such group allowed in a section)

  ELSE IF (GRPLEN.GE.8 .AND. CHARR(1:6).EQ.'YYYNNN' .AND.&
         &(GROUP(GRPLEN-1:GRPLEN).EQ.'CB' .OR.&
         &GROUP(GRPLEN-2:GRPLEN).EQ.'TCU')) THEN
    IF (ELEM(3).GT.0) THEN
      FAIL=12
      RETURN
    ENDIF

    ESTAR=3
    CALL MTRCCC(REPORT,POINT1,GRPLEN,CHARR,ELEM,ESTAR,&       !2.0
               &CLDAMT,CLDTYPE,CLDHT,VVIS)
    IF (SECTION.EQ.0) THEN
      REXP(43)=CLDAMT
      REXP(44)=CLDTYPE
      REXP(45)=CLDHT
    ELSE
      REXP(92+OFFSET)=CLDAMT
      REXP(93+OFFSET)=CLDTYPE
      REXP(94+OFFSET)=CLDHT
    ENDIF
    BADRUN=0

! *********** FORECAST TEMPERATURE (T?6MYTfTf/GfGfZ) ******************

! T may be followed by X (max) or N (min); M before figures if T<0.

  ELSE IF ((GROUP(1:2).EQ.'TX'.AND.GRPLEN.GE.9&
          &.AND.CHARR(GRPLEN-7:GRPLEN-3).EQ.'NN/NN'.AND.&
          &GROUP(GRPLEN:GRPLEN).EQ.'Z')&
        &.OR.(GROUP(1:2).EQ.'TX'.AND.GRPLEN.LE.8&
         &.AND.CHARR(GRPLEN-5:GRPLEN-1).EQ.'NN/NN'.AND.&
         &GROUP(GRPLEN:GRPLEN).EQ.'Z')) THEN

   IF (ELEM(10).GE.2) THEN
     FAIL=12
     RETURN
   ENDIF

   IF (GRPLEN.GE.10&
      &.AND.CHARR(GRPLEN-7:GRPLEN-1).EQ.'NN/NNNN')THEN
     HOUR=IVALUE(GROUP(GRPLEN-2:GRPLEN-1))
     DAY=IVALUE(GROUP(GRPLEN-4:GRPLEN-3))
     TEMP=IVALUE(GROUP(GRPLEN-7:GRPLEN-6))
   ENDIF

   IF (GRPLEN.GE.8&
      &.AND.CHARR(GRPLEN-5:GRPLEN-1).EQ.'NN/NN') THEN
     HOUR=IVALUE(GROUP(GRPLEN-2:GRPLEN-1))
     TEMP=IVALUE(GROUP(GRPLEN-5:GRPLEN-4))
   ENDIF

   IF (GROUP(GRPLEN-6:GRPLEN-6).EQ.'M'.OR.&
      &GROUP(GRPLEN-8:GRPLEN-8).EQ.'M') TEMP=-TEMP

     IF (GROUP(2:2).EQ.'X') THEN
       REXP(62)=DAY
       REXP(48)=HOUR
       REXP(49)=TEMP+KELVIN
     ENDIF

   ELSE IF ((GROUP(1:2).EQ.'TN'.AND.GRPLEN.GE.9&
            &.AND.CHARR(GRPLEN-7:GRPLEN-3).EQ.'NN/NN'.AND.&
            &GROUP(GRPLEN:GRPLEN).EQ.'Z')&
            &.OR.(GROUP(1:2).EQ.'TN'.AND.GRPLEN.LE.8&
            &.AND.CHARR(GRPLEN-5:GRPLEN-1).EQ.'NN/NN'.AND.&
            &GROUP(GRPLEN:GRPLEN).EQ.'Z')) THEN

     IF (ELEM(10).GE.2) THEN
       FAIL=12
       RETURN
     ENDIF

     IF (GRPLEN.GE.10.AND.CHARR(GRPLEN-7:GRPLEN-1)&
        &.EQ.'NN/NNN') THEN
       HOUR=IVALUE(GROUP(GRPLEN-2:GRPLEN-1))
       DAY=IVALUE(GROUP(GRPLEN-4:GRPLEN-3))
       TEMP=IVALUE(GROUP(GRPLEN-7:GRPLEN-6))
     ENDIF

     IF (GRPLEN.GE.8.AND.CHARR(GRPLEN-5:GRPLEN-1)&
         &.EQ.'NN/NN') THEN
       HOUR=IVALUE(GROUP(GRPLEN-2:GRPLEN-1))
       TEMP=IVALUE(GROUP(GRPLEN-5:GRPLEN-4))
     ENDIF

     IF (GROUP(GRPLEN-6:GRPLEN-6).EQ.'M'.OR.&
         &GROUP(GRPLEN-8:GRPLEN-8).EQ.'M') TEMP=-TEMP

     IF (GROUP(2:2).EQ.'N') THEN
       REXP(63)=DAY
       REXP(58)=HOUR
       REXP(59)=TEMP+KELVIN
     ENDIF

     ELEM(10)=ELEM(10)+1
     BADRUN=0

! ********************* ICING (6IchihihitL) ***************************
! (The icing group is from the old TAF code, but still used (in 2002)
!  by some American airfields)

! Icing type/level/thickness (6 figures starting with '6')

   ELSE IF (GRPLEN.EQ.6 .AND. NCHAR.EQ.0&
             &.AND. GROUP(1:1).EQ.'6') THEN
     IF (ELEM(8).GT.0) THEN
       FAIL=12
       RETURN
     ENDIF

! Expand icing severity Ic

     IF (SECTION.EQ.0) THEN
       REXP(52)=IVALUE(GROUP(2:2))

! Expand icing level hihihi (multiple of 30m)

       READ (GROUP(3:5),'(I3)') ICELV
       IF (ICELV.EQ.999) THEN
           ICELV=30000
       ELSE
         ICELV=ICELV*30
       ENDIF
       REXP(50)=ICELV

! Expand icing depth tL (single-figure multiple of 300m)

       REXP(51)=IVALUE(GROUP(6:6))*300
       ELEM(8)=1
     ENDIF

     BADRUN=0

! ********************* TURBULENCE (5BhBhBhBtL) ***********************
! (The turbulence group is from the old TAF code, but still used (in
!  2002) by 4 UK RAF airfields & some American airfields)

! See if this is a turbulence group (6 figures starting with '5').
! More than one 5-group can be reported in a section, usually one
! at the surface and one aloft; keep the first group.
! There is no slot for turbulence in change sections.

   ELSE IF (GRPLEN.EQ.6 .AND. NCHAR.EQ.0&
          &.AND. GROUP(1:1).EQ.'5') THEN

! Expand turbulence severity (B) & set frequency to 1 for occasional
! if B is even, 2 for frequent if B is odd.

     IF (SECTION.EQ.0 .AND. ELEM(9).EQ.0) THEN
       READ (GROUP(2:2),'(I1)') TURBIN
       REXP(53)=TURBIN
       IF (TURBIN.GE.2) REXP(56)=MOD(TURBIN,2)+1

! Expand turbulence height hBhBhB (multiple of 30m)

       READ (GROUP(3:5),'(I3)') TURBLV
       IF (TURBLV.EQ.999) THEN
         TURBLV=30000
       ELSE
         TURBLV=TURBLV*30
       ENDIF
       REXP(54)=TURBLV

! Expand turbulence thickness tL (single-figure multiple of 300m)

       REXP(55)=IVALUE(GROUP(6:6))*300
       ELEM(9)=1
     ENDIF
     BADRUN=0

! **************** Plain language? Unrecognised group? ****************

! Assume RMK is the start of plain language & stop expansion.
! If this group is not recognised, join it up with next & try again.
! (Use GROUP as work area: it will be reset straight away)

   ELSE IF (GROUP(1:4).EQ.'RMK ') THEN
       LREPFL=.TRUE.
   ELSE                          ! Group unrecognised
    BADRUN=BADRUN+1
     IF (BADRUN.EQ.1) THEN       ! First such group?
      BADLEN=GRPLEN
     ELSE IF (BADRUN.EQ.2) THEN  ! If next group unrecognised too,
      GROUP=REPORT(POINT1-BADLEN-1:POINT1-2)
      REPORT(POINT1-BADLEN:POINT1-1)=GROUP
      POINT1=POINT1-BADLEN
      GRPLEN=GRPLEN+BADLEN      ! try again with groups joined up.
     ELSE                        ! If retry fails, give up.
      FAIL=4
      LREPFL=.TRUE.
     ENDIF
   ENDIF
 END DO

! Copy the original report back to the start of the string if a    !2.1
! group has remained unrecognised after groups run together.       !2.1

IF (REPLEN.LT.LEN(REPORT)/2 .AND. FAIL.GE.4) THEN            !2.1
 REPORT(:REPLEN)=REPORT(LEN(REPORT)-REPLEN+1:)              !2.1
ENDIF                                                        !2.1
RETURN
END SUBROUTINE TAFEXP
