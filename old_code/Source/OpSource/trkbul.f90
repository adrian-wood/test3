SUBROUTINE TRKBUL(BULL,L,TTAAII,CCCC,YYGGGG,CORN,IFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : TRKBUL
!
! PURPOSE       : To store TRACKOBs from one bulletin
!
! DESCRIPTION   : A bulletin can contain several daily reports
!                 starting NNXX & ending with call sign; each
!                 daily report can contain many obs for given
!                 times & lat/longs.  A BUFR message is stored
!                 for each such ob.  (No characters stored.)
!                   A report is abandoned (i.e. go to next NNXX)
!                 if a bad date, time or lat/long group is found.
!
! DATA TYPE(S)  : TRACKOB (FM-62-VIII)
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED, CCCODE, IVALUE, ENBUFR, INDLALO, AIRSTO, IDES
!
! ARGUMENTS     : (1) Bulletin (will be tidied up by BULLED)       (I/O)
!                 (2) Length of bulletin (changed by BULLED)       (I/O)
!                 (3) TTAAii of report (for trailer)                (I)
!                 (4) Collecting centre (for trailer)               (I)
!                 (5) Date/Time of bulletin (not used)              (I)
!                 (6) Correction number (for trailer)               (I)
!                 (7) Storage unit number (for AIRSTO)              (I)
!
! REVISION INFO :
!
! $Workfile: trkbul.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 25/01/2011 21:46:44$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         25/01/2011 21:46:44    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         25/01/2011 15:12:42    John Norton     After
!       initial porting of batches MDBSTOR 19 & 23
!  1    MetDB_Refresh 1.0         19/01/2011 11:28:01    John Norton
!       Pre-porting f77 version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE airsto_mod
USE bulled_mod
USE cccode_mod
USE datim_mod
USE enbufr_mod
USE ides_mod
USE indlalo_mod
USE ivalue_mod ! function

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(INOUT) :: BULL   !a01 bulletin
INTEGER,          INTENT(INOUT) :: L      !a02 length of bulletin
CHARACTER(LEN=6), INTENT(IN)    :: TTAAII !a03 bulletin header
CHARACTER(LEN=4), INTENT(IN)    :: CCCC   !a04 collecting centre
CHARACTER(LEN=6), INTENT(IN)    :: YYGGGG !a05 date/time of bulletin
CHARACTER(LEN=2), INTENT(IN)    :: CORN   !a06 report correction number
INTEGER,          INTENT(IN)    :: IFT    !a07 FT number of storage dataset

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  CC ! current speed in tenths, as reported
INTEGER          ::  CPERIOD ! averaging period for current
INTEGER          ::  DATIME(5) ! data time for AIRSTO
INTEGER          ::  DAY
INTEGER          ::  DD ! current direction in tens of degrees
INTEGER          ::  DESCR(30) ! descriptor array with room for expansion
INTEGER          ::  HOUR
INTEGER          ::  I ! loop variable
INTEGER          ::  ICCCC ! number corresponding to CCCC
INTEGER          ::  IVER  ! New argument for use in call to ENBUFR
INTEGER          ::  J ! pointer within bulletin
INTEGER          ::  JX ! to find start of report (NNXX)
INTEGER          ::  KNOTS ! 4-group indicator for current speed unit
INTEGER          ::  LAST ! pointer to before call sign at end of rep
INTEGER          ::  LAT ! latitude in hundredths
INTEGER          ::  LCS ! length of call sign
INTEGER          ::  LEN ! length of BUFR message
INTEGER          ::  LONG ! longitude in hundredths
INTEGER          ::  MIN
INTEGER          ::  MONTH
INTEGER          ::  NDESCR ! descriptor count for ENBUFR
INTEGER          ::  NOBS ! one ob per BUFR message
INTEGER          ::  NOW(8) ! time from DATIM call
INTEGER          ::  NVALUES ! number of met elements (to go in index)
INTEGER          ::  Q ! quadrant
INTEGER          ::  SAL ! salinity in hundredths of parts per thou
INTEGER          ::  SPERIOD ! averaging period for salinity
INTEGER          ::  TOR(5) ! time of receipt (year, month,...)
INTEGER          ::  TPERIOD ! averaging period for temperature
INTEGER          ::  TW ! water temperature in tenths Celsius
INTEGER          ::  YEAR

REAL             ::  VALUES(17) ! values to be encoded

CHARACTER(LEN=9)   ::  CALL_SIGN  ! call sign
CHARACTER(LEN=23)  ::  ENTRY      ! index entry for AIRSTO & storage
CHARACTER(LEN=9)   ::  IDENT      ! copy of call sign for ENBUFR to translate
CHARACTER(LEN=100) ::  MESSAGE    ! message to be stored

LOGICAL          ::  ERROR ! set if error in date, time or lat/long

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! Call CCCODE to get integer collecting centre number from 001031 table.

CALL CCCODE(287,ICCCC,CCCC)

! Call BULLED to remove extra spaces & set control characters to spaces.

CALL BULLED(1,L,BULL)

! Set values which are always the same (pointer to ident & zero depth)
! & time of receipt from current time.

VALUES(1)=1.
VALUES(9)=0

CALL DATIM(NOW)
DO I=1,5
  TOR(I)=NOW(9-I)
END DO

! Find start of report ('NNXX', followed by date group) & end of report
! (delimited by either next 'NNXX' or end of bulletin).
! (The -3 & -2 in the LAST= statements are meant to point to the last
! character of the call sign rather than an '=' or space after it.)

J=1
DOLABEL1: &
DO WHILE (J < L)
  JX=INDEX(BULL(J:),'NNXX')
  IF (JX == 0) RETURN
  J=J+JX-1
!                                      One NNXX found; look for another
  LAST=INDEX(BULL(J+5:),'NNXX')
  IF (LAST > 0) THEN
    LAST=J+5+LAST-3
  ELSE                         ! If no more NNXX's, bulletin end
    LAST=L-2
  END IF

! Go back to start of last group, which must be call sign.
! Make sure call sign doesn't end with equal sign.

  LCS=0
  DO WHILE (BULL(LAST:LAST) /= ' ')
    LCS=LCS+1
    LAST=LAST-1
  END DO

  CALL_SIGN=BULL(LAST+1:LAST+LCS)
  IF (INDEX(CALL_SIGN,'=') > 0) THEN
    CALL_SIGN=CALL_SIGN(:INDEX(CALL_SIGN,'=')-1)
  END IF

! Move past NNXX.  Date group follows. (Assumed to be within last 9 yrs)

  J=J+5
  DAY=IVALUE(BULL(J:J+1))
  MONTH=IVALUE(BULL(J+2:J+3))
  YEAR=IVALUE(BULL(J+4:J+4)) + 10*(TOR(1)/10)
  IF (YEAR > TOR(1)) YEAR=YEAR-10

  ERROR=.FALSE.
  IF (MONTH < 0 .OR. DAY < 0 .OR. YEAR < 0 .OR. &
      MONTH > 12 .OR. DAY > 31) THEN
    PRINT *,'TRKBUL: bad DATE group ',BULL(J:J+4)
    ERROR=.TRUE.
  END IF
  J=J+6

! Set these items to missing data (will be reset if a 4-group is found)

  TPERIOD=-9999999
  SPERIOD=-9999999
  CPERIOD=-9999999
  KNOTS=-9999999

! Set values which are so until another NNXX overrides them

  VALUES(2)=YEAR
  VALUES(3)=MONTH
  VALUES(4)=DAY

! Now find obs between date & call sign.  Each starts with three
! mandatory groups, time, latitude & longitude.  The time group
! starts with an hour (first figure 0, 1 or 2), the latitude group
! with a quadrant (1, 3, 5 or 7), the longitude with 0 or 1.
!    The optional groups start 4, 6, 8 or 9 (assume they appear
! in that order), so are clearly distinguished.  (The 4-group is
! mandatory in the first ob, then only included if different.)
!    The loop below assumes that order, going to the next NNXX if
! groups are out of step.  (BULLED replaced new-line characters
! by spaces, so we can't just go on to the next line!)
!    So first check that a time group follows (ending with '/').
!    Accept missing minutes (setting them to zero).

DOLABEL2: &
  DO WHILE (J < LAST-20 .AND. .NOT.ERROR)
    NVALUES=0

! Initialise values from groups which may not be reported this time.

    DO I=10,17
      VALUES(I)=-9999999.
    END DO

    HOUR=IVALUE(BULL(J:J+1))
    MIN=IVALUE(BULL(J+2:J+3))
    IF (HOUR < 0 .OR. HOUR >= 24 .OR. MIN >= 60 &
       .OR. BULL(J+4:J+4) /= '/') THEN
      PRINT *,'TRKBUL: bad time group ',BULL(J:J+4)
      ERROR=.TRUE.
    END IF
    J=J+6

    IF (MIN < 0) MIN=0
    VALUES(5)=HOUR
    VALUES(6)=MIN

! Latitude & longitude (in hundredths) are unsigned, with a quadrant
! before the latitude.

    Q=IVALUE(BULL(J:J))
    LAT=IVALUE(BULL(J+1:J+4))
    LONG=IVALUE(BULL(J+6:J+10))
    J=J+12

    IF (LAT < 0 .OR. LAT > 9000 .OR. &
        LONG < 0 .OR. LONG > 18000 .OR. &
        .NOT.(Q == 1 .OR. Q == 3 .OR. Q == 5 .OR. Q == 7)) THEN
      PRINT *,'TRKBUL: bad lat/long groups ',BULL(J:J+10)
      ERROR=.TRUE.
    END IF

    IF (Q == 3 .OR. Q == 5) LAT=-LAT
    IF (Q == 5 .OR. Q == 7) LONG=-LONG

    VALUES(7)=REAL(LAT)/100.
    VALUES(8)=REAL(LONG)/100.

! Lat/long may be followed by a 4-group (mandatory the first time)
! giving averaging periods (code figures) & units of current speed.
! (A code figure of 9 means missing. Otherwise multiply by 15 to give
! a representative number of minutes: see table 2604 in WMO Manual.)

IFLABEL1: &
    IF (BULL(J:J) == '4') THEN
      TPERIOD=IVALUE(BULL(J+1:J+1))
      SPERIOD=IVALUE(BULL(J+2:J+2))
      CPERIOD=IVALUE(BULL(J+3:J+3))
      KNOTS=IVALUE(BULL(J+4:J+4))

      IF (TPERIOD >= 0 .AND. TPERIOD < 9) TPERIOD=TPERIOD*15
      IF (SPERIOD >= 0 .AND. SPERIOD < 9) SPERIOD=SPERIOD*15
      IF (CPERIOD >= 0 .AND. CPERIOD < 9) CPERIOD=CPERIOD*15

      IF (TPERIOD == 9) TPERIOD=-9999999
      IF (SPERIOD == 9) SPERIOD=-9999999
      IF (CPERIOD == 9) CPERIOD=-9999999

      J=J+6
    END IF IFLABEL1

! A 6-group (optional) is water temperature (sign, then tenths Celsius)

    IF (BULL(J:J) == '6') THEN
      TW=IVALUE(BULL(J+2:J+4))
      IF (BULL(J+1:J+1) == '1') TW=-TW
      NVALUES=NVALUES+1
      J=J+6

      VALUES(10)=TPERIOD
      IF (TW /= -9999999) VALUES(11)=REAL(TW)/10.+273.1
    END IF

! An 8-group (optional) is salinity (hundredths of parts per thousand)

    IF (BULL(J:J) == '8') THEN
      SAL=IVALUE(BULL(J+1:J+4))
      NVALUES=NVALUES+1
      J=J+6

      VALUES(12)=SPERIOD
      IF (SAL /= -9999999) VALUES(13)=REAL(SAL)/100.
    END IF

! A 9-group (optional) is current (speed in knots or m/s, in tenths).
! Only set speed if a 4-group has given the units (0: m/s, 1: knots).

IFLABEL2: &
    IF (BULL(J:J) == '9') THEN
      DD=IVALUE(BULL(J+1:J+2))
      CC=IVALUE(BULL(J+3:J+4))
      NVALUES=NVALUES+1
      J=J+6

      VALUES(14)=CPERIOD
      IF (DD /= -9999999) VALUES(15)=DD*10.
      VALUES(16)=CPERIOD

      IF (KNOTS >= 0 .AND. CC /= -9999999) THEN
        VALUES(17)=REAL(CC)/10.
        IF (KNOTS == 1) VALUES(17)=VALUES(17)*1852/3600
      END IF
    END IF IFLABEL2

! Encode a BUFR message

IFLABEL3: &
    IF (.NOT.ERROR) THEN
      DESCR(1)=IDES(303192)
      NDESCR=1
      NOBS=1
      IDENT=CALL_SIGN
      CALL ENBUFR(DESCR,VALUES,NDESCR,17,NOBS,IDENT, &
                  TOR,MESSAGE,.FALSE.,LEN,IVER)

! Set originating centre & data type in section 1 of BUFR message
! (assuming version 1; change displacement if total length at start)

      IF (ICCCC >= 0) THEN
        MESSAGE(9:9)=CHAR(ICCCC/256)
        MESSAGE(10:10)=CHAR(MOD(ICCCC,256))
      END IF
      MESSAGE(13:13)=CHAR(31)

! Set bytes 3-16 of index entry (rest will be filled in by AIRSTO)

      ENTRY(3:11)=TTAAII(1:4)//CORN(2:2)//CCCC
      ENTRY(12:12)=CHAR(NVALUES)
      CALL INDLALO(ENTRY,VALUES(7),VALUES(8))

! Put time of data in integer array for AIRSTO & store message

      DO I=1,5
        DATIME(I)=VALUES(I+1)
      END DO
      CALL AIRSTO(DATIME,ENTRY,MESSAGE(:LEN), &
                  IFT,27998,CALL_SIGN,TOR)
    END IF IFLABEL3
  END DO DOLABEL2   ! end of loop round lines of report (messages stored)
END DO DOLABEL1     ! end of loop round reports starting 'NNXX date'
RETURN
END
