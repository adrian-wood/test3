SUBROUTINE BOYEXP(REPORT,RLEN,DATIME,ARRAY,DROGFLG)

!-----------------------------------------------------------------------
!
! PROGRAM       : BOYEXP
!
! PURPOSE       : Expand BUOY report into array for BUFR encoding
!
! CALLED BY     : BUOY
!
! CALLS         : IVALUE
!                 DATIM
!                 VALDDY - DATE CHECKING ROUTINE
!
! ARGUMENTS     : REPORT - BUOY report, starting 'ZZYY'            (I)
!                    (no CRLFs, only single spaces between groups)
!                 RLEN   - length of report                        (I)
!                 DATIME - report date/time                        (O)
!                 ARRAY  - values for encoding (see note below)    (O)
!                 DROGFLG- flag set if T/salin or current profile  (O)
!                    (This flag decides between two possible descriptor
!                     sequences for encoding, corresponding to array
!                     layouts with or without section 3 data)
!
!                 There is no error return as such, but BUOY will
!                 reject any ob with no lat/long; so any return before
!                 the longitude is set amounts to an error return.
!                    Nor is any count of good values returned.  Many
!                 buoy reports come in an element or two at a time
!                 over a period of several hours, so equal counts
!                 could refer to different elements.  Fortunately
!                 these subreports (to be combined as superobs) tend to
!                 come in at slightly different times and/or positions;
!                 otherwise all but the first would be lost!
!
! REVISION INFO :
!
! $Workfile: boyexp.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 14/01/2011 17:57:56$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         14/01/2011 17:57:56    Rosemary Lavery
!       updated comments (call list) on review
!  3    MetDB_Refresh 1.2         14/01/2011 11:30:31    Alison Weir
!       Correct character in Revision info
!  2    MetDB_Refresh 1.1         14/01/2011 11:21:03    Alison Weir     Ported
!        to f95 - MDBSTOR batch12
!  1    MetDB_Refresh 1.0         05/01/2011 17:04:51    Alison Weir
!       MDBSTOR batch 12 initial F77 version
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
USE datim_mod
USE ivalue_mod
USE valddy_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)  ::  REPORT    !A1
INTEGER,          INTENT(IN)  ::  RLEN      !A2 length of report
INTEGER,          INTENT(OUT) ::  DATIME(5) !A3 report date/time
REAL,             INTENT(OUT) ::  ARRAY(0:600) !A4 output array
LOGICAL,          INTENT(OUT) ::  DROGFLG   !A5 set if there's a profile

! Local declarations:

INTEGER   ::     SEC0      ! start of section 0 values in array
INTEGER   ::     SEC1      ! start of section 1 values
INTEGER   ::     SEC2      ! start of section 2 values
INTEGER   ::     SEC3      ! start of section 3 values
INTEGER   ::     SEC4      ! start of section 4 values
INTEGER   ::     CURT      ! start of current profile in array

INTEGER   ::     POS       ! pointer to current group in report
INTEGER   ::     S1START   ! pointer to start of section 1
INTEGER   ::     S2START   ! pointer to start of section 2
INTEGER   ::     S3START   ! pointer to start of section 3
INTEGER   ::     S4START   ! pointer to start of section 4
INTEGER   ::     SECTEND(0:4) ! pointers to ends of sections
INTEGER   ::     LASTSEC   ! last section found

INTEGER   ::     QUAD      ! quadrant of globe
INTEGER   ::     ACCLAT    ! latitude precision
INTEGER   ::     ACCLON    ! longitude precision
INTEGER   ::     IW        ! wind speed units flag
INTEGER   ::     SIGN      ! sign of temperature

INTEGER   ::     GRPCT     ! count of groups in section (to set q/c)
INTEGER   ::     NSALGP    ! number of temperature/salinity levels
INTEGER   ::     NCURGP    ! number of current levels
INTEGER   ::     QSAL      ! quality of T/salinity profile
INTEGER   ::     QCUR      ! quality of current profile
INTEGER   ::     IQAVAL
INTEGER   ::     IQLVAL
INTEGER   ::     IQTVAL

INTEGER   ::     NOW(8)    ! system date/time
INTEGER   ::     SYSYR     ! last figure of year from system date
INTEGER   ::     DAY       ! day
INTEGER   ::     MONTH     ! month
INTEGER   ::     YEAR      ! year
INTEGER   ::     I         ! short-term loop variable
INTEGER   ::     J         ! short-term loop variable

REAL      ::     MDATA     ! missing data
REAL      ::     TTTT      ! temperature
REAL      ::     QVAL      ! Qd from start of section 1
REAL      ::     DISP      ! Qx from start of section 1
REAL      ::     QC        ! q/c for all (or all-1) groups in section
REAL      ::     FRACTION  ! figures-to-real conversion for lat/long

LOGICAL   ::     KNOTS     ! set if wind speed in knots rather than m/s
LOGICAL   ::     TSALPRO   ! set if there's a T/salinity profile
LOGICAL   ::     CURPRO    ! set if there's a current profile

NSALGP=0
NCURGP=0
DROGFLG=.FALSE.
TSALPRO=.FALSE.
CURPRO=.FALSE.
KNOTS=.FALSE.
DATIME(:)=0

! Set first element in array to 3 to indicate that each quality element
! in the array will occupy 3 bits.  Set rest of array to missing.

MDATA=-9999999.0
ARRAY(0)=3.0
DO I=1,600
  ARRAY(I)=MDATA
END DO

! Check that report length goes beyond section 0

IF (RLEN <= 37) RETURN

!----------------------------------------------------------------------
!
! Go through report looking for section & profile starts
!
!----------------------------------------------------------------------

! After mandatory groups look for optional group (6QlQt//)

SECTEND(0)=37
POS=SECTEND(0)+1
IF (REPORT(POS:POS) == '6') THEN
  SECTEND(0)=SECTEND(0)+6
  POS=POS+6
END IF

! Check for section identifiers and set flags accordingly.
! Set beginning and end of each section.
! (Only loop as far as the start of section 4: between lat/long & there
! all groups have 5 figures, but 444 is only 3 figures, no added flags.)

S1START=0
S2START=0
S3START=0
S4START=0
LASTSEC=0

DOLABEL1: &
DO WHILE (POS+4 <= RLEN .AND. LASTSEC < 4)
!                                                     111QdQx
IFLABEL1: &
  IF (REPORT(POS-1:POS+2) == ' 111' .AND. LASTSEC < 1) THEN
    S1START=POS
    LASTSEC=1
!                                                     222QdQx
  ELSE IF (REPORT(POS-1:POS+2) == ' 222' .AND. LASTSEC < 2) THEN
    SECTEND(LASTSEC)=POS-1
    S2START=POS
    LASTSEC=2
!                                                     333Qd1Qd2
  ELSE IF (REPORT(POS-1:POS+2) == ' 333' .AND. LASTSEC < 3) THEN
    SECTEND(LASTSEC)=POS-1
    S3START=POS
    LASTSEC=3
!                                                     444 (only 3 figs)
  ELSE IF (REPORT(POS-1:POS+3) == ' 444 ') THEN
    SECTEND(LASTSEC)=POS-1
    S4START=POS
    LASTSEC=4

! In section 3 count depth groups in each profile, i.e. count levels
! (until next section or next profile or end...)
!  Set a flag when the initial group is found and count 2-groups while
! the flag is set; assume current profile follows T/salinity, so unset
! T/sal flag if & when initial current group found.
!  In the T/sal profile all groups have initial indicator figures, so
! we can simple count 2-groups; but in the current profile only the
! depth group has an indicator figure, so look at every other group.

  ELSE IF (LASTSEC == 3) THEN
    IF (REPORT(POS:POS+3) == '8887') THEN
      TSALPRO=.TRUE.
    ELSE IF (TSALPRO .AND. REPORT(POS:POS) == '2') THEN
      NSALGP=NSALGP+1

    ELSE IF (REPORT(POS:POS+1) == '66') THEN
      TSALPRO=.FALSE.
      CURPRO=.TRUE.
    ELSE IF (CURPRO .AND. REPORT(POS:POS) == '2') THEN
      NCURGP=NCURGP+1
      POS=POS+6
    END IF
  END IF IFLABEL1

  POS=POS+6
END DO DOLABEL1

! If no section with meteorological data has been found, give up.
! (Some reports have only the header & the 444 location section.)

IF (S1START == 0 .AND. S2START == 0 .AND. S3START == 0) RETURN

! Set section end for last section found

SECTEND(LASTSEC)=RLEN

! If either number of replications is nonzero, the buoy has a profile.

IF (NSALGP >= 1 .OR. NCURGP >= 1) DROGFLG=.TRUE.

! Set pointers to section starts in output value array.
! (Section 3 data is omitted if there's no profile, so there are two
! array layouts corresonding to two possible descriptor sequences -
! but why not have just one sequence, with zero counts if no profiles?)

SEC0=1
SEC1=SEC0+26
SEC2=SEC1+18

IF (DROGFLG) THEN
  SEC3=SEC2+10
  CURT=SEC3+6+1+6*NSALGP
  SEC4=CURT+4+1+6*NCURGP
ELSE
  SEC3=0
  CURT=0
  SEC4=SEC2+10
END IF

!----------------------------------------------------------------------
!
! Expand section 0  (identifier, time & place)
!
!----------------------------------------------------------------------

! Return to beginning of report (after MiMiMjMj) for identifier group

POS=6
ARRAY(SEC0+1)=IVALUE(REPORT(POS:POS+4))

! Date group (day/month/year: ddmmy)

POS=POS+6
DAY=IVALUE(REPORT(POS:POS+1))
MONTH=IVALUE(REPORT(POS+2:POS+3))
YEAR=IVALUE(REPORT(POS+4:POS+4))

! Use system year to set year from single figure in report:
! reject if not this year or last year.

CALL DATIM(NOW)
SYSYR=MOD(NOW(8),10)
IF (SYSYR == YEAR) THEN
  YEAR=NOW(8)
ELSE IF (SYSYR-1 == YEAR .OR. (SYSYR == 0 .AND. YEAR == 9)) THEN
  YEAR=NOW(8)-1
ELSE
  PRINT *,' BOYEXP: not this year or last year  ',REPORT(1:80)
  RETURN
END IF

! Check date using function VALDDY

IF (.NOT.VALDDY(DAY,MONTH,YEAR)) THEN
  PRINT*, ' BOYEXP: bad date  ',REPORT(1:80)
  RETURN
END IF

! Time group (hour/minute: hhmmiw).  Set minute to zero if not reported

POS=POS+6
DATIME(1)=YEAR
DATIME(2)=MONTH
DATIME(3)=DAY
DATIME(4)=IVALUE(REPORT(POS:POS+1))
DATIME(5)=IVALUE(REPORT(POS+2:POS+3))
IF (DATIME(5) == MDATA) DATIME(5)=0

! Put date/time elements in value array

ARRAY(SEC0+3)=YEAR
ARRAY(SEC0+5)=MONTH
ARRAY(SEC0+7)=DAY
ARRAY(SEC0+9)=DATIME(4)
ARRAY(SEC0+11)=DATIME(5)

! iw - units and source of windspeed.  Use it to set knots flag.

IW=IVALUE(REPORT(POS+4:POS+4))
IF (IW == 3.OR.IW == 4) KNOTS=.TRUE.
ARRAY(SEC0+25)=IW

! Latitude (6-figure group, quadrant at start; stop if only 5 figures;
!           but the last figure or 2 figures of the 6 can be slashes.)

POS=POS+6
IF (REPORT(POS+5:POS+5) == ' ') THEN
  PRINT *,' BOYEXP: 5-figure latitude group  ',REPORT(1:80)
  RETURN
END IF
QUAD=IVALUE(REPORT(POS:POS))

IF (QUAD /= 1 .AND.QUAD /= 3 .AND.QUAD /= 5 .AND.QUAD /= 7) THEN
  PRINT*,' BOYEXP: invalid quadrant  ',REPORT(1:80)
  RETURN
END IF

! Latitude can be in tenths, hundredths or thousandths.
! Latitude in tenths or hundredths (3 or 4 figures, coarse accuracy):

J=SEC0+17
IFLABEL2: &
IF (REPORT(POS+5:POS+5) == '/') THEN
  ACCLAT=0
  IF (REPORT(POS+4:POS+4) == '/') THEN
    ARRAY(J)=IVALUE(REPORT(POS+1:POS+3))
    FRACTION=0.1
  ELSE
    ARRAY(J)=IVALUE(REPORT(POS+1:POS+4))
    FRACTION=0.01
  END IF

! Latitude in thousandths (5 figures, fine accuracy):

ELSE
  ACCLAT=1
  ARRAY(J)=IVALUE(REPORT(POS+1:POS+5))
  FRACTION=0.001
END IF IFLABEL2

IF (ARRAY(J) /= MDATA) THEN
  ARRAY(J)=ARRAY(J)*FRACTION
  IF (QUAD == 3 .OR. QUAD == 5) ARRAY(J)=-ARRAY(J)
END IF

POS=POS+7

! Longitude can be in tenths, hundredths or thousandths.
! Longitude in tenths or hundredths (4 or 5 figures, coarse accuracy):

J=SEC0+19
IF (REPORT(POS+5:POS+5) == ' ') THEN
  PRINT *,'BOYEXP: 5-figure longitude group  ',REPORT(1:80)
  RETURN
END IF

IFLABEL3: &
IF (REPORT(POS+5:POS+5) == '/') THEN
  ACCLON=0.0
  IF (REPORT(POS+4:POS+4) == '/') THEN
    ARRAY(J)=IVALUE(REPORT(POS:POS+3))
    FRACTION=0.1
  ELSE
    ARRAY(J)=IVALUE(REPORT(POS:POS+4))
    FRACTION=0.01
  END IF

! Longitude in thousandths (6 figures, coarse accuracy):

ELSE
  ACCLON=1.0
  ARRAY(J)=IVALUE(REPORT(POS:POS+5))
  FRACTION=0.001
END IF IFLABEL3

IF (ARRAY(J) /= MDATA) THEN
  ARRAY(J)=ARRAY(J)*FRACTION
  IF (QUAD == 5 .OR. QUAD == 7) ARRAY(J)=-ARRAY(J)
END IF

POS=POS+7

! See if latitude & longitude have same precision. If not, set coarse.

IF (ACCLAT == ACCLON) THEN
  ARRAY(SEC0+13)=ACCLAT
ELSE
  ARRAY(SEC0+13)=0.0
END IF

! Check latitude & longitude are valid

IF (ARRAY(SEC0+17) < -90.0 .OR. ARRAY(SEC0+17) > 90.0) THEN
  PRINT*,' BOYEXP: invalid latitude  ',REPORT(1:80)
  RETURN
END IF

IF (ARRAY(SEC0+19) < -180.0 .OR. ARRAY(SEC0+19) > 180.0) THEN
  PRINT*,' BOYEXP: invalid longitude  ',REPORT(1:80)
  RETURN
END IF

! If section 0 optional group exists, set corresponding quality bits.
! Set time quality (Qt), values 0-5 (table 3334), on 5 time elements.

IFLABEL4: &
IF (SECTEND(0) > 37) THEN
  IQTVAL=IVALUE(REPORT(POS+2:POS+2))
  IF (IQTVAL < 0.0 .OR. IQTVAL > 5.0) IQTVAL=MDATA
  DO I=1,5
    ARRAY(SEC0+(2*I))=IQTVAL
  END DO

! Set quality (Ql), values 0-5 (table 3334), on both lat & long.
! QA is location confidence, a code figure corresponding to 66%
! confidence radius (see code table 3302).  N.B. this is stored
! as a separate element (002193) rather than a q/c field.

  IQLVAL=IVALUE(REPORT(POS+1:POS+1))
  IF (IQLVAL < 0.0 .OR. IQLVAL > 5.0) IQLVAL=MDATA
  ARRAY(SEC0+16)=IQLVAL
  ARRAY(SEC0+18)=IQLVAL

  IQAVAL=IVALUE(REPORT(POS+3:POS+3))
  ARRAY(SEC0+15)=IQAVAL
END IF IFLABEL4

!----------------------------------------------------------------------
!
! Expand section 1
!
!----------------------------------------------------------------------

IFLABEL5: &
IF (S1START > 0) THEN
  POS=S1START

! Set quality & bad group indicator (Qd,Qx).
! Qd (table 3334) is 0 if no check, 1 if OK, >1 if suspect or worse...
! Qx (see regulation 18.3.3) is 9 if all groups have same Qd or if
! more than one group has Qd>1; otherwise it indicates the bad group.

  QVAL=IVALUE(REPORT(POS+3:POS+3))
  DISP=IVALUE(REPORT(POS+4:POS+4))
  IF (QVAL < 0.0 .OR. QVAL > 5.0) QVAL=MDATA

! Set all QC flags to QVAL if DISP (Qx) is 9, one otherwise.
! If DISP<9 the value of one will be reset to QVAL for one group.

  IF (DISP == 9) THEN
    QC=QVAL
  ELSE
    QC=1
  END IF

  DO I=SEC1,SEC1+16,2
    ARRAY(I)=QC
  END DO

! Loop round groups, identified by first figure (no check on order).

  GRPCT=0
DOLABEL2: &
  DO WHILE (POS+6 <= RLEN .AND. POS+6 <= SECTEND(1))
    POS=POS+6
    GRPCT=GRPCT+1

! Wind group (0ddff)  Convert knots to m/s.

IFLABEL6: &
    IF (REPORT(POS:POS) == '0') THEN
      ARRAY(SEC1+1)=IVALUE(REPORT(POS+1:POS+2))
      IF (ARRAY(SEC1+1) /= MDATA) &
          ARRAY(SEC1+1)=ARRAY(SEC1+1)*10.

      ARRAY(SEC1+3)=IVALUE(REPORT(POS+3:POS+4))
      IF (ARRAY(SEC1+3) /= MDATA .AND. KNOTS) &
          ARRAY(SEC1+3)=ARRAY(SEC1+3)*0.5148
! Q/C
      IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC1)=QVAL
      IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC1+2)=QVAL

! Air temperature group (1sTTT).  Convert tenths Celsius to Kelvin.

    ELSE IF (REPORT(POS:POS) == '1') THEN IFLABEL6
      SIGN=IVALUE(REPORT(POS+1:POS+1))
      IF (SIGN == 1 .OR. SIGN == 0) THEN
        ARRAY(SEC1+5)=IVALUE(REPORT(POS+2:POS+4))
        IF (ARRAY(SEC1+5) /= MDATA) THEN
          ARRAY(SEC1+5)=ARRAY(SEC1+5)*0.1               ! tenths
          IF (SIGN == 1) ARRAY(SEC1+5)=-ARRAY(SEC1+5)   ! sign
          ARRAY(SEC1+5)=ARRAY(SEC1+5)+273.1             ! C to K
        END IF
! Q/C
        IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC1+4)=QVAL
      END IF

! Humidity group (29UUU or 2sTdTdTd).  If dew point, convert to Kelvin.

    ELSE IF (REPORT(POS:POS) == '2') THEN IFLABEL6
IFLABEL7: &
      IF (REPORT(POS+1:POS+1) == '9') THEN
        ARRAY(SEC1+9)=IVALUE(REPORT(POS+2:POS+4))
! Q/C
        IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC1+8)=QVAL
      ELSE
        SIGN=IVALUE(REPORT(POS+1:POS+1))
        IF (SIGN == 1 .OR. SIGN == 0) THEN
          ARRAY(SEC1+7)=IVALUE(REPORT(POS+2:POS+4))
          IF (ARRAY(SEC1+7) /= MDATA) THEN
            ARRAY(SEC1+7)=ARRAY(SEC1+7)*0.1             ! tenths
            IF (SIGN == 1) ARRAY(SEC1+7)=-ARRAY(SEC1+7) ! sign
            ARRAY(SEC1+7)=ARRAY(SEC1+7)+273.1           ! C to K
          END IF
! Q/C
          IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC1+6)=QVAL
        END IF
      END IF IFLABEL7

! Station level pressure (3PPPP).  Convert to Pascals, adding 1000mb
! if necessary (on the assumption that surface pressure >800mb).

    ELSE IF (REPORT(POS:POS) == '3') THEN IFLABEL6
      ARRAY(SEC1+11)=IVALUE(REPORT(POS+1:POS+4))
      IF (ARRAY(SEC1+11) /= MDATA) THEN
        IF (ARRAY(SEC1+11) < 8000) &
            ARRAY(SEC1+11)=ARRAY(SEC1+11)+10000.
        ARRAY(SEC1+11)=ARRAY(SEC1+11)*10.
      END IF
! Q/C
      IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC1+10)=QVAL

! MSL pressure (4PPPP).  Convert to Pascals as above.

    ELSE IF (REPORT(POS:POS) == '4') THEN IFLABEL6
      ARRAY(SEC1+13)=IVALUE(REPORT(POS+1:POS+4))
      IF (ARRAY(SEC1+13) /= MDATA) THEN
        IF (ARRAY(SEC1+13) < 8000) &
            ARRAY(SEC1+13)=ARRAY(SEC1+13)+10000.
        ARRAY(SEC1+13)=ARRAY(SEC1+13)*10.
      END IF
! Q/C
      IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC1+12)=QVAL

! 3-hour pressure tendency (5appp).  Convert to Pascals.

    ELSE IF (REPORT(POS:POS) == '5') THEN IFLABEL6
      ARRAY(SEC1+15)=IVALUE(REPORT(POS+1:POS+1))
      ARRAY(SEC1+17)=IVALUE(REPORT(POS+2:POS+4))
      IF (ARRAY(SEC1+17) /= MDATA) &
        ARRAY(SEC1+17)=ARRAY(SEC1+17)*10.
! Q/C
      IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC1+14)=QVAL
      IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC1+16)=QVAL
    END IF IFLABEL6
  END DO DOLABEL2

! Some BUOY reports have only station-level pressure, others only MSL;
! so if either is missing, set it from the other (at end of section 1).

  IF (ARRAY(SEC1+11) == MDATA) ARRAY(SEC1+11)=ARRAY(SEC1+13)
  IF (ARRAY(SEC1+13) == MDATA) ARRAY(SEC1+13)=ARRAY(SEC1+11)
END IF IFLABEL5

!----------------------------------------------------------------------
!
! Expand section 2
!
!----------------------------------------------------------------------

IFLABEL8: &
IF (S2START > 0) THEN
  POS=S2START

! Quality control figures handled as in section 1 - see notes there.

  QVAL=IVALUE(REPORT(POS+3:POS+3))
  IF (QVAL < 0.0 .OR. QVAL > 5.0) QVAL=MDATA
  DISP=IVALUE(REPORT(POS+4:POS+4))

  IF (DISP == 9) THEN
    QC=QVAL
  ELSE
    QC=1
  END IF

  DO I=SEC2,SEC2+8,2
    ARRAY(I)=QC
  END DO

  GRPCT=0
DOLABEL3: &
  DO WHILE (POS+6 <= SECTEND(2))
    POS=POS+6
    GRPCT=GRPCT+1

! Sea surface temperature (0sTwTwTw). Convert tenths Celsius to Kelvin.

IFLABEL9: &
    IF (REPORT(POS:POS) == '0') THEN
      SIGN=IVALUE(REPORT(POS+1:POS+1))
      ARRAY(SEC2+1)=IVALUE(REPORT(POS+2:POS+4))
      IF (ARRAY(SEC2+1) /= MDATA) THEN
        ARRAY(SEC2+1)=ARRAY(SEC2+1)*0.1                 ! tenths
        IF (SIGN == 1) ARRAY(SEC2+1)=-ARRAY(SEC2+1)     ! sign
        ARRAY(SEC2+1)=ARRAY(SEC2+1)+273.1               ! C to K
      END IF
! Q/C
      IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC2)=QVAL

! Wave period (seconds) & height (half-metres)

    ELSE IF (REPORT(POS:POS) == '1') THEN IFLABEL9
      ARRAY(SEC2+3)=IVALUE(REPORT(POS+1:POS+2))
      ARRAY(SEC2+5)=IVALUE(REPORT(POS+3:POS+4))
      IF (ARRAY(SEC2+5) /= MDATA) ARRAY(SEC2+5)=ARRAY(SEC2+5)*0.5
! Q/C
      IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC2+2)=QVAL
      IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC2+4)=QVAL

! Wave period (tenths of seconds) in 20... group

    ELSE IF (REPORT(POS:POS) == '2') THEN IFLABEL9
      IF (REPORT(POS+1:POS+1) == '0') THEN
        ARRAY(SEC2+7)=IVALUE(REPORT(POS+2:POS+4))
        IF(ARRAY(SEC2+7) /= MDATA) ARRAY(SEC2+7)=ARRAY(SEC2+7)*0.1
! Q/C
        IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC2+6)=QVAL

! Wave height (tenths of metres) in 21... group

      ELSE IF (REPORT(POS+1:POS+1) == '1') THEN
        ARRAY(SEC2+9)=IVALUE(REPORT(POS+2:POS+4))
        IF(ARRAY(SEC2+9) /= MDATA) ARRAY(SEC2+9)=ARRAY(SEC2+9)*0.1
! Q/C
        IF (DISP < 9 .AND. GRPCT == DISP) ARRAY(SEC2+8)=QVAL
      END IF
    END IF IFLABEL9
  END DO DOLABEL3
END IF IFLABEL8

!----------------------------------------------------------------------
!
! Expand section 3 (temperature/salinity & current profiles)
! - but only if some levels were found in the preliminary scan!
!
!----------------------------------------------------------------------

IFLABEL10: &
IF (DROGFLG) THEN
  POS=S3START

! Get temperature/salinity & current profile quality from 333 group.

  QSAL=IVALUE(REPORT(POS+3:POS+3))
  IF (QSAL < 0.0 .OR. QSAL > 5.0) QSAL=MDATA
  QCUR=IVALUE(REPORT(POS+4:POS+4))
  IF (QCUR < 0.0 .OR. QCUR > 5.0) QCUR=MDATA
  POS=POS+6

! Expand temperature/salinity profile.
! First get method of salinity measurement (k2) from 8887k2 group

  ARRAY(SEC3+6)=NSALGP
IFLABEL11: &
  IF (NSALGP > 0) THEN
    ARRAY(SEC3+5)=IVALUE(REPORT(POS+4:POS+4))
    POS=POS+6

! Depth (in metres)

DOLABEL4: &
    DO I=1,NSALGP
      IF (REPORT(POS:POS) == '2') THEN
        ARRAY(SEC3+8+6*(I-1))=IVALUE(REPORT(POS+1:POS+4))
        ARRAY(SEC3+7+6*(I-1))=QSAL
        POS=POS+6
      END IF

! Temperature at depth
! (this can be negative - 50 added to unsigned whole degrees -
!  and/or in tenths rather than hundredths - last figure slashed)

IFLABEL12: &
      IF (REPORT(POS:POS) == '3') THEN
        IF (REPORT(POS+4:POS+4) == '/') THEN   ! tenths
          TTTT=IVALUE(REPORT(POS+1:POS+3))
          IF (TTTT /= MDATA) TTTT=TTTT*0.1
        ELSE                                   ! hundredths
          TTTT=IVALUE(REPORT(POS+1:POS+4))
          IF (TTTT /= MDATA) TTTT=TTTT*0.01
        END IF
        IF (TTTT >= 50.) TTTT=50.-TTTT
        IF (TTTT /= MDATA)ARRAY(SEC3+10+6*(I-1))=TTTT+273.15

        ARRAY(SEC3+9+6*(I-1))=QSAL
        POS=POS+6
      END IF IFLABEL12

! Salinity at depth (hundredths of parts per thousand reported)

      IF (REPORT(POS:POS) == '4') THEN
        J=SEC3+12+6*(I-1)
        ARRAY(J)=IVALUE(REPORT(POS+1:POS+4))
        ARRAY(J-1)=QSAL
        IF (ARRAY(J) /= MDATA) ARRAY(J)=ARRAY(J)*0.01
        POS=POS+6
      END IF
    END DO DOLABEL4
  END IF IFLABEL11

! Expand current profile

  ARRAY(CURT+4)=NCURGP
IFLABEL13: &
  IF (NCURGP > 0) THEN

! Method of removing ship's velocity from current (k6) and
! duration and time of current measurement (k3), both from 66?9?-group.

    ARRAY(CURT+1)=IVALUE(REPORT(POS+2:POS+2))
    ARRAY(CURT+3)=IVALUE(REPORT(POS+4:POS+4))
    POS=POS+6

DOLABEL5: &
    DO I=1,NCURGP
IFLABEL14: &
      IF (REPORT(POS:POS) == '2') THEN

! Depth (in metres) in 2-group; ddfff groups have no identifier.

        ARRAY(CURT+5+6*(I-1))=QCUR
        ARRAY(CURT+6+6*(I-1))=IVALUE(REPORT(POS+1:POS+4))
        POS=POS+6

! Current direction: convert from tens to whole degrees

        J=CURT+8+6*(I-1)
        ARRAY(J-1)=QCUR
        ARRAY(J)=IVALUE(REPORT(POS:POS+1))
        IF (ARRAY(J) /= MDATA) ARRAY(J)=ARRAY(J)*10

! Current speed: convert from cm/s to m/s

        J=CURT+10+6*(I-1)
        ARRAY(J-1)=QCUR
        ARRAY(J)=IVALUE(REPORT(POS+2:POS+4))
        IF (ARRAY(J) /= MDATA) ARRAY(J)=ARRAY(J)*0.01
        POS=POS+6
      END IF IFLABEL14
    END DO DOLABEL5
  END IF IFLABEL13
END IF IFLABEL10

!----------------------------------------------------------------------
!
! Expand section 4
!
!----------------------------------------------------------------------

IFLABEL15: &
IF (S4START > 0) THEN
  POS=S4START
  POS=POS+4

! Set q/c flags Qp, Q2, QTW & Q4 as bits in one value (033250).
! N.B. 033250 is a 6-figure flag table, and these 4 flags are bits 1-4
! - so the power of two added should be twice what's below!
! (Better to change description of flag table than change this code?)
  IF (POS+4 <= RLEN .AND. REPORT(POS:POS) == '1') THEN
    J=0
    IF (REPORT(POS+1:POS+1) == '1') J=J+16
    IF (REPORT(POS+2:POS+2) == '1') J=J+8
    IF (REPORT(POS+3:POS+3) == '1') J=J+4
    IF (REPORT(POS+4:POS+4) == '1') J=J+2
    ARRAY(SEC4+1)=J
    POS=POS+6
  ELSE
    ARRAY(SEC4+1)=0.0
  END IF

! QN/QL/QA group (see code tables 3313, 3311, 3302).
! QN (transmission quality) is 0 if good or 1 if dubious.
! QL is quality of location & QA radius of 66% confidence.

  IF (POS+4 <= RLEN .AND. REPORT(POS:POS) == '2') THEN
    IF (REPORT(POS+1:POS+1) == '1') THEN
      ARRAY(SEC4+1)=ARRAY(SEC4+1)+1.0
    END IF

    ARRAY(SEC4+3)=IVALUE(REPORT(POS+2:POS+2))
    ARRAY(SEC4+5)=IVALUE(REPORT(POS+3:POS+3))
    POS=POS+6
  END IF

! Check for alternative latitude and longitude (6-figure) groups

IFLABEL16: &
  IF (POS+12 <= RLEN .AND. REPORT(POS+5:POS+5) /= ' ') THEN
    QUAD=IVALUE(REPORT(POS:POS))
IFLABEL17: &
    IF (QUAD == 1 .OR.QUAD == 3 .OR.QUAD == 5 .OR.QUAD == 7) THEN

! Second possible latitude (3 or 4 or 5 figures)

      J=SEC4+13
      IF (REPORT(POS+5:POS+5) == '/') THEN
        IF (REPORT(POS+4:POS+4) == '/') THEN
          ARRAY(J)=IVALUE(REPORT(POS+1:POS+3))
          FRACTION=0.1
        ELSE
          ARRAY(J)=IVALUE(REPORT(POS+1:POS+4))
          FRACTION=0.01
        END IF
      ELSE
        ARRAY(J)=IVALUE(REPORT(POS+1:POS+5))
        FRACTION=0.001
      END IF

      IF (ARRAY(J) /= MDATA) THEN
        ARRAY(J)=ARRAY(J)*FRACTION
        IF (QUAD == 3 .OR. QUAD == 5) ARRAY(J)=-ARRAY(J)
      END IF

      POS=POS+7

! Second possible longitude (4 or 5 or 6 figures)

      J=SEC4+15
      IF (REPORT(POS+5:POS+5) == '/') THEN
        IF (REPORT(POS+4:POS+4) == '/') THEN
          ARRAY(J)=IVALUE(REPORT(POS:POS+3))
          FRACTION=0.1
        ELSE
          ARRAY(J)=IVALUE(REPORT(POS:POS+4))
          FRACTION=0.01
        END IF
      ELSE
        ARRAY(J)=IVALUE(REPORT(POS:POS+5))
        FRACTION=0.001
      END IF

      IF (ARRAY(J) /= MDATA) THEN
        ARRAY(J)=ARRAY(J)*FRACTION
        IF (QUAD == 5 .OR. QUAD == 7) ARRAY(J)=-ARRAY(J)
      END IF

      POS=POS+7
    END IF IFLABEL17

! If no long/lat group, there may be a date/time (2 groups) or a drift
! group (7-group) or both.  The date group starts with a day, so its
! first figure can't be more than 3; the time group ends with a slash.

  ELSE
IFLABEL18: &
    IF (POS+10 <= RLEN .AND. REPORT(POS+10:POS+10) == '/' &
                       .AND. REPORT(POS:POS) <= '3') THEN
      ARRAY(SEC4+21)=IVALUE(REPORT(POS:POS+1))
      ARRAY(SEC4+19)=IVALUE(REPORT(POS+2:POS+3))
      ARRAY(SEC4+17)=IVALUE(REPORT(POS+4:POS+4))

      IF (ARRAY(SEC4+21) > 31) ARRAY(SEC4+21)=MDATA
      IF (ARRAY(SEC4+19) > 12) ARRAY(SEC4+19)=MDATA

! Set this year or last year from system year & single figure in report

      IF (SYSYR == INT(ARRAY(SEC4+17))) THEN
        ARRAY(SEC4+17)=NOW(8)
      ELSE IF ((SYSYR-1) == INT(ARRAY(SEC4+17))) THEN
        ARRAY(SEC4+17)=NOW(8)-1
      ELSE
        ARRAY(SEC4+17)=MDATA
      END IF
      POS=POS+6

! Hour & minute (if minute missing set it to zero)

      ARRAY(SEC4+23)=IVALUE(REPORT(POS:POS+1))
      ARRAY(SEC4+25)=IVALUE(REPORT(POS+2:POS+3))
      IF (ARRAY(SEC4+25) == MDATA) ARRAY(SEC4+25)=0.0
      POS=POS+6
    END IF IFLABEL18

! Drift speed & direction group (7VBVBdBdB)
! Convert speed from cm/s to m/s & direction from tens to whole degrees

    IF (POS+4 <= RLEN .AND. REPORT(POS:POS) == '7') THEN
      ARRAY(SEC0+21)=IVALUE(REPORT(POS+1:POS+2))
      IF (ARRAY(SEC0+21) /= MDATA) &
          ARRAY(SEC0+21)=ARRAY(SEC0+21)*0.01

      ARRAY(SEC0+23)=IVALUE(REPORT(POS+3:POS+4))
      IF (ARRAY(SEC0+23) /= MDATA) &
          ARRAY(SEC0+23)=ARRAY(SEC0+23)*10.0
      POS=POS+6
    END IF
  END IF IFLABEL16

! Engineering groups (8ViViViVi), up to 3 of them

  I=2
  DO WHILE (POS+4 <= RLEN.AND.REPORT(POS:POS) == '8'.AND.I <= 4)
    ARRAY(SEC4+4+(2*I)-1)=IVALUE(REPORT(POS+1:POS+4))
    POS=POS+6
    I=I+1
  END DO

! Drogue group (9idZdZdZd): drogue type & depth.
! N.B. This group is ignored if there's no profile; but in Nov 2001 it
! is often reported when there's no profile & never (?) when there is!
! But without a profile there's no section 3 in the array, so drogue
! depth can't be stored (with the existing descriptor sequence).

  IF (POS+4 <= RLEN.AND.REPORT(POS:POS) == '9'.AND.DROGFLG) THEN
    ARRAY(SEC3+1)=IVALUE(REPORT(POS+1:POS+1))
    ARRAY(SEC3+3)=IVALUE(REPORT(POS+2:POS+4))
  END IF
END IF IFLABEL15

! Print array if diagnostics needed:

!     PRINT *,REPORT(1:RLEN)

!     PRINT *,'buoy: ',IFIX(ARRAY(2)),' lat/long: ',(ARRAY(I),I=18,24,2)
!     PRINT *,'date/time:',(IFIX(ARRAY(I)),I=4,12,2)
!     PRINT *,'wind, T,Td,RH: ',(ARRAY(SEC1+I),I=1,9,2)
!     PRINT *,'pressures & tendency: ',(ARRAY(SEC1+I),I=11,17,2)
!     PRINT *,'Tw, wave period & ht: ',(ARRAY(SEC2+I),I=1,9,2)
!     IF (DROGFLG) PRINT *,' drogue depth: ',ARRAY(SEC3+3)

!     IF (NSALGP > 0) PRINT *,NSALGP,'levels in T/salinity profile'
!     DO J=SEC3,SEC3+(NSALGP-1)*6,6
!       PRINT *,ARRAY(J+8),ARRAY(J+10),ARRAY(J+12)
!     END DO

!     IF (NCURGP > 0) PRINT *,NCURGP,'levels in current profile'
!     DO J=CURT,CURT+(NCURGP-1)*6,6
!       PRINT *,ARRAY(J+6),ARRAY(J+8),ARRAY(J+10)
!     END DO

!     IF (S4START > 0) THEN
!       PRINT *,'time of last position',(IFIX(ARRAY(SEC4+I)),I=17,25,2)
!     END IF
!     PRINT *,' '
RETURN
END SUBROUTINE BOYEXP
