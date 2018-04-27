      SUBROUTINE TRKBUL(BULL,L,TTAAII,CCCC,YYGGGG,CORN,IFT)

      IMPLICIT NONE

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
! CALLS         : BULLED, CCCODE, IVALUE, ENBUFR, INDLALO, AIRSTO
!
! PARAMETERS    : (1) Bulletin (will be tidied up by BULLED)       (I/O)
!                 (2) Length of bulletin (changed by BULLED)       (I/O)
!                 (3) TTAAii of report (for trailer)                (I)
!                 (4) Collecting centre (for trailer)               (I)
!                 (5) Date/Time of bulletin (not used)              (I)
!                 (6) Correction number (for trailer)               (I)
!                 (7) Storage unit number (for AIRSTO)              (I)
!
! REVISION INFO :
!
! $Workfile: trkbul.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 04/11/2008 14:54:38$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         04/11/2008 14:54:38    Brian Barwell   Check
!        for missing data before scaling data values.
!  1    Met_DB_Project 1.0         30/01/2006 20:25:28    Sheila Needham  
! $
! Revision 2.1  2004/03/03 16:07:38  usmdb
! 15 March 2004     C Long
! 2.1  Complete rewrite of TRACKOB storage: old TRKEXC etc incorporated,
!      AIRSTO used rather than TAFREP.
!
! Revision 2.0  2001/07/03  10:44:23  10:44:23  usmdb (MetDB account c/o usjh)
! Removed unused variables. Removed argument REPEND from
! call to TRKEXC as not used in TRKEXC. Added copyright and
! modified header - S.Cox
!
! Revision 1.1  98/06/11  09:45:51  09:45:51  usmdb (Generic MetDB accou
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      INTEGER J              ! pointer within bulletin
      INTEGER L              ! length of bulletin
      INTEGER JX             ! to find start of report (NNXX)
      INTEGER I              ! loop variable
      INTEGER LAST           ! pointer to before call sign at end of rep
      INTEGER LCS            ! length of call sign
      INTEGER ICCCC          ! number corresponding to CCCC
      INTEGER IFT            ! FT number of storage dataset
      INTEGER IVALUE         ! conversion from figures to integer
      INTEGER NVALUES        ! number of met elements (to go in index)

      INTEGER DATIME(5)      ! data time for AIRSTO
      INTEGER NOW(8)         ! time from DATIM call
      INTEGER TOR(5)         ! time of receipt (year, month,...)
      INTEGER DESCR(30)      ! descriptor array with room for expansion

      INTEGER YEAR
      INTEGER MONTH
      INTEGER DAY
      INTEGER HOUR
      INTEGER MIN
      INTEGER Q              ! quadrant
      INTEGER LAT            ! latitude in hundredths
      INTEGER LONG           ! longitude in hundredths

      INTEGER TPERIOD        ! averaging period for temperature
      INTEGER SPERIOD        ! averaging period for salinity
      INTEGER CPERIOD        ! averaging period for current
      INTEGER TW             ! water temperature in tenths Celsius
      INTEGER SAL            ! salinity in hundredths of parts per thou
      INTEGER DD             ! current direction in tens of degrees
      INTEGER CC             ! current speed in tenths, as reported
      INTEGER KNOTS          ! 4-group indicator for current speed unit

      INTEGER IDES
      INTEGER NDESCR         ! descriptor count for ENBUFR
      INTEGER NOBS           ! one ob per BUFR message
      INTEGER LEN            ! length of BUFR message

      REAL VALUES(17)        ! values to be encoded

      CHARACTER CORN*2       ! report correction number
      CHARACTER BULL*(*)     ! bulletin
      CHARACTER CCCC*4       ! collecting centre
      CHARACTER YYGGGG*6     ! date/time of bulletin
      CHARACTER TTAAII*6     ! bulletin header
      CHARACTER CALL_SIGN*9  ! call sign
      CHARACTER IDENT*9      ! copy of call sign for ENBUFR to translate
      CHARACTER ENTRY*23     ! index entry for AIRSTO & storage
      CHARACTER MESSAGE*100  ! message to be stored
      CHARACTER HEAD*80      ! Revision information                  !2

      LOGICAL ERROR          ! set if error in date, time or lat/long
      LOGICAL FIRST          ! .TRUE. if first call to TRKBUL        !2

      DATA FIRST /.TRUE./                                            !2

      IF (FIRST) THEN                                                !2
        HEAD = '$Workfile: trkbul.f$ ' //
     &         '$Revision: 2$ $Date: 04/11/2008 14:54:38$'
        FIRST = .FALSE.                                              !2
      END IF                                                         !2

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
      ENDDO

! Find start of report ('NNXX', followed by date group) & end of report
! (delimited by either next 'NNXX' or end of bulletin).
! (The -3 & -2 in the LAST= statements are meant to point to the last
! character of the call sign rather than an '=' or space after it.)

      J=1
      DO WHILE (J.LT.L)
        JX=INDEX(BULL(J:),'NNXX')
        IF (JX.EQ.0) RETURN
        J=J+JX-1
!                                      One NNXX found; look for another
        LAST=INDEX(BULL(J+5:),'NNXX')
        IF (LAST.GT.0) THEN
          LAST=J+5+LAST-3
        ELSE                         ! If no more NNXX's, bulletin end
          LAST=L-2
        ENDIF

! Go back to start of last group, which must be call sign.
! Make sure call sign doesn't end with equal sign.

        LCS=0
        DO WHILE (BULL(LAST:LAST).NE.' ')
          LCS=LCS+1
          LAST=LAST-1
        ENDDO

        CALL_SIGN=BULL(LAST+1:LAST+LCS)
        IF (INDEX(CALL_SIGN,'=').GT.0) THEN
          CALL_SIGN=CALL_SIGN(:INDEX(CALL_SIGN,'=')-1)
        ENDIF

! Move past NNXX.  Date group follows. (Assumed to be within last 9 yrs)

        J=J+5
        DAY=IVALUE(BULL(J:J+1))
        MONTH=IVALUE(BULL(J+2:J+3))
        YEAR=IVALUE(BULL(J+4:J+4)) + 10*(TOR(1)/10)
        IF (YEAR.GT.TOR(1)) YEAR=YEAR-10

        ERROR=.FALSE.
        IF (MONTH.LT.0 .OR. DAY.LT.0 .OR. YEAR.LT.0 .OR.
     &      MONTH.GT.12 .OR. DAY.GT.31) THEN
          PRINT *,'TRKBUL: bad DATE group ',BULL(J:J+4)
          ERROR=.TRUE.
        ENDIF
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

        DO WHILE (J.LT.LAST-20 .AND. .NOT.ERROR)
          NVALUES=0

! Initialise values from groups which may not be reported this time.

          DO I=10,17
            VALUES(I)=-9999999.
          ENDDO

          HOUR=IVALUE(BULL(J:J+1))
          MIN=IVALUE(BULL(J+2:J+3))
          IF (HOUR.LT.0 .OR. HOUR.GE.24 .OR. MIN.GE.60
     &       .OR. BULL(J+4:J+4).NE.'/') THEN
            PRINT *,'TRKBUL: bad time group ',BULL(J:J+4)
            ERROR=.TRUE.
          ENDIF
          J=J+6

          IF (MIN.LT.0) MIN=0
          VALUES(5)=HOUR
          VALUES(6)=MIN

! Latitude & longitude (in hundredths) are unsigned, with a quadrant
! before the latitude.

          Q=IVALUE(BULL(J:J))
          LAT=IVALUE(BULL(J+1:J+4))
          LONG=IVALUE(BULL(J+6:J+10))
          J=J+12

          IF (LAT.LT.0 .OR. LAT.GT.9000 .OR.
     &        LONG.LT.0 .OR. LONG.GT.18000 .OR.
     &        .NOT.(Q.EQ.1 .OR. Q.EQ.3 .OR. Q.EQ.5 .OR. Q.EQ.7)) THEN
            PRINT *,'TRKBUL: bad lat/long groups ',BULL(J:J+10)
            ERROR=.TRUE.
          ENDIF

          IF (Q.EQ.3 .OR. Q.EQ.5) LAT=-LAT
          IF (Q.EQ.5 .OR. Q.EQ.7) LONG=-LONG

          VALUES(7)=REAL(LAT)/100.
          VALUES(8)=REAL(LONG)/100.

! Lat/long may be followed by a 4-group (mandatory the first time)
! giving averaging periods (code figures) & units of current speed.
! (A code figure of 9 means missing. Otherwise multiply by 15 to give
! a representative number of minutes: see table 2604 in WMO Manual.)

          IF (BULL(J:J).EQ.'4') THEN
            TPERIOD=IVALUE(BULL(J+1:J+1))
            SPERIOD=IVALUE(BULL(J+2:J+2))
            CPERIOD=IVALUE(BULL(J+3:J+3))
            KNOTS=IVALUE(BULL(J+4:J+4))

            IF (TPERIOD.GE.0 .AND. TPERIOD.LT.9) TPERIOD=TPERIOD*15
            IF (SPERIOD.GE.0 .AND. SPERIOD.LT.9) SPERIOD=SPERIOD*15
            IF (CPERIOD.GE.0 .AND. CPERIOD.LT.9) CPERIOD=CPERIOD*15

            IF (TPERIOD.EQ.9) TPERIOD=-9999999
            IF (SPERIOD.EQ.9) SPERIOD=-9999999
            IF (CPERIOD.EQ.9) CPERIOD=-9999999

            J=J+6
          ENDIF

! A 6-group (optional) is water temperature (sign, then tenths Celsius)

          IF (BULL(J:J).EQ.'6') THEN
            TW=IVALUE(BULL(J+2:J+4))
            IF (BULL(J+1:J+1).EQ.'1') TW=-TW
            NVALUES=NVALUES+1
            J=J+6

            VALUES(10)=TPERIOD
            IF (TW.NE.-9999999) VALUES(11)=REAL(TW)/10.+273.1        !2
          ENDIF

! An 8-group (optional) is salinity (hundredths of parts per thousand)

          IF (BULL(J:J).EQ.'8') THEN
            SAL=IVALUE(BULL(J+1:J+4))
            NVALUES=NVALUES+1
            J=J+6

            VALUES(12)=SPERIOD
            IF (SAL.NE.-9999999) VALUES(13)=REAL(SAL)/100.           !2
          ENDIF

! A 9-group (optional) is current (speed in knots or m/s, in tenths).
! Only set speed if a 4-group has given the units (0: m/s, 1: knots).

          IF (BULL(J:J).EQ.'9') THEN
            DD=IVALUE(BULL(J+1:J+2))
            CC=IVALUE(BULL(J+3:J+4))
            NVALUES=NVALUES+1
            J=J+6

            VALUES(14)=CPERIOD
            IF (DD.NE.-9999999) VALUES(15)=DD*10.                    !2
            VALUES(16)=CPERIOD

            IF (KNOTS.GE.0 .AND. CC.NE.-9999999) THEN                !2
              VALUES(17)=REAL(CC)/10.
              IF (KNOTS.EQ.1) VALUES(17)=VALUES(17)*1852/3600
            ENDIF
          ENDIF

! Encode a BUFR message

          IF (.NOT.ERROR) THEN
            DESCR(1)=IDES(303192)
            NDESCR=1
            NOBS=1
            IDENT=CALL_SIGN
            CALL ENBUFR(DESCR,VALUES,NDESCR,17,NOBS,IDENT,
     &                  TOR,MESSAGE,.FALSE.,LEN)

! Set originating centre & data type in section 1 of BUFR message
! (assuming version 1; change displacement if total length at start)

            IF (ICCCC.GE.0) THEN
              MESSAGE(9:9)=CHAR(ICCCC/256)
              MESSAGE(10:10)=CHAR(MOD(ICCCC,256))
            ENDIF
            MESSAGE(13:13)=CHAR(31)

! Set bytes 3-16 of index entry (rest will be filled in by AIRSTO)

            ENTRY(3:11)=TTAAII(1:4)//CORN(2:2)//CCCC
            ENTRY(12:12)=CHAR(NVALUES)
            CALL INDLALO(ENTRY,VALUES(7),VALUES(8))

! Put time of data in integer array for AIRSTO & store message

            DO I=1,5
              DATIME(I)=VALUES(I+1)
            ENDDO
            CALL AIRSTO(DATIME,ENTRY,MESSAGE(:LEN),
     &                  IFT,27998,CALL_SIGN,TOR)
          ENDIF
        ENDDO     ! end of loop round lines of report (messages stored)
      ENDDO       ! end of loop round reports starting 'NNXX date'
      RETURN
      END
