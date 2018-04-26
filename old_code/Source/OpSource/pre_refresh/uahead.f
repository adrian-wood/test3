      SUBROUTINE UAHEAD(OB,PTR,REPLEN,TT,PART,ARRAY,BLOCK,STN,IDENT,
     &                  TYPE,ID,KNOTS,ERROR,B17BITS,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! ROUTINE       : UAHEAD
!
! PURPOSE       : Expand Upper Air identifier & date/time groups
!
! DATA TYPE(S)  : All upper air (TEMPS, PILOTS, DROPSONDES; LAND,
!               : SHIP & MOBIL)
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (function)
!               : STAPOS (abbreviated station list)
!               : OBHOUR
!
! ARGUMENTS     :  (1) INPUT OB, 5-FIG GROUPS SEPARATED BY SPACES   (I)
!               :  (2) POINTER (TO 1st standard level on return)   (I/O)
!               :  (3) length of ob                                 (I)
!               :  (4) TT (NEEDED IF MORE THAN ONE PART POSSIBLE)   (I)
!               :  (5) PART (A,B,C,D)                               (O)
!               :  (6) OUTPUT ARRAY                                 (O)
!               :  (7) BLOCK NUMBER                                 (O)
!               :  (8) STATION NUMBER                               (O)
!               :  (9) IDENTIFIER, IF NOT STATION                   (O)
!               : (10) type (TT, PP, UU, XX...)                     (O)
!               : (11) 100's FIGURE OF HIGHEST LEVEL WITH WIND (ID) (O)
!               : (12) FLAG SET ON IF WINDS IN KNOTS                (O)
!               : (13) FLAG SET ON IF ERROR IN DATE/TIME OR STATION (O)
!               : (14) INDEX ENTRY BTYE17 BITS 5-8                  (O)
!               : (15) Q/C array (1-BIT FLAGS: 0-OK 1-SUSPECT)      (O)
!
! REVISION INFO :
!
! $Workfile: uahead.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 04/11/2008 15:00:09$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         04/11/2008 15:00:09    Brian Barwell
!       Remove statements to bypass call to STAPOS for block 99 stations
!       (added at version 1.13).
!  1    Met_DB_Project 1.0         30/01/2006 20:25:38    Sheila Needham  
! $
! Revision 2.1  2001/11/06  10:07:22  10:07:22  usmdb (MetDB account c/o usjh)
! For bad reports, output OB(1:REPLEN) instead of OB(1:REPLEN+1)
! which is beyond the end of the string! S.Cox
!
! Revision 2.0  2001/07/03  10:44:32  10:44:32  usmdb (Generic MetDB account)
! Removed unused variables. Added copyright and modified
! header - S.Cox
!
! Revision 1.13  2000/08/09  15:06:57  15:06:57  usmdb (Generic MetDB account)
! 21/8/2000. Check added so that STAPOS is not called for Block
! 99 (CDL Stations). Stan Kellett
!
! Revision 1.12  2000/07/10  11:15:45  11:15:45  usmdb (Generic MDB account)
! 17 July 2000     C Long
! Start with DATIME missing in case no date/time group found.
! (as it was the code could use date/time from a previous ob!)
!
! Revision 1.11  99/07/12  16:14:18  16:14:18  usmdb (Generic MDB account)
! 19 July 1999     C Long
! Set Id (top level with wind) for dropsonde part A
!
! Revision 1.10  99/06/10  14:40:45  14:40:45  usmdb (Generic MDB account)
! Put REJ on end of identifier uscl 21 June 1999
!
! Revision 1.9  98/10/15  11:04:11  11:04:11  usmdb (Generic MDB account)
! Get dropsonde call sign from group after 61616
!
! Revision 1.8  98/03/13  11:07:46  11:07:46  usmdb (Generic MDB account)
! Set ERROR before returning when something's wrong!                  !M
!
! Revision 1.7  1998/02/19 13:40:53  usmdb
! Remove code at start specific to dropsonde, which is now handled
! (XXBB only) more like other data
!
! Revision 1.6  1998/01/27 10:13:21  usmdb
! Ignore reports with incorrect quadrant, latitude or longitude
! values. J.Norton                                                    !L
!
! Revision 1.5  1997/09/10 15:50:04  uspm
! Remove code to set height of a ship (UU) report. Only mobile
! reports (II) should have height reported in the message - S.Cox     !K
!
! Revision 1.4  1997/08/11 07:57:51  uspm
! Remove a redundant diagnostic write statement
!
! 04-08-97  Removal of unrequired checks to determine type and
! part of report. No change letter in body of module.                 !J
!
! Revision 1.3  1997/07/31 11:45:07  uspm
! First revision for COSMOS
!
! Revision 1.2  1997/07/25 13:58:36  uspm
! Latest version from COSMOS - allows storage even if no lat,long found
!
! Revision 1.1  1997/07/04 14:38:56  uspm
! Initial revision
!
! 04-07-97  !I  : Allow storage of reports even where the Lat/Long
!               : values are unknown. Code removed that checked for
!               : missing values and caused rejection.
!
! 05-02-97  !H  : Change check for ident
!
! 05-02-97  !G  : Change check from xx/paret/part to xxbb
!
! 27-01-97  !F  : General cleanup of code and correct indenting atc.
!
! 27-01-97  !E  : Ensure that variables are reset when doing a
!               : dropsonde report. Correction to calculation of
!               : IDENT/DATIME/Lat groups.
!
! 20-01-97  !D  : Set IDENT to missing. Correction to test for
!               : callsign/or datime group.
!
! 13-01-97  !C  : Correct handling of reported positions by putting
!               : lat and long in array rather tha lat/10 and long/10
!
! 16-12-96  !B  : Increase in Tolerance allowed for late reports to
!               : cover Fastex Dropsonde experiment.
!
! 16-12-96  !A  : Additional check for callsign and datime groups
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

      IMPLICIT NONE

!DECLARE CHARACTER
      CHARACTER OB*(*)               !raw report
      CHARACTER TT*2
      CHARACTER PART*1               !part a,b,c,d
      CHARACTER TYPE*2               !report type
      CHARACTER STNNO*5              !call to stapos
      CHARACTER IDENT*(*)            !id of station
      CHARACTER*1 B17BITS            !byte 17 bits in index
      CHARACTER*20 CHARR
      CHARACTER*80 HEAD              !revision information           !2

!DECLARE REAL
      REAL ARRAY(999)                !array of expened elements
      REAL LAT
      REAL LONG
      REAL LATENS
      REAL LOTENS
      REAL LATPOS                    !used in call to stapos
      REAL LONGPOS                   !used in call to stapos
      REAL HGPT                      !used in call to stapos
      REAL HGT                       !used in call to stapos
      REAL Q
      REAL QCBIT_ARRAY(999)          !array of qc bit flags
      REAL HEIGHT

!DECLARE INTEGER
      INTEGER GRPLEN
      INTEGER REPLEN
      INTEGER NCHAR
      INTEGER REQ_WMONO              !requested wmono to stapos
      INTEGER ISTAT                  !used in call to stapos
      INTEGER PTR                    !pointer within report
      INTEGER DATIME                 !date/time array from report
      INTEGER YMDH(4)                !date/time from obhour
      INTEGER DAY                    !day from report
      INTEGER HOUR                   !hour from report
      INTEGER BLOCK                  !wmo block number
      INTEGER STN                    !wmo station number
      INTEGER HTUNITS                !units of height
      INTEGER FT2METR                !feet to metres conversion
      INTEGER IPART                  !integer part of report
      INTEGER ITYPE                  !integer type of report
      INTEGER IT
      INTEGER IVALUE                 !function subprogram
      INTEGER ID
      INTEGER ILALO
      INTEGER MARSQ                  !marsden square box
      INTEGER MMM
      INTEGER MISSING                !indicates value missing in report
      INTEGER I                      !used in loops
      INTEGER MIN
      INTEGER ILAT                                                    !L
      INTEGER ILONG                                                   !L
      INTEGER ISIX                                                 !1.9

!DECLARE LOGICAL
      LOGICAL KNOTS                  !indicates wind speed in knots
      LOGICAL ERROR                  !indicates error in decode
      LOGICAL DEBUG                  !turns on/off diagnostics
      LOGICAL HEADSET                                              !2.1
      LOGICAL LREPFL

***********************************************************************
*
* FIND THE FOLLOWING COORDINATE GROUPS IN A REPORT HEADER:
*
*  MIMJ IDENT YYGGI, THEN EITHER STNNO (IF LAND STATION)
*  ~~~~ ~~~~~ ~~~~~              ~~~~~  OR 99LAT QLONG MMMUU HHHHI,
*                                          ~~~~~ ~~~~~ ~~~~~ ~~~~~
* WHERE IDENT CAN COME BEFORE, AFTER OR BETWEEN MIMJ & YYGGI;
* ONLY A MOBIL HAS A HEIGHT GROUP.
*
* SO PROCEED AS FOLLOWS:
*  1. FIND MJ, WHICH IS ONE OF AA,BB,CC,DD; WE KNOW WHICH TO EXPECT
*      FROM TT IN BULLETIN HEADING
*  2. CHECK MI: TT/UU/XX/II/PP OR PERHAPS QQ/EE (SHIP'S/MOBILE PILOT)
*  3. IF MI IS UU OR II, LOOK FOR IDENT BEFORE, AFTER OR BETWEEN
*      MIMJ & YYGGI; ASSUME IDENT CAN'T HAVE 5 FIGURES, SO 5-FIGURE
*      GROUP IS YYGGI
*  4. IF MI IS UU, II OR XX, GO ON FROM 99 TO LAT/LONG (& HEIGHT IF II)
*  5. CHECK LAT/LONG AGAINST MARSDEN SQUARE.
*
***********************************************************************

!save all variables
      SAVE

!data statements
      DATA HEADSET/.FALSE./                                         !2.1

!initialize variables
      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD = '$Workfile: uahead.f$ ' //
     &         '$Revision: 2$ $Date: 04/11/2008 15:00:09$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

      MISSING=-9999999           !set missing value
      DEBUG=.FALSE.              !diagnostics .true. = on
      ERROR=.FALSE.              !set error as false to start
      MIN=-9999999
      LAT=-9999999.
      LONG=-9999999.
      LOTENS=-9999999.
      LATENS=-9999999.
      LATPOS=-9999999.           !set lat from stapos to missing
      LONGPOS=-9999999.          !set long from stapos missing
      LAT=-9999999               !set lat from report as missing
      LONG=-9999999              !set long from report as missing
      FT2METR=0.3048             !FEET TO METER CONVERSION
      LREPFL=.FALSE.
      IDENT(:)=' '               !Set IDENT to missing incase no ident!d
      DATIME=-9999999            !in case no date/time group found !1.12

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! IPART (if set) will point to AA, BB, CC or DD; set type from the    !
! previous two letters.                                               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IPART=INDEX(OB,PART//PART//' ')
      IF (IPART.EQ.0) THEN                                           !m
        ERROR=.TRUE.                                                 !m
        GOTO 999                                                     !m
      ENDIF                                                          !m
      PART=OB(PTR+IPART:PTR+IPART)
      TYPE=OB(PTR+IPART-3:PTR+IPART-2)

      IF (TYPE.NE.'TT' .AND. TYPE.NE.'UU' .AND. TYPE.NE.'XX' .AND.
     &    TYPE.NE.'II' .AND. TYPE.NE.'PP' .AND.
     &    TYPE.NE.'QQ' .AND. TYPE.NE.'EE') THEN
        WRITE(6,*)'UAHEAD: type unrecognised',TYPE
        ERROR=.TRUE.
        GOTO 999
      ENDIF

      ITYPE=IPART-2
      IT=PTR+ITYPE-1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! for temps & pilots from land stations the header consists of mimj    !
! followed by only the date/time group and station number; otherwise   !
! find the latitude group to delimit the area where the call sign may be
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (TYPE.EQ.'TT' .OR. TYPE.EQ.'PP') THEN
        DATIME=IVALUE(OB(IT+5:IT+8))         ! DATE/TIME GROUP
        ID=IVALUE(OB(IT+9:IT+9))             ! DATE/TIME GROUP
        BLOCK=IVALUE(OB(IT+11:IT+12))        ! WMO BLOCK
        STN=IVALUE(OB(IT+13:IT+15))          ! STATION NUMBER
        IF ((BLOCK .EQ. MISSING) .OR. (STN .EQ. MISSING)) THEN
          write(6,*)'UAHEAD Bad station details rejected report',block,
     &    stn
          ERROR=.TRUE.                                               !m
          GOTO 999
        ENDIF
        PTR=PTR+ITYPE+16                     ! PAST LAND HEADER
*
        STNNO=OB(IT+11:IT+15) ! STATION NUMBER
        IDENT=STNNO                          ! IDENT FOR LAND STATION
        ISTAT=0                            !set istat to 0 for stapos
        REQ_WMONO=(BLOCK*1000)+STN         !append block and station

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!There are two calls to STAPOS. The first looks up the requested      !
!station in the upperair list. If the requested station is not found  !
!then a second call to STAPOS looks up the 'x' list. This list contains
!stations that have not been identified as being upperair/surface.    !
!If after the second call the requested station details are still not !
!available then a message is output to warn of the situation.         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        CALL STAPOS(REQ_WMONO,'U',LATPOS,LONGPOS,HGPT,HGT,ISTAT)

        IF ((LATPOS .EQ. -9999999) .AND. (LONGPOS .EQ. -9999999)) THEN
          ISTAT=0
          CALL STAPOS(REQ_WMONO,'X',LATPOS,LONGPOS,HGPT,HGT,ISTAT)
        ENDIF

        IF ((LATPOS .EQ. -9999999) .AND. (LONGPOS .EQ. -9999999)) THEN
          ISTAT=0
          CALL STAPOS(REQ_WMONO,'S',LATPOS,LONGPOS,HGPT,HGT,ISTAT)
        ENDIF

        IF ((LATPOS .EQ. -9999999) .AND. (LONGPOS .EQ. -9999999)) THEN
          WRITE(6,*)'UpperAir (UAHEAD)-->Station not found in STAPOS',
     &    REQ_WMONO
        ENDIF

! If not TT or PP (i.e. if ship, drop or mobile)...               !1.11

      ELSE
        ILALO=INDEX(OB(PTR:),' 99')          ! FIND LATITUDE GROUP
        IF (ILALO.EQ.0 .OR. ILALO-ITYPE.LT.10) THEN                   !E
          ERROR=.TRUE.                                                !m
          GOTO 999
        ENDIF

! If drop has group before XX, get ident from it and day, hour    !1.11
! & top level with wind from group after XX..                     !1.11

        IF (TT.EQ.'UZ' .AND. INDEX(OB,'XX').GT.4) THEN            !1.11
          IF (OB(PTR:PTR) .EQ. ' ') THEN
            IDENT=OB(PTR+1:PTR+5)                                     !F
          ELSE
            IDENT=OB(PTR:PTR+4)                                       !F
          ENDIF
          PTR=IPART+3
          DATIME=IVALUE(OB(PTR:PTR+3))
          ID=IVALUE(OB(PTR+4:PTR+4))

! Otherwise look at groups after AA, BB etc: may be ident then    !1.11
! day/hour, may be the other way round...                         !1.11

        ELSE
          PTR=IPART+3
          GRPLEN=0
          CALL AIRLOC(REPLEN,OB,PTR,GRPLEN,LREPFL)
          CALL AIRGRP(OB(PTR-GRPLEN-1:PTR-1),GRPLEN,NCHAR,CHARR)
          IF ((NCHAR.GE. 1) .AND. (OB(PTR-2:PTR-2) .NE. '/')) THEN
            IDENT=OB(PTR-GRPLEN-1:PTR-1)
            IF (OB(PTR:PTR+1) .NE. '99') THEN
              DATIME=IVALUE(OB(PTR:PTR+3))
              ID=IVALUE(OB(PTR+4:PTR+4))
            ENDIF
          ELSE
            DATIME=IVALUE(OB(PTR-GRPLEN-1:PTR-3))
            ID=IVALUE(OB(PTR-2:PTR-2))                            !1.11
!Now find the IDENT which comes after the DATIME group
            CALL AIRLOC(REPLEN,OB,PTR,GRPLEN,LREPFL)                  !A
            CALL AIRGRP (OB(PTR-GRPLEN-1:PTR-1),GRPLEN,NCHAR,CHARR)   !A
            IF (NCHAR .GE. 1) THEN                                    !A
              IDENT=OB(PTR-GRPLEN-1:PTR-1)                            !A
            ENDIF                                                     !A
          ENDIF                                                      !A
        ENDIF

! If it's a dropsonde and we still haven't found an identifier,    !1.9
! look for a 61616 section and if found use the first group in it. !1.9
! (This copes with Sept 1998 data)                                 !1.9

        IF (TT.EQ.'UZ' .AND. IDENT.EQ.' ') THEN                    !1.9
          ISIX=INDEX(OB,'61616')                                   !1.9
          IF (ISIX.GT.0) THEN                                      !1.9
            PTR=ISIX+6                                             !1.9
            CALL AIRLOC(REPLEN,OB,PTR,GRPLEN,LREPFL)               !1.9
            CALL AIRGRP (OB(PTR-GRPLEN-1:PTR-1),GRPLEN,NCHAR,CHARR)!1.9
            IF (NCHAR.GE.1) IDENT=OB(PTR-GRPLEN-1:PTR-1)           !1.9
          ENDIF                                                    !1.9
        ENDIF                                                      !1.9

! If the plain language 62626 section contains the keyword EYE,   !1.10
! EYEWALL or RAINBAND, set a flag in the identifier (which will   !1.10
! be expanded to REJ in UAEDIT).                                  !1.10

        IF (TT.EQ.'UZ') THEN                                      !1.10
          ISIX=INDEX(OB,'62626')                                  !1.10
          IF (ISIX.GT.0) THEN                                     !1.10
            IF (INDEX(OB(ISIX:),'EYE').GT.0 .OR.                  !1.10
     &          INDEX(OB(ISIX:),'RAINBAND').GT.0) THEN            !1.10
              IDENT(7:7)=CHAR(255)                                !1.10
            ENDIF                                                 !1.10
          ENDIF                                                   !1.10
        ENDIF                                                     !1.10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! now find lat/long if mobile platform (& height if mobil, type='xx'). !
! the lat & long are followed by a check group consisting of a marsden !
! square (a ten-degree square to check the tens figures) & units figures
! to repeat the last-but-one figures of the lat & long groups.         !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        PTR=ILALO+1
        ILAT=IVALUE(OB(PTR+2:PTR+4))           ! LATITUDE            !L
        Q=IVALUE(OB(PTR+6:PTR+6))              ! QUADRANT
        ILONG=IVALUE(OB(PTR+7:PTR+10))         ! LONGITUDE           !L
        MARSQ=IVALUE(OB(PTR+12:PTR+14))        ! MARSDEN SQUARE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! keep tens figures of lat & long for marsden square check            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (Q.EQ.1.OR.Q.EQ.3.OR.Q.EQ.5.OR.Q.EQ.7)THEN                !L 00810000
                                                                     !L 00800000
          IF (ILAT.LT.0 .OR. ILAT.GT.900)THEN                        !L 00008100
            PRINT*,' UAHEAD BAD LATITUDE: ',OB(1:REPLEN)            !2.101090000
            ERROR=.TRUE.                                             !L 00800000
            GO TO 999                                                !L 00800000
          ELSE                                                       !L 00800000
            IF (Q.EQ.3 .OR. Q.EQ.5) ILAT=-ILAT   ! SOUTH NEGATIVE    !L 00008200
            LAT=REAL(ILAT)                                           !L 00800086
            LATENS=ILAT/100                                          !L 00800085
          ENDIF                                                      !L 00800000
                                                                     !L 00800000
          IF(ILONG.LT.0 .OR. ILONG.GT.1800)THEN                      !L 00800000
            PRINT*,' UAHEAD BAD LONGITUDE: ',OB(1:REPLEN)           !2.101090000
            ERROR=.TRUE.                                             !L 00800000
            GO TO 999                                                !L 00800000
          ELSE                                                       !L 00800000
            IF (Q.EQ.5 .OR. Q.EQ.7) ILONG=-ILONG ! WEST NEGATIVE      L 00800083
*                                                                    !L 00008400
            LONG=REAL(ILONG)                                         !L 00800086
            LOTENS=ILONG/100                                         !L 00800086
          ENDIF                                                      !L 00800000
        ELSE                                                         !L 00800000
          PRINT*,' UAHEAD BAD QUADRANT: ',OB(1:REPLEN)              !2.1090000
          ERROR=.TRUE.
          GO TO 999
        ENDIF                                                        !L 00800000

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! work out the marsden square from the lat/long & compare the two values
! (test the quadrant for n/s & e/w, because quadrant on equator & at 180
! e/w can be either, and the marsden square depends on which is chosen;!
! the convention for which square a multiple of 10 degrees falls in    !
! amounts to saying that n*10.0 & the almost equal lat or long ending in
! 9.9... are in different squares, i.e. it all depends on the tens fig;!
! so beware of using (360-long)/10, which puts both in the same square!)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (MARSQ.NE.MISSING) THEN
          IF (Q.EQ.3 .OR. Q.EQ.5) THEN       ! IF SOUTH, START AT 300.
            MMM=300+LATENS*36-LOTENS
          ELSE IF (LAT.LT.80) THEN           ! IF NORTH BUT NOT POLAR,
            MMM=1+LATENS*36-LOTENS           !  START AT 1.
          ELSE                               ! IF POLAR, CAN'T USE RANGE
            MMM=901-LOTENS                   !  289-324, SO START AT 901
          ENDIF
          IF (Q.LE.3) MMM=MMM+36 ! IF WEST, ADD 36
        ENDIF
*
* THE PLATFORM HEIGHT (FOR A MOBIL) IS IN FEET OR METRES, AS INDICATED
* BY THE LAST FIGURE IN THE GROUP, WHICH ALSO SUGGESTS THE ACCURACY.
*
        IF (TYPE.EQ.'II') THEN
          HEIGHT=IVALUE(OB(PTR+18:PTR+21))    ! IN FEET OR METRES
          HTUNITS=IVALUE(OB(ILALO+22:ILALO+22))
          IF (HEIGHT.NE.MISSING .AND. HTUNITS.GE.5) THEN
            HEIGHT=HEIGHT*FT2METR            ! CONVERT FEET TO METRES
            HTUNITS=HTUNITS-4                ! ACCURACY IN RANGE 1 TO 4
          ENDIF
          PTR=PTR+6                          ! PAST FURTHER HEADER GROUP
        ENDIF
        PTR=PTR+18
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CONVERT THE DATE/TIME, SETTING THE KNOTS OR M/S FLAG               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (DATIME.EQ.MISSING) then
        error =.true.
      endif

      IF (.NOT. ERROR) THEN
        DAY=DATIME/100
        HOUR=DATIME-DAY*100
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Day group is also used to indicate if wind speed is reported in    !
! knots or m/s. To indicate that the wind speed is in knots 50 is    !
! added to the day.                                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (DAY.GE.51) THEN
          KNOTS=.TRUE.                          !set knots flags
          DAY=DAY-50                            !correct day
        ELSEIF ((DAY .GE. 1) .AND. (DAY .LE. 31)) THEN
          KNOTS=.FALSE.                         !windspeed not in knots
        ELSE                                    !day already set correct
          ERROR=.TRUE.
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! if day & time ok, keep hour at end of ident field, so that different!
! ascents from the same station will get different index entries.     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (DAY.GT.31 .OR. HOUR.GE.24) THEN
          ERROR= .TRUE.
        ENDIF

        IF (TT .EQ. 'UZ') THEN
          IDENT(9:9)=CHAR(0)
          IDENT(8:8)=CHAR(HOUR)
        ELSE
          WRITE(IDENT(8:9),'(I2)')HOUR
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! finally set the elements found in the output array for encoding     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (TYPE.EQ.'TT' .OR. TYPE.EQ.'PP') THEN   ! land station

          ARRAY(2)=BLOCK
          IF (BLOCK .GT. 0) THEN
            QCBIT_ARRAY(2)=0.
          ELSE
            QCBIT_ARRAY(2)=1.
          ENDIF

          ARRAY(3)=STN
          IF (STN .GT. 0) THEN
            QCBIT_ARRAY(3)=0.
          ELSE
            QCBIT_ARRAY(3)=1.
          ENDIF

          ARRAY(4)=MISSING                     ! NO IDENTIFIER
          QCBIT_ARRAY(4)=0.

          ARRAY(5)=LATPOS                      ! LATITUDE FROM FX LND
          IF (LATPOS .GT. -9999999) THEN
            QCBIT_ARRAY(5)=0.
          ELSE
            QCBIT_ARRAY(5)=1.
          ENDIF

          ARRAY(6)=LONGPOS                     ! LONGITUDE FROM FX LND
          IF (LONGPOS .GT. -9999999) THEN
            QCBIT_ARRAY(6)=0.
          ELSE
            QCBIT_ARRAY(6)=1.
          ENDIF

          ARRAY(7)=HGT                    !station height
          ARRAY(8)=HGPT                   !pressure sensor height
          QCBIT_ARRAY(7)=0.
          QCBIT_ARRAY(8)=0.

        ELSE                              ! mobile, lat/long reported
          ARRAY(2)=MISSING
          QCBIT_ARRAY(2)=0.
          ARRAY(3)=MISSING
          QCBIT_ARRAY(3)=0.
          ARRAY(4)=1                         ! IDENT IN CHARACTER STRING
          QCBIT_ARRAY(4)=0.

          ARRAY(5)=LAT/10.                                            !C
          IF (LAT .GT. -9999999) THEN                                 !C
            QCBIT_ARRAY(5)=0.
          ELSE
            QCBIT_ARRAY(5)=1.
          ENDIF

          ARRAY(6)=LONG/10.                                           !C
          IF (LONG .GT. -9999999) THEN                                !C
            QCBIT_ARRAY(6)=0.
          ELSE
            QCBIT_ARRAY(6)=1.
          ENDIF

          ARRAY(7)=MISSING                   !- SHIP HEIGHT UNKNOWN
          ARRAY(8)=MISSING                   !- SHIP HEIGHT UNKNOWN
          QCBIT_ARRAY(6)=0.
          QCBIT_ARRAY(7)=0.

          IF (TYPE.EQ.'II') THEN                                      !K
            ARRAY(7)=HEIGHT                  !- HEIGHT ONLY IF MOBILE
            ARRAY(8)=MISSING
          ENDIF
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! check the report day & hour against the bulletin date/time,
! (longer tolerance for dropsondes)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        YMDH(3)=ARRAY(11)                      ! BULLETIN DAY (INTEGER)
        YMDH(4)=ARRAY(12)                      ! BULLETIN HOUR (INTEGER)
        IF (TT .EQ. 'UZ') THEN
          CALL OBHOUR(YMDH,DAY,HOUR,' ',800,ISTAT)
        ELSE
          CALL OBHOUR(YMDH,DAY,HOUR,' ',48,ISTAT)
        ENDIF
        IF (ISTAT .GT. 0) THEN
          ERROR=.TRUE.
        ENDIF

        IF (.NOT. ERROR) THEN
          IF ((YMDH(3).NE.MISSING) .AND.
     &    (YMDH(2) .NE. MISSING)) THEN
            DO I=1,4                          ! PUT YEAR/MONTH (NOW SET)
              ARRAY(8+I)=YMDH(I)              ! & DAY ETC IN REAL ARRAY
              QCBIT_ARRAY(8+I)=0.
            ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!In preference we use the report day/hour in the data being passed to  !
!TAFREP. Therefore arry(11), array(12) are overwritten with the reports!
!day and hour RATHER THAN the bulletin day/time.                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ARRAY(11)=DAY                     !set to report day
            ARRAY(12)=HOUR                    !set to report hour
            ARRAY(13)=MISSING                 ! MINUTES MAY BE SET LATER
            QCBIT_ARRAY(13)=0.
            ERROR=.FALSE.

          ELSE
            DO I=1,4
              QCBIT_ARRAY(8+I)=1
            ENDDO
            ERROR=.TRUE.
          ENDIF
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!This section sets bits in BYTE 17 of the index entry                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (TYPE .EQ. 'TT') THEN
          B17BITS=CHAR(4)
        ELSEIF (TYPE .EQ. 'II') THEN
          B17BITS=CHAR(6)
        ELSEIF (TYPE .EQ. 'UU') THEN
          B17BITS=CHAR(7)
        ELSEIF (TYPE .EQ. 'PP') THEN
          B17BITS=CHAR(0)
        ELSEIF (TYPE .EQ. 'EE') THEN
          B17BITS=CHAR(2)
        ELSEIF (TYPE .EQ. 'QQ') THEN
          B17BITS=CHAR(3)
        ELSEIF (TT .EQ. 'UZ') THEN
          B17BITS=CHAR(10)
        ENDIF
      ENDIF

      IF ((ARRAY(9) .LT. 1980) .OR. (ARRAY(9) .GT. 3000)) THEN
        ERROR=.TRUE.
      ENDIF
999   CONTINUE
      RETURN
      END
