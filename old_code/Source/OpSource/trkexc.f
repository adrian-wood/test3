      SUBROUTINE TRKEXC(REPORT,TTAAII,CCCC,ICCCC,YYGGGG,
     &                  CORNUM,IFT)                                !2.0

      IMPLICIT NONE

!----------------------------------------------------------------------
!
! PROGRAM       : TRKEXC
!
! PURPOSE       : DECODE SECTION 2 OF TRACKOB REPORT (DATA SECTION)
!
! DESCRIPTION   : LOOPS ROUND REPORT LOOKING FOR INDICATORS AT THE
!                 START OF EACH GROUP. DECODES THE REPORT AND STORES
!                 THE ELEMENTS IN THE ELEMS_ARRAY.
!
! DATA TYPE(S)  : TRACKOB
!
! CALLED BY     : TRKBUL
!
! CALLS         : TRKSPLIT,TRKSGN
!
! PARAMETERS    : (1)Report to be decoded -Input
!                 (2)Bulletin Header info -Input
!                 (3)Collecting centre (character) -Input
!                 (4)Collecting centre (Integer) -Input
!                 (5)Date/time from bulletin -Input
!                 (6)Correction Number -Input
!                 (7)Storage unit number -Input
!
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:29$
! $Source: /home/us0400/mdb/op/lib/source/RCS/trkexc.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:29    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:26  usmdb
! Removed unused variables.Initialised variables DDFF_AVG_REAL,
! SALIN_AVG_REAL and TEMP_AVG_REAL to 0 to satisfy compiler,
! however their values will always be 0 which I imagine is not
! what is desired here. Delete unused dummy argument REPEND.
! Removed arguments PTR and ISTAT from call to TRKSTO as not
! used in TRKSTO. Added copyright and modified header - S.Cox
!
! Revision 1.2  99/07/12  16:28:35  16:28:35  usmdb (Generic MetDB account)
! 19 July 1999     C Long
! Delete uninformative messages
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

!declare character
      CHARACTER*400 REPORT_ARRAY(200)
      CHARACTER REPORT*(*)                  !Report being decoded
      CHARACTER YYGGGG*(*)                  !Date/Time group from report
      CHARACTER CALL_SIGN*(9)               !Callsign of current report
      CHARACTER CCCC*(*)                    !Collecting centre no.
      CHARACTER TTAAII*(*)
      CHARACTER TYPE*2
      CHARACTER CORNUM*(*)
      CHARACTER*132      HEAD               ! Revision information

!declare real
      REAL SALINITY_HUND
      REAL WATER_TEMP_KELVIN
      REAL WATER_TEMP_TENS
      REAL SEA_SPEED_REAL
      REAL TEMP_AVG_REAL                    !Temp average period
      REAL SALIN_AVG_REAL                   !Salinity average period
      REAL DDFF_AVG_REAL
      REAL ELEMS_ARRAY(17)                  !Decoded elements array
      REAL LAT_TENS                         !Latitude
      REAL LONG_TENS                        !Longitude

!declare integer
      INTEGER IVALUE                           !Real value function
      INTEGER DDFF_AVG
      INTEGER WATER_TEMP
      INTEGER SALINITY
      INTEGER SEA_DIRECTION
      INTEGER SEA_SPEED
      INTEGER LOOP_COUNT
      INTEGER ELEM_BASE_DISP
      INTEGER PTR                           !Pointer within report
      INTEGER ISTAT                         !Error check flag
      INTEGER TOR(5)                        !TOR array
      INTEGER YMDH(4)                       !Time array to OBHOUR
      INTEGER REP_DAY                       !Day from report
      INTEGER REP_HOUR                      !Hour from report
      INTEGER REP_MONTH                     !Month from report
      INTEGER REP_MINS                      !Minutes from report
      INTEGER IERR                          !Error check to OBHOUR
      INTEGER QUAD                          !Quadrant of Lat/Long
      INTEGER LAT                           !Latitude as read
      INTEGER LONG                          !Longitude as read
      INTEGER TEMP_AVG                      !Temp average time period
      INTEGER SALIN_AVG                     !Salinity average time
      INTEGER CURRENT_AVG                   !sea current time average
      INTEGER DATA_UNITS                    !indicates knots or ms-1
      INTEGER MISSING                       !Missing data value
      INTEGER BUL_DAY                       !Day from Bulletin Header
      INTEGER BUL_HOUR                      !Hour from Bulletin Header
      INTEGER CALL_LOOP                     !Used in Callsign section
      INTEGER SET_TIME
      INTEGER CLEAR_ARRAY
      INTEGER ICCCC                         !Integer collecting no.
      INTEGER IFT
      INTEGER I                             !Used as loop counter   !2.0
      INTEGER NOW(8)                        !System time array
      INTEGER NUM_LEV                       !Array subscript
      INTEGER NUM_REPS                      !Total TRKOBS to process
      INTEGER EQUALS
      INTEGER CallSignReturnCode
      INTEGER SingleReportLength

!declare logical
      LOGICAL NO_MORE_GROUPS
      LOGICAL GOT_CALL_SIGN                 !Indicate call_sign decoded
      LOGICAL KNOTS                         !Indicates units
      LOGICAL CALL_SIGN_ERROR               !Indicates error in decode
      LOGICAL STILL_EXPANDING               !Indicates to calling
                                            !routine that report is
      SAVE                                  !still being decoded.

!initialize variables

      CallSignReturnCode=9
      SingleReportLength=0
      NO_MORE_GROUPS=.FALSE.
      LOOP_COUNT=1
      ELEM_BASE_DISP=3
      KNOTS=.FALSE.
      MISSING=-9999999
      GOT_CALL_SIGN=.FALSE.
      CALL_SIGN_ERROR=.FALSE.
      STILL_EXPANDING=.TRUE.
      DATA_UNITS=MISSING
      TEMP_AVG=MISSING
      CURRENT_AVG=MISSING
      DO I=1,17
        ELEMS_ARRAY(I)=-9999999.
      ENDDO
      DO I=1,200
        REPORT_ARRAY(I)(:)=' '
      ENDDO
      TYPE='NN'
      NUM_LEV=0
      REP_DAY=MISSING
      REP_MONTH=MISSING
      CALL_LOOP=0
      EQUALS=0

!-----------------------------------------------------------------------
! Note: the 3 variables below were not initialised prior to 2.0.
! Initialise to 0 to satisfy compiler. However their values will
! always be 0 which I imagine is not what is desired here           !2.0
!-----------------------------------------------------------------------

      DDFF_AVG_REAL=0                                               !2.0
      SALIN_AVG_REAL=0                                              !2.0
      TEMP_AVG_REAL=0                                               !2.0

      HEAD='
     &$Source: 
     &'//'$ $Date: 30/01/2006 20:25:29$'

!---------------------------------------------------------------------
!Sometimes the REPORT passed from TRKBUL will consist of several
!smaller TRACKOBs strung together. These need to be further split into
!individual reports for processing. Call TRKSPLIT to do this.
!---------------------------------------------------------------------

      CALL TRKSPLIT(REPORT,NUM_REPS,REPORT_ARRAY)


!----------------------------------------------------------------------
!Get TOR from system time
!----------------------------------------------------------------------

      CALL DATIM(NOW)
      DO I=0,4,1
        TOR(1+I)=NOW(8-I)
      ENDDO

!----------------------------------------------------------------------
!Set the total reports to decode and set character report back to
!blank spaces
!----------------------------------------------------------------------

      DO WHILE (NUM_REPS .GT. 0)
        STILL_EXPANDING=.TRUE.
        REPORT=REPORT_ARRAY(NUM_REPS)
        SingleReportLength=INDEX(REPORT,'  ')
        PTR=INDEX(REPORT,'NNXX')+5

!----------------------------------------------------------------------
!Get the Day and Month from report.
!----------------------------------------------------------------------

        REP_DAY=IVALUE(REPORT(PTR:PTR+1))
        REP_MONTH=IVALUE(REPORT(PTR+2:PTR+3))
        PTR=PTR+6                               !Pointer at GGgg/

!----------------------------------------------------------------------
!Get Day and Hour from the Bulletin heading
!----------------------------------------------------------------------

        BUL_DAY=IVALUE(YYGGGG(1:2))
        BUL_HOUR=IVALUE(YYGGGG(3:4))
        DO WHILE (STILL_EXPANDING .AND. (ISTAT .EQ. 0))

!----------------------------------------------------------------------
!Move pointer to start of next group,get hours & mins and move pointer
!----------------------------------------------------------------------

          IF (REPORT(PTR+4:PTR+4) .EQ. '/') THEN
            REP_HOUR=IVALUE(REPORT(PTR:PTR+1))
            REP_MINS=IVALUE(REPORT(PTR+2:PTR+3))
            PTR=PTR+6                           !Pointer at Quadrant

!----------------------------------------------------------------------
!Set-up YMDH array for call to OBHOUR to validate report time
!----------------------------------------------------------------------

            YMDH(3)=BUL_DAY
            YMDH(4)=BUL_HOUR
            IERR=0

!----------------------------------------------------------------------
!Call OBHOUR to calculate the correct Year etc. This routine will
!validate the repoert day/time information. We can also adjust the
!tolerance for allowing late reports. This is set at 6 hrs.
!----------------------------------------------------------------------

            CALL OBHOUR(YMDH,REP_DAY,REP_HOUR,' ',500,IERR)
            IF (IERR .EQ. 0) THEN
              DO SET_TIME=1,4
                ELEMS_ARRAY(SET_TIME+1)=YMDH(SET_TIME)
              ENDDO

!----------------------------------------------------------------------
!Reset Day and Hour back to Report Day & Report Hour also set minutes.
!----------------------------------------------------------------------

              ELEMS_ARRAY(4)=REP_DAY
              ELEMS_ARRAY(5)=REP_HOUR
              ELEMS_ARRAY(6)=REP_MINS
            ELSE
              PRINT *,'TRKEXC: date/time error ',REPORT(1:PTR)     !1.1
              ISTAT=8
            ENDIF                           !End of IERR check
          ELSE
            IF (ISTAT .EQ. 0) THEN

!----------------------------------------------------------------------
!Day/Time OKay so now get Quadrant,Latitude and Longitude
!----------------------------------------------------------------------

              QUAD=IVALUE(REPORT(PTR:PTR))
              LAT=IVALUE(REPORT(PTR+1:PTR+4))
              LONG=IVALUE(REPORT(PTR+6:PTR+10))

!----------------------------------------------------------------------
!Check the value of Quad and convert Lat/Long as appropriate
!----------------------------------------------------------------------

              IF (QUAD .EQ. 3) THEN
                LAT=-LAT
                LAT_TENS=REAL(LAT)*(1./100.)
                ELEMS_ARRAY(7)=LAT_TENS
                LONG_TENS=REAL(LONG)*(1./100.)
                ELEMS_ARRAY(8)=LONG_TENS
              ELSEIF (QUAD .EQ. 5) THEN
                LAT=-LAT
                LONG=-LONG
                LAT_TENS=REAL(LAT)*(1./100.)
                LONG_TENS=REAL(LONG)*(1./100.)
                ELEMS_ARRAY(7)=LAT_TENS
                ELEMS_ARRAY(8)=LONG_TENS
              ELSEIF (QUAD .EQ. 7) THEN
                LONG=-LONG
                LONG_TENS=REAL(LONG)*(1./100.)
                ELEMS_ARRAY(8)=LONG_TENS
                LAT_TENS=REAL(LAT)*(1./100.)
                ELEMS_ARRAY(7)=LAT_TENS
              ELSEIF (QUAD .EQ .1) THEN
                LAT_TENS=REAL(LAT)*(1./100.)
                LONG_TENS=REAL(LONG)*(1./100.)
                ELEMS_ARRAY(7)=LAT_TENS
                ELEMS_ARRAY(8)=LONG_TENS
              ELSE
                LAT=MISSING
                LONG=MISSING
              ENDIF
              PTR=PTR+12

!----------------------------------------------------------------------
!Now look for callsign at the end of the report
!----------------------------------------------------------------------

              IF (CallSignReturnCode .EQ. 9) THEN
                CALL TRKSGN(REPORT,SingleReportLength,CALL_SIGN,
     &          CallSignReturnCode)
              ENDIF

!----------------------------------------------------------------------
!In the Trackob reports temp groups start '60' or '61', salinity start
!with an '8' and sea current '9'. The group that starts with a '4'
!contains the averaging period information for the following groups in
!the report.
!----------------------------------------------------------------------

              IF (REPORT(PTR:PTR) .EQ. '4') THEN
                TEMP_AVG=IVALUE(REPORT(PTR+1:PTR+1))
                SALIN_AVG=IVALUE(REPORT(PTR+2:PTR+2))
                DDFF_AVG=IVALUE(REPORT(PTR+3:PTR+3))

                IF (TEMP_AVG .EQ. 9) THEN
                  TEMP_AVG=MISSING
                ENDIF
                IF (SALIN_AVG .EQ. 9) THEN
                  SALIN_AVG=MISSING
                ENDIF
                IF (DDFF_AVG .EQ. 9) THEN
                  DDFF_AVG=MISSING
                ENDIF

                IF (REPORT(PTR+4:PTR+4) .EQ. '0') THEN
                  KNOTS=.TRUE.
                ENDIF
                PTR=PTR+6
              ENDIF

              IF ((REPORT(PTR:PTR+1) .EQ. '61') .OR.
     &           (REPORT(PTR:PTR+1) .EQ.'60')) THEN
                NUM_LEV=NUM_LEV+1
                WATER_TEMP=IVALUE(REPORT(PTR+2:PTR+4))
                IF (REPORT(PTR+1:PTR+1) .EQ. '1') THEN
                   WATER_TEMP=WATER_TEMP*(-1)
                ENDIF
!----------------------------------------------------------------------
!Convert to kelvin
!----------------------------------------------------------------------
                WATER_TEMP_TENS=WATER_TEMP/10.
                WATER_TEMP_KELVIN=WATER_TEMP_TENS+273.1
                ELEMS_ARRAY(10)=TEMP_AVG_REAL
                NUM_LEV=NUM_LEV+1
                ELEMS_ARRAY(11)=WATER_TEMP_KELVIN
                PTR=PTR+6
              ENDIF

!----------------------------------------------------------------------
!Check for and decode the salinity group
!----------------------------------------------------------------------

              IF (REPORT(PTR:PTR) .EQ. '8') THEN
                NUM_LEV=NUM_LEV+1
                SALINITY=IVALUE(REPORT(PTR+1:PTR+4))
                SALINITY_HUND=REAL(SALINITY)/100.
                ELEMS_ARRAY(12)=SALIN_AVG_REAL
                ELEMS_ARRAY(13)=SALINITY_HUND
                PTR=PTR+6
              ENDIF

!----------------------------------------------------------------------
!Check for and decode the sea direction/speed group
!----------------------------------------------------------------------

              IF (REPORT(PTR:PTR) .EQ. '9') THEN
                NUM_LEV=NUM_LEV+1
                SEA_DIRECTION=IVALUE(REPORT(PTR+1:PTR+2))
                SEA_DIRECTION=SEA_DIRECTION*10.
                ELEMS_ARRAY(14)=DDFF_AVG_REAL
                ELEMS_ARRAY(15)=SEA_DIRECTION
                SEA_SPEED=IVALUE(REPORT(PTR+3:PTR+4))
                SEA_SPEED_REAL=REAL(SEA_SPEED)/100.
                IF (KNOTS) THEN
                  SEA_SPEED_REAL=REAL(SEA_SPEED)*(1852/3600)
                ENDIF
                ELEMS_ARRAY(16)=DDFF_AVG_REAL
                NUM_LEV=NUM_LEV+1
                ELEMS_ARRAY(17)=SEA_SPEED_REAL
                PTR=PTR+6
              ENDIF
              NUM_LEV=NUM_LEV+9

!----------------------------------------------------------------------
!Set sea depth to zero i.e. surface
!----------------------------------------------------------------------

              ELEMS_ARRAY(9)=0.0
              ELEMS_ARRAY(1)=1

!----------------------------------------------------------------------
!Since we store each 'part' as an individual report further expansion
!is halted to allow storage of this message. The storage routine will
!then pass control back to this point where upon expansion will
!continue if possible.
!----------------------------------------------------------------------

!First check to see if more to come in this report
              IF (REPORT(PTR:PTR) .GE. '0' .AND. REPORT(PTR:PTR)
     &        .LE. '2') THEN
                STILL_EXPANDING=.TRUE.
              ELSE
                STILL_EXPANDING=.FALSE.
              ENDIF

              CALL TRKSTO(REPORT,CCCC,SingleReportLength,
     &        ICCCC,CORNUM,TTAAII,TOR,NUM_LEV,ELEMS_ARRAY,
     &        CALL_SIGN,IFT)                                       !2.0

!----------------------------------------------------------------------
!If there are more groups to be expanded - increment the loop counter
!so that array displacements can be calculated.
!----------------------------------------------------------------------

              IF (STILL_EXPANDING) THEN
                LOOP_COUNT=LOOP_COUNT+1
                NUM_LEV=0
              ENDIF

!----------------------------------------------------------------------
!The array must be re-set to missing for next time around. The
!variables with the values for the report year/month are left unchanged.
!----------------------------------------------------------------------

              DO CLEAR_ARRAY=1,17,1
                ELEMS_ARRAY(CLEAR_ARRAY)=MISSING
              ENDDO
            ENDIF
          ENDIF                           !End of ISTAT basedon time
          ISTAT=0
        ENDDO
        NUM_REPS=NUM_REPS-1
      ENDDO
      RETURN
      END
