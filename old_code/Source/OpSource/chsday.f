      SUBROUTINE CHSDAY (CHASE_DATIM,BLTNHDR,NOW,PUBLIC,HOLIDAY)   !2.0a

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : CHSDAY
!
! PURPOSE       : To issue the daily chaser bulletin. It only checks
!                 for missing reports.
!                 On Wednesday to Monday it is for all hours on day-2.
!                 On Tuesday it is for day-8 to day-2, all hours.
!
! DESCRIPTION   : The date and time NOW is established and a check
!                 is made on whether the chaser bulletin is for the
!                 current or a previous hour.
!                 The Message Handling System (MHS) is opened - this
!                 act also creates a dataset into which one or more
!                 messages may be written.
!                 There are currently 3 chaser bulletins, one for UK,
!                 one for German and one for Gibralter/Akrotiri
!                 stations.  The required message header for whichever
!                 of these messages is to be sent is written by MHS
!                 to the above dataset.
!                 A loop is set up to fetch and output data for each
!                 of these bulletins sequentially so any additional
!                 bulletins can be added by just increasing the number
!                 of loops.
!                 Other routines are called to obtain the station
!                 identifiers to be chased and whether their reports
!                 have been received, so this program is, in essence,
!                 to control the order in which bulletins are output
!                 and to establish all the main array sizes.
!                 Should any of the arrays need amending, the only
!                 program that will require amendment is this.
!
! DATA TYPE(S)  : SYNOP, NCM, SREW
!  HANDLED
!
! CALLED BY     : CHSMAN
!
! CALLS         : CHSDST   - To obtain a list of stations to be chased
!                 CHSDRP   - To obtain a list of missing or flagged
!                            reports.
!                 CHSDBL   - To output the station identifiers to be
!                            chased.
!                 DATE31   - To convert date to century day
!                 DATE13   - To convert century day to date
!                 UAMPI    - Message file processing
!                 UATAIL   - Indicates end of message
!                 UAWAHL   - Generates a WMO abbreviated header line
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:43$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chsday.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:43    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:31  usmdb
! Removed unused variables. Removed argument STNLIST from
! call to CHSDBL and argument REPORT_TYPE from call to
! CHSDST. Removed variable BLANK as it is not set.
! BUFFER(1:)=BLANK changed to BUFFER(1:)=' '. Added
! copyright and modified header - S.Cox
! !2.0a Added NOW as an input argument so as to initialise
! NOW array - S.Kellett
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

! Declare variables

      INTEGER       BLTNID          ! Variable used to loop the number
                                    ! of bulletins being output.
      INTEGER       CDREQ           ! Century day being processed
      INTEGER       CENT_DAY_1ST    ! First century day of daily sweep
      INTEGER       CENT_DAY_LAST   ! Last century day of daily sweep
      INTEGER       CHASE_DATIM(9)  ! Array of data from DATIMX.
      INTEGER       CHASE_HOUR      ! Hour for which reports are being
                                    ! checked
      INTEGER       CHASE_STNID_DAY (500)! The list of stations whose
                                    ! reports are expected for the day
                                    ! being chased.
      INTEGER       CHASE_STNID(500)! The list of stations whose reports
                                    ! are expected for the hour being
                                    ! chased.
      INTEGER       CHECK_STNID(500)! Array associated with the above
                                    ! but used only in CHSRPS  -declared
                                    ! here only to allow all these check
                                    ! arrays to have variable dimension
                                    ! in the subsidiary routines.
      INTEGER       CURCD           ! Century day of today
      INTEGER       FIRST_HOUR      ! First hour to be retrieved
      INTEGER       INTERVAL        ! Hours loop interval
      INTEGER       LAST_HOUR       ! Last hour to be retrieved
      INTEGER       LOOP_PRAC       ! Station practice copy loop
      INTEGER       LOOPVAR         ! Loop variable for initialisation
      INTEGER       NOW(14)         ! Array of data from DATIMX.
      INTEGER       MISRCNT         ! The number of stations whose
                                    ! reports are missing.
      INTEGER       NUMTYPE         ! The number of data types to be
                                    ! chased for a bulletin.
      INTEGER       REPORT_TYPE     ! Variable used to loop the number
                                    ! of data types being chased.
      INTEGER       STNLIST_DAY     ! The number of stations whose
                                    ! reports are expected for the day
                                    ! being chased.
      INTEGER       STNLIST         ! The number of stations whose
                                    ! reports are expected for the hour
                                    ! being chased.
      INTEGER       WKDAY           ! Day of week

      LOGICAL       PUBLIC          ! Flag set if the current yearday is
                                    ! a public holiday.
      LOGICAL       STN_MATCH(500)  ! Whether corresponding station on
                                    ! STNID was mathed by data retrieved
                                    ! in CHSRPS.

      CHARACTER*7   BLTNHDR         ! The day and hour of the bulletin.

      CHARACTER*68  BUFFER          ! The output data string.
      CHARACTER*10  DAY_OF_WEEK     ! name of day of week
      CHARACTER*80  HOLIDAY         ! Countries on holiday for the
                                    ! hour being chased.
      CHARACTER*29  MISNG_RPRTS(500) ! The list of station identifiers
                                    ! whose reports are missing.
      CHARACTER*3   NRB             ! Required in a call to the MHS
                                    ! system, but not used.
      CHARACTER*27  STNPRAC_DAY(500) ! The list of elements expected
                                    ! for the day being chased.
      CHARACTER*27  STNPRAC(500)    ! The list of elements expected for
                                    ! the hour being chased.
      CHARACTER*6   TTAA            ! The bulletin identifier.
      CHARACTER*132  HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/chsday.F,v $
     &'//'$Date: 30/01/2006 20:21:43$ $Revision: 1$"'

!     Initialise variables

!    Time details stored in array CHASE_DATIM are;

!    CHASE_DATIM(1) = yearday
!    CHASE_DATIM(2) = year
!    CHASE_DATIM(3) = month
!    CHASE_DATIM(4) = day
!    CHASE_DATIM(5) = hour
!    CHASE_DATIM(6) = am/pm flag
!    CHASE_DATIM(7) = day of the week
!    CHASE_DATIM(8) = start of summer time
!    CHASE_DATIM(9) = end of summer time

!     Set first and last day for days required.

      CALL DATE31(CHASE_DATIM(4),CHASE_DATIM(3),CHASE_DATIM(2),CURCD)
      CENT_DAY_LAST=CURCD-2
      CENT_DAY_1ST=CENT_DAY_LAST

      IF(CHASE_DATIM(7).EQ.5)CENT_DAY_1ST=CENT_DAY_LAST-6 !TUESDAY RUN

!     Loop the number of bulletins to be broadcast.

      DO BLTNID=11,13

!       Set the bulletin identifier and, for bulletins SXDL80 and
!          SXMM80, the chaser identifier REPORT_TYPE.

        IF (BLTNID .EQ. 11) THEN

          TTAA='SXUK80'

        ELSEIF (BLTNID .EQ. 12) THEN

          TTAA='SXDL80'
          CHASE_DATIM(5)=NOW(5)  ! Ensure hour is correct after NCM

        ELSEIF (BLTNID .EQ. 13) THEN

          TTAA='SXMM80'
          CHASE_DATIM(5)=NOW(5)  ! Ensure hour is correct after NCM
        ENDIF

!       Construct the bulletin header using the time and identifier.
        NRB='   '
        CALL UAWAHL (TTAA, BLTNHDR, NRB)
        WRITE(14,'(1X,A,1X,A)')TTAA, BLTNHDR

!       For the U.K bulletin only, a check on public holidays
!        and seasonal reporting practices is made.
!       The start times of summer and winter and the yeardays
!         of public holidays in Scotland, England, Wales and
!         Northern Ireland are held in a dataset.
!       The list is read sequentially with a check made each time for a
!         match between the current yearday and the list of holidays.
!       If the current yearday matches any of the public holidays
!         listed then the countries affected are kept for later use.
!       If no match is found it is assumed the current yearday is
!         not a public holiday.

        IF (BLTNID .EQ. 11) THEN

!         SYNOP and SREW reports are chased hourly.
!         NCM reports are reported at 09Z and 21Z.

          NUMTYPE=3

!       Bulletins SXDL80 and SXMM80 will be for SYNOP data only.

        ELSE
          NUMTYPE=1
        ENDIF

!       Loop the number of data types being chased within each bulletin.

        DO REPORT_TYPE= 1, NUMTYPE

!       Add the standard text requesting re-transmission.

          IF(REPORT_TYPE.EQ.1)THEN
            BUFFER='THE FOLLOWING SYNOP REPORTS ARE STILL MISSING FROM'
     &           // ' THE DATABANK.'
            CALL UADATA(BUFFER,64)
            WRITE(14,'(1X,A)')BUFFER
            BUFFER='PLEASE SEND LATE BULLETIN USING OS@OLDS '
            CALL UADATA(BUFFER,40)
            WRITE(14,'(1X,A)')BUFFER
          ELSEIF(REPORT_TYPE.EQ.2)THEN
            BUFFER='THE FOLLOWING SREW REPORTS ARE STILL MISSING FROM'
     &           // ' THE DATABANK.'
            CALL UADATA(BUFFER,63)
            WRITE(14,'(1X,A)')BUFFER
! Removed code to write out line asking user to resend SREW report !1.5
          ELSEIF(REPORT_TYPE.EQ.3)THEN
            BUFFER='THE FOLLOWING NCM REPORTS ARE STILL MISSING FROM'
     &           // ' THE DATABANK.'
            CALL UADATA(BUFFER,62)
            WRITE(14,'(1X,A)')BUFFER
            BUFFER='PLEASE SEND LATE BULLETIN USING OC@OLDC '      !1.3
            CALL UADATA(BUFFER,40)
            WRITE(14,'(1X,A)')BUFFER
          ENDIF

          BUFFER(1:)=' '                                            !2.0
          CALL UADATA(BUFFER,1)
          BUFFER(1:)=' '
          WRITE(14,'(1X,A)')BUFFER

! Set hours required for differing report types

          IF(REPORT_TYPE.EQ.3)THEN
            FIRST_HOUR=09
            LAST_HOUR=21
            INTERVAL=12
          ELSE
            FIRST_HOUR=00
            LAST_HOUR=23
            INTERVAL=01
          ENDIF

!       Loop the days required

          DO CDREQ=CENT_DAY_1ST,CENT_DAY_LAST

            CALL DATE13(CDREQ,CHASE_DATIM(4),CHASE_DATIM(3),
     &                        CHASE_DATIM(2))


!     Set the day of the week pointer allowing for the summer/
!       winter and public holiday flags.

            WKDAY = MOD(CDREQ,7)

!
! SUNDAY=0, SATURDAY=6
            IF    (WKDAY.EQ.0)THEN
              CHASE_DATIM(7)= 0
              DAY_OF_WEEK= '   SUNDAY '
            ELSEIF(WKDAY.EQ.1)THEN
              CHASE_DATIM(7)= 1
              DAY_OF_WEEK= '   MONDAY '
            ELSEIF(WKDAY.EQ.2)THEN
              CHASE_DATIM(7)= 2
              DAY_OF_WEEK= '  TUESDAY '
            ELSEIF(WKDAY.EQ.3)THEN
              CHASE_DATIM(7)= 3
              DAY_OF_WEEK= 'WEDNESDAY '
            ELSEIF(WKDAY.EQ.4)THEN
              CHASE_DATIM(7)= 4
              DAY_OF_WEEK= ' THURSDAY '
            ELSEIF(WKDAY.EQ.5)THEN
              CHASE_DATIM(7)= 5
              DAY_OF_WEEK= '   FRIDAY '
            ELSEIF(WKDAY.EQ.6)THEN
              CHASE_DATIM(7)= 6
              DAY_OF_WEEK= ' SATURDAY '
            ENDIF


!       Loop through the hours required

            DO CHASE_HOUR=FIRST_HOUR,LAST_HOUR,INTERVAL
               CHASE_DATIM(5)=CHASE_HOUR

!     If the hour being chased is in the afternoon (12-23Z), then
!        set the am/pm flag.

              IF (CHASE_DATIM(5) .GT. 11) THEN
                CHASE_DATIM(6)=1
              ELSE
                CHASE_DATIM(6)=0
              ENDIF
!       Initialise the count of missing stations, missing elements and
!         suspect elements.

              MISRCNT=0
              STNLIST=0
              DO LOOPVAR=1,500
                CHASE_STNID(LOOPVAR)=0
                MISNG_RPRTS(LOOPVAR)(1:)=' '
                STNPRAC(LOOPVAR)(1:)=' '
              ENDDO

!         Set the chaser identifier for the SXUK80 bulletin.
!         The data type could be either SYNOP, SREW or NCM so use
!           the value of REPORT_TYPE to determine the data type.
!         If NCM data is being chased, offset the hour being chased
!            to 2 hours previous to the hour being chased.

!         Obtain a list of all stations who are expected to report
!           for the hour being chased.

               IF(CHASE_DATIM(5).EQ.FIRST_HOUR)THEN
                 CALL CHSDST (CHASE_DATIM   , BLTNID,               !2.0
     &                        PUBLIC       , HOLIDAY    ,
     &                        CHASE_STNID_DAY, STNPRAC_DAY, STNLIST_DAY)
               ENDIF

!         Copy daily practice values to working arrays for each hour.

               DO LOOP_PRAC=1,STNLIST_DAY
                 CHASE_STNID(LOOP_PRAC)=CHASE_STNID_DAY(LOOP_PRAC)
                 STNPRAC(LOOP_PRAC)=STNPRAC_DAY(LOOP_PRAC)
               ENDDO
               STNLIST=STNLIST_DAY

               IF (STNLIST .GT. 0) THEN

!           Check databank for receipt of expected reports.
!          (NB. CHECK_STNID is effectively a dummy argument.)

                 CALL CHSDRP(CHASE_DATIM, BLTNID   , REPORT_TYPE,
     &                   CHASE_STNID, STNPRAC , STN_MATCH  , STNLIST,
     &                   CHECK_STNID,
     &                   MISNG_RPRTS, MISRCNT )
               ENDIF

!         Output the UASTATions identifiers of any UASTATions whose
!          reports have not yet been received and an abbreviation of
!          any elements flagged as either missing or suspected
!          incorrect, if any.  Should all reports be received and/or
!          no elements flagged then a message is output to confirm this.


               CALL CHSDBL (CHASE_DATIM, REPORT_TYPE,
     &                  MISNG_RPRTS, MISRCNT,
     &                  DAY_OF_WEEK)                                !2.0

            ENDDO                    ! hours loop.
          ENDDO                    ! days loop.
        ENDDO                    ! Report-type loop.
        CALL UATAIL              ! Terminate country message.
      ENDDO                    ! Bulletin loop.

      RETURN
      END
