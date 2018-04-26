      PROGRAM CHSMAN

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : CHSMAN
!
! PURPOSE       : To issue a chaser bulletin for either the current
!                 date and time or any hour from the previous 23.
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
! CALLED BY     : NONE
!
! CALLS         : CHSSTN   - To obtain a list of stations to be chased
!                 CHSRPS   - To obtain a list of missing or flagged
!                            reports.
!                 CHSBUL   - To output the station identifiers to be
!                            chased.
!                 DATIMX   - To obtain current date and time details
!                 DATE32   - To convert date to year and julian day
!                 UAMPI    - Message file processing
!                 UATAIL   - Indicates end of message
!                 UAWAHL   - Generates a WMO abbreviated header line
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:46$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chsman.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:46    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:33  usmdb
! Removed unused variable STN. Added copyright and
! modified header - S.Cox
! 2.0a - Added NOW array to call of CHSDAY - S.Kellett
!
! Revision 1.5  2000/03/10  09:09:51  09:09:51  usmdb (Generic MDB account)
! 14/01/2000 BUGFIX so that the PUBLIC flag gets set to true on
!            Public Holidays.        Stan Kellett
!
! Revision 1.4  99/09/09  10:16:08  10:16:08  usmdb (Generic MDB account)
! Change date: 20-09-99
! Correct positioning of UASCPY=UASTAT. This prevents hex 0's
! being written to the chaser datasets - S.Cox
!
! 06/09/1998      Add test option. John Norton                        !b
!
! 25/08/1998      Remove reference to YES2DA. John Norton             !a
!
! 18/12/1997      Add details of MET.PROCLIB members called to header
!                 documentation. John Norton
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
!234567

! Declare variables

      INTEGER       BLTNID          ! Variable used to loop the number
                                    ! of bulletins being output.
      INTEGER       CHASE_DATIM(9)  ! Array of data from DATIMX.
      INTEGER       CURCD           ! CENTURY DAY                    !a
      INTEGER       INPUT_HOUR      ! When supplied by operator.
      INTEGER       NOW(14)         ! Array of data from DATIMX.
      INTEGER       IOS             ! Return code value from IOSTAT
      INTEGER       LOOPVAR         ! Loop variable for initialisation
      INTEGER       NUMREC          ! Loop variable used when reading
                                    ! public holiday information.
      INTEGER       REPORT_TYPE     ! Variable used to loop the number
                                    ! of data types being chased.
      INTEGER       NUMTYPE         ! The number of data types to be
                                    ! chased for a bulletin.
      INTEGER       NCM             ! Value used to modify the hour
                                    ! being chased for NCM data.
      INTEGER       NUMHOLS         ! The number of public holidays
                                    ! in the current year.
      INTEGER       PARMCH          ! The character length of the
                                    ! hour input by the operator when
                                    ! chasing a particular hour.
      INTEGER       RC              ! The value returned by the MHS
                                    ! system when attempting to allocate
                                    ! an output dataset.
      INTEGER       SPACE           ! The space required by the MHS
                                    ! for the output dataset.
      INTEGER       CHASE_STNID(500)! The list of stations whose reports
                                    ! are expected for the hour being
                                    ! chased.
      INTEGER       CHECK_STNID(500)! Array associated with the above
                                    ! but used only in CHSRPS  -declared
                                    ! here only to allow all these check
                                    ! arrays to have variable dimension
                                    ! in the subsidiary routines.
      LOGICAL       STN_MATCH(500)  ! Whether corresponding station on
                                    ! STNID was mathed by data retrieved
                                    ! in CHSRPS.
      INTEGER       STNLIST         ! The number of stations whose
                                    ! reports are expected for the hour
                                    ! being chased.
      INTEGER       PARM_READ       ! Length of parameter string read.

      LOGICAL       PUBLIC          ! Flag set if the current yearday is
                                    ! a public holiday.

      CHARACTER*44  DSN             ! The dataset name allocated by the
                                    ! MHS system.

      CHARACTER*29  MISNG_RPRTS(500) ! The list of station identifiers
                                    ! whose reports are missing.
      INTEGER       MISRCNT         ! The number of stations whose
                                    ! reports are missing.
      CHARACTER*89  MISNG_ELEMS(500) ! The list of station identifiers
                                    ! and flagged element abbreviations
                                    ! for output.
      INTEGER       MISECNT         ! The number of stations whose
                                    ! elements are flagged.

      CHARACTER*3   NRB             ! Required in a call to the MHS
                                    ! system, but not used.
      CHARACTER*80  HOLIDAY         ! Countries on holiday for the
                                    ! hour being chased.
      CHARACTER*2   REQUEST         ! The hour requested by the operator
                                    ! to be chased, should the operator
                                    ! decide to run a special or repeat
                                    ! chase.
      CHARACTER*4   UASTAT          ! The output bulletin status.
      CHARACTER*4   UASCPY          ! Copy of output bulletin status.
      CHARACTER*23  STNDATA(500)    ! The list of elements expected for
                                    ! the hour being chased.
      CHARACTER*6   TTAA            ! The bulletin identifier.
      CHARACTER*7   BLTNHDR         ! The day and hour of the bulletin.

      CHARACTER*79  ENDLINE1        ! END OF FT12 MESSAGE MARKER 1
      CHARACTER*79  ENDLINE2        ! END OF FT12 MESSAGE MARKER 2
      CHARACTER*3   HOLIDAY_CHAR    ! character used in setting !1.5
                                    ! Public Holiday Flag  !1.5
      CHARACTER*132  HEAD

       HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/chsman.F,v $
     &$'//'$ $Date: 30/01/2006 20:21:46$ $Revision: 1$'

!     Initialise variables

      RC=1
      NCM=0
      NUMREC=1
      IOS=0
      HOLIDAY=' '
      PUBLIC=.FALSE.
      SPACE=35
      UASTAT='AOG0'      ! operational value
!!!   UASTAT='DOT9'      ! test debug output (operational value is AOG0)

!     Call the system clock to get the current date and time.
!     Time details used from the array are;

!    now( 5) = hour
!    now( 6) = day
!    now( 7) = month
!    now( 8) = year
!    now(12) = day of the week
!    now(13) = yearday
      CALL DATIMX (NOW)

!    Check the operator over-ride value to find out if a particular
!      hour is to be chased. A validity check is made on the input
!      value.  If the input hour is greater than the current hour,
!      then the chase is for the previous day.
!    The day of the month, month and yearday are amended if necessary.
!    If there is no operator intervention then the chase will be made
!      for the previous hour.
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

!     IF (PARMCH(REQUEST) .EQ. 2) THEN
      PARM_READ = PARMCH(REQUEST)

      IF (PARM_READ .EQ. 2.AND.REQUEST.NE.'  ') THEN

        READ (REQUEST,'(I2)') INPUT_HOUR

! Allow chaser to be run without transmitting message by adding 50 to
! input_hour in parameter field.

        IF (INPUT_HOUR.GE.50 .AND. INPUT_HOUR.LE.73)THEN             !b
           UASTAT='DOT9'      ! test debug output                    !b
           INPUT_HOUR=INPUT_HOUR-50                                  !b
           RC=0                                                     !1.3
        ENDIF                                                        !b

        IF (INPUT_HOUR .GE. 0 .AND. INPUT_HOUR .LE. 23
     &                        .AND. INPUT_HOUR .GE. NOW(5)) THEN

!         Set Yesterday's date and Julian date. '
          CALL DATE31(NOW(6),NOW(7),NOW(8),CURCD)                   !a
          CALL DATE13((CURCD-1),NOW(6),NOW(7),NOW(8))               !a
          CALL DATE32 (NOW(6), NOW(7), NOW(8), NOW(13), NOW(8))
        ENDIF
        NOW(5)=INPUT_HOUR
      ENDIF

      UASCPY=UASTAT            ! Keep copy of UASTAT                !1.4
      CHASE_DATIM(1)=NOW(13)
      CHASE_DATIM(2)=NOW(8)
      CHASE_DATIM(3)=NOW(7)
      CHASE_DATIM(4)=NOW(6)
      CHASE_DATIM(5)=NOW(5)

!     Set the day of the week pointer allowing for the summer/
!       winter and public holiday flags.

      CHASE_DATIM(7)=NOW(12) +2

!     If the hour being chased is in the afternoon (12-23Z), then
!        set the am/pm flag.

      IF (CHASE_DATIM(5) .GT. 11) THEN
        CHASE_DATIM(6)=1
      ELSE
        CHASE_DATIM(6)=0
      ENDIF

!     Create the bulletin time using the current day and hour.

      WRITE (BLTNHDR,'(2I2.2, A)') NOW(6), NOW(5), '00'

!     Allocate the Message Handling System dataset.  The job
!       cannot continue until this is successful.
!     Upon successful allocation, output the name of the dataset
!       allocated to allow browsing using MHS system AFTER THE SET
!       HAS BEEN FREED.


        CALL UAMPI (UASTAT, SPACE, DSN, RC)
        WRITE (6,*) DSN

      IF(RC.EQ.0)THEN                                               !1.3
!     Loop the number of bulletins to be broadcast.

      DO BLTNID=1,3

!       Set the bulletin identifier and, for bulletins SXDL80 and
!          SXMM80, the chaser identifier REPORT_TYPE.

        IF (BLTNID .EQ. 1) THEN

          TTAA='SXUK80'

        ELSEIF (BLTNID .EQ. 2) THEN

          TTAA='SXDL80'
          CHASE_DATIM(5)=NOW(5)  ! Ensure hour is correct after NCM

        ELSEIF (BLTNID .EQ. 3) THEN

          TTAA='SXMM80'
          CHASE_DATIM(5)=NOW(5)  ! Ensure hour is correct after NCM
        ENDIF

!       Construct the bulletin header using the time and identifier.
        NRB='   '
        CALL UAWAHL (TTAA, BLTNHDR, NRB)
        WRITE(12,'(1X,A,1X,A)')TTAA, BLTNHDR              ! test line

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

        IF (BLTNID .EQ. 1) THEN

          OPEN (30,FILE='HOLIDAY')
          READ (30,'(2(/),3(I3.3,1X))')
     &                   NUMHOLS,CHASE_DATIM(8),CHASE_DATIM(9)
          DO WHILE (NUMREC .LE. NUMHOLS)

            READ(30,'(A80)') HOLIDAY
            IF (HOLIDAY(1:1) .NE. ' ' .AND. IOS .NE. -1) THEN

              WRITE (HOLIDAY_CHAR,'(I3.3)')CHASE_DATIM(1)        !1.5

              IF (HOLIDAY_CHAR.EQ.HOLIDAY(1:3)) THEN             !1.5
                PUBLIC= .TRUE.
                NUMREC=NUMHOLS
              ENDIF
            ENDIF
            NUMREC=NUMREC +1
          ENDDO
          CLOSE(30)

!         SYNOP and SREW reports are chased hourly.
!         NCM reports are chased at 11Z and 23Z. Set the number of
!           data types to be checked accordingly.

          IF (CHASE_DATIM(5) .EQ. 11 .OR. CHASE_DATIM(5) .EQ. 23) THEN

            NUMTYPE=3
          ELSE
            NUMTYPE=2
          ENDIF

!       Bulletins SXDL80 and SXMM80 will be for SYNOP data only.

        ELSE
          NUMTYPE=1
        ENDIF

!       Loop the number of data types being chased within each bulletin.

        DO REPORT_TYPE= 1, NUMTYPE

!       Initialise the count of missing stations, missing elements and
!         suspect elements.

          MISRCNT=0
          MISECNT=0
          STNLIST=0
          DO LOOPVAR=1,500
            CHASE_STNID(LOOPVAR)=0
            MISNG_ELEMS(LOOPVAR)(1:)=' '
            MISNG_RPRTS(LOOPVAR)(1:)=' '
            STNDATA(LOOPVAR)(1:)=' '
          ENDDO

!         Set the chaser identifier for the SXUK80 bulletin.
!         The data type could be either SYNOP, SREW or NCM so use
!           the value of REPORT_TYPE to determine the data type.
!         If NCM data is being chased, offset the hour being chased
!            to 2 hours previous to the hour being chased.

          IF (BLTNID.EQ.1 .AND. REPORT_TYPE.EQ.3)
     &                               CHASE_DATIM(5)=CHASE_DATIM(5) -2

!         Obtain a list of all stations who are expected to report
!           for the hour being chased.

          CALL CHSSTN (CHASE_DATIM   , REPORT_TYPE, BLTNID,
     &                  PUBLIC       , HOLIDAY    ,
     &                  CHASE_STNID  , STNDATA    , STNLIST)

          IF (STNLIST .GT. 0) THEN

!           Check databank for receipt of expected reports.
!          (NB. CHECK_STNID is effectively a dummy argument.)

            CALL CHSRPS(CHASE_DATIM, BLTNID   , REPORT_TYPE,
     &                   CHASE_STNID, STNDATA , STN_MATCH  , STNLIST,
     &                   CHECK_STNID,
     &                   MISNG_RPRTS, MISRCNT ,
     &                   MISNG_ELEMS, MISECNT)
          ENDIF

!         Output the UASTATions identifiers of any UASTATions whose
!          reports have not yet been received and an abbreviation of
!          any elements flagged as either missing or suspected
!          incorrect, if any.  Should all reports be received and/or
!          no elements flagged then a message is output to confirm this.


          CALL CHSBUL (CHASE_DATIM, REPORT_TYPE,
     &                  MISNG_RPRTS, MISRCNT    ,
     &                  MISNG_ELEMS, MISECNT    ,
     &                  STNLIST)

        ENDDO                    ! Report-type loop.
        CALL UATAIL              ! Terminate country message.
      ENDDO                      ! Country loop.

! Run daily chaser at 08z.

      IF(CHASE_DATIM(5).EQ.8)THEN
        CALL CHSDAY (CHASE_DATIM,BLTNHDR,NOW,PUBLIC,HOLIDAY)       !2.0a
      ENDIF

      UASTAT='F '                ! Free the message file.
      CALL UAMPI (UASTAT, SPACE, DSN, RC)

      IF(UASCPY.EQ.'AOG0')THEN
        WRITE (6,'(A)') ' CHASER MESSAGE FILE RELEASED FOR TRANSMISSION'
      ELSE
        WRITE (6,'(A)') ' TEST CHASER MESSAGE NOT TRANSMITTED '
      ENDIF

      ENDLINE1(01:40)='========================================'
      ENDLINE1(41:79)='======================================='
      ENDLINE2=ENDLINE1
      ENDLINE2(21:43)=' END OF BLTNHDRMESSAGE '
      ENDLINE2(29:35)=BLTNHDR
      ENDLINE2(48:51)=UASCPY(1:4)
      WRITE(12,'(A)')ENDLINE1
      WRITE(12,'(A)')ENDLINE2
      WRITE(12,'(A)')ENDLINE1

      ELSE                                                         !1.3
        WRITE (6,'(A)')                                            !1.3
     &    ' CHASER MESSAGE NOT TRANSMITTED. UAMFP PROBLEM'         !1.3
      ENDIF                                                        !1.3

      STOP
      END
