      SUBROUTINE CHSSTN  (CHASE_DATIM , REPORT_TYPE, BLTNID,
     &                    PUBLIC      , HOLIDAY    ,
     &                    CHASE_STNID , STNDATA    , STNLIST)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : CHSSTN                                              
!                                                                     
! PURPOSE       : To create a list of station Ids and elements        
!                 expected for a particular Day and Hour.             
!                                                                     
! DESCRIPTION   : An MDB request for a specified list of Stations or  
!                 WMO block is checked sequentially to extablish      
!                 whether a report is expected for given Day and Hour.
!                 A dataset containing the start times of Summer and  
!                 Winter and the YearDay of all Public Holidays       
!                 affecting Scotland, England, Wales and Northern     
!                 Ireland is used with Flag table checks to construct 
!                 the list.                                           
!                                                                     
! DATA TYPE(S)  : SYNOP, SREW, NCM                                    
! HANDLED                                                            
!                                                                     
! CALLED BY     : CHSMAIN                                             
!                                                                     
! CALLS         : MDB     - System used to retrieve Station details.  
!                 BINCONV - Modulr to convert an Integer value to a   
!                           character string representing the Binary  
!                           value of the Integer.                     
!                                                                     
! PARAMETERS    : IN(I) OUT(O)                                        
!                                                                     
!                (1)I   CHASE_DATIM - Of Chaser validity             
!                (1)I   REPORT_TYPE - Whether SYNOP, SREW or NCM.     
!                (7)I   PUBLIC  - Flag set if Today is a holiday      
!                (8)I   HOLIDAY - A list of Stations on holiday       
!               (10)O   CHASE_STNID - Station Ids to be chased        
!               (11)O   STNDATA - List of suspect or missing elements 
!                                  to be chased                       
!               (12)O   STNLIST - The number of stations to be chased 
!                                                                     
! DATASETS USED : FT30F001  The start and end of British Summer Time  
!                           and a list of Public Holidays and the     
!                           countries affected for the current year.  
!                                                                     
! REVISION INFO  :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:49$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chsstn.F,v $
!
! CHANGE RECORD :
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:49    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:34  usmdb
! Corrected HEAD, removed unused variables. Added copyright
! and modified header - S.Cox
!
! Revision 1.3  2000/03/10  09:10:20  09:10:20  usmdb (Generic MDB account)
! 20/03/2000 Change to a misleading comment.
! 
! Revision 1.2  98/09/30  09:53:52  09:53:52  usmdb (Generic MDB account)
! Variables reordered.
! v(G)=26, ev(G)= 1
!
! Revision 1.1  98/09/08  16:11:39  16:11:39  usmdb (Generic MDB account
! Initial revision
!
! 07/08/98       Moved setting number of obs variable                 !C 
!                STNS_REQUESTED into MDB processing loop.            
!
! 16/07/98       change how ncm expected flags are retrieved          !B 
!                                                                     
! 26/06/98       ALLOW FOR NEW SUMMER/WINTER FLAGS IN DAYSTR          !A 
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

!     Calling arguments

      INTEGER       CHASE_DATIM(*)! The hour of chaser validity.
      INTEGER       REPORT_TYPE   ! Whether SYNOP, SREW or NCM.
      INTEGER       BLTNID        ! Whether UK, Germany or Gib./Cyprus.

      LOGICAL       PUBLIC        ! Determines whether checks are to be
                                  ! made on holiday status of a station.
      CHARACTER*(*) HOLIDAY       ! Yearday of public holidays and
                                  ! countries affected.

      INTEGER       CHASE_STNID(*)! List of station identifiers.
      CHARACTER*(*) STNDATA(*)    ! List of station identifiers and the
                                  ! elements expected.
      INTEGER       STNLIST       ! Number of stations expected to
                                  ! report for the hour being chased.

!     MDB retrieval arguments

      CHARACTER*8   MDB_SUB_TYPE  ! Data type for call to MDB.
      CHARACTER*200 REQUEST_STR   ! ... for MDB call.

      REAL          DATA_ARRAY(1000,48)
     &                            ! Array of data for MDB call.

      INTEGER       STNS_REQUESTED ! Ie -size of retrieval array.

      INTEGER       NELEM         ! Number of elements in MDB request.
      INTEGER       ISTAT         ! Return code status from MDB call.
      CHARACTER*32  CSTR(1000)    ! Character string from MDB call
                                  ! containing character element data.
      CHARACTER*1   CREP(1000)    ! Not applicable but needs a length
                                  ! to ensure a valid MDB call.

      CHARACTER*150 ELEMENT_STR   ! Required data from STNMAS.

!     Working variables

      INTEGER       CURRENT_STN   ! Looping through stations retrieved.
      INTEGER       DAYOFWEEK     ! Used with DAYSTR to make code able
                                  !   easier to read.
      INTEGER       ELEM          ! The displacement within array NELEM
                                  ! being checked.
      INTEGER       FLAG          ! Value of flag table DAYS, SYNELM,
                                  ! SYNTAM, SYNTPM, NCMEL1, NCMEL2,
                                  ! UAHRS, NCMTIM, and pointer to
      INTEGER       HOUR          ! Pointer to correct hour in flag
                                  ! table UAHRS.

      INTEGER       TOTALOFSTNS   ! Total of stations retrieved from MDB

      LOGICAL       CHECK         ! Used to skip sections if no
                                  ! further processing required.
      LOGICAL       SUMMERTIME    ! Whether "chase" day is within Brit-
                                  !   ish SummerTime period.

      CHARACTER*6   CHASTR        ! Binary representation of flag table
                                  ! CHASER.
      CHARACTER*10  DAYSTR        ! Binary representation of flag table
                                  ! DAYS.                            !a
      CHARACTER*12  NCMST1        ! Binary representation of flag table
                                  ! NCMEL1
      CHARACTER*13  NCMST2T       ! Binary representation of flag table
                                  ! NCMEL2                            !b
      CHARACTER*2   NCMTIME       ! Binary representation of flag table
                                  ! NCMTIM.
      CHARACTER*13  SYNSTR        ! Binary representation of flag table
                                  ! SYNELM.
      CHARACTER*12  SYNTIME       ! Binary representation of flag tables
                                  ! SYNTAM and SYNTPM.
!      CHARACTER*4   UATIME        ! Binary representation of flag !2.0
!                                  ! table UAHRS.                  !2.0
      CHARACTER*132  HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/chsstn.F,v $
     &'//'$Date: 30/01/2006 20:21:49$ $Revision: 1$'                                   !2.0

!     Initialise variables

      MDB_SUB_TYPE='STNMAS  '
      REQUEST_STR=' '
      CSTR(1)=' '
      NELEM=48

      ISTAT=0
      STNLIST=0

      DAYOFWEEK=CHASE_DATIM(7)+1                                     !a

      SUMMERTIME=(CHASE_DATIM(1) .GE. CHASE_DATIM(8)   ! Summer start
     &      .AND. CHASE_DATIM(1) .LT. CHASE_DATIM(9))  ! Summer end



!     Set up STNMAS element list for the chaser bulletin ...

      ELEMENT_STR=
     &  'WMO_STN_IX_ID '   //
     &  'CTRY_NAME '       //
     &  'STN_CLS_YR '      //
     &  'UAIR_RPT_TM_ID '  //
     &  'CHSR_SYN_ELEM_ID '//
     &  'CHSR_NCM1_ELEM_ID     CHSR_NCM2_ELEM_ID ' //
     &  'REPORTING_PRACTICE*10 CHSR_BLTN_ID'


!     ... for the UK,  ...
      IF (BLTNID .EQ. 1) THEN
               REQUEST_STR='PLATFORM 03 ELEMENTS ' // ELEMENT_STR

!     ... German OR ...
      ELSEIF (BLTNID .EQ. 2) THEN
        REQUEST_STR=
     &'PLATFORM 10320 10328 10401 10402 10405 ELEMENTS ' // ELEMENT_STR

!     ... Gibraltar/Akrotiri chaser bulletin.
      ELSEIF (BLTNID .EQ. 3) THEN
        REQUEST_STR=
     &          'PLATFORM 08495 17601 SURFACE ELEMENTS ' // ELEMENT_STR
      ENDIF


!     Check each station's details, in the station master
!'      dataset, to establish which stations should be chased.

1     CONTINUE      ! Allow loop-back to retrieve further staions.
      STNS_REQUESTED=1000                                            !C
      CALL MDB (MDB_SUB_TYPE, REQUEST_STR    ,
     &          DATA_ARRAY  , STNS_REQUESTED ,
     &          NELEM       , ISTAT          , CSTR , CREP)
      TOTALOFSTNS=STNS_REQUESTED     ! Set number of reports retrieved.

      DO  CURRENT_STN = 1 , TOTALOFSTNS       ! Stations loop

!       Check if the station is closed.
!       If the value returned for the year of station closure is
!         greater than or equal to zero then the station           !1.5
!         is closed and no checks should be made.

        CHECK=DATA_ARRAY (CURRENT_STN,3) .LT. 0.0    ! Station OPEN.

!       Check if the station is to be chased.
!       If the correct flag, in flag table CHASER, is not set
!         for the bulletin being output then the station is not to be
!         chased and no checks are necessary.

        IF (CHECK) THEN

          FLAG=INT( DATA_ARRAY (CURRENT_STN,48))
          CALL BINCONV(FLAG,CHASTR)

!         IF (CHASTR(1:1) .NE. '1')  CHECK=.FALSE.

!         Flags currently do not differentiate between UK and other stns
          IF (BLTNID .EQ. 1 .AND. CHASTR(1:1) .NE. '1' .OR.
     &        BLTNID .EQ. 2 .AND. CHASTR(2:2) .NE. '1' .OR.
     &        BLTNID .EQ. 3 .AND. CHASTR(3:3) .NE. '1')  CHECK=.FALSE.
        ENDIF


!       Check the station reporting practices.

!       There are ten reporting practice variations which must
!         be checked until a reporting practice for the current day
!         and hour is found. If none suitable is found, the station
!         is not chased.


        IF (CHECK) THEN

          ELEM=8              ! First SYNOP reporting-practice element.

          DO WHILE (ELEM .LE. 48)        ! Elements loop

!           Reporting practice flags (10 * 4)
            IF (DATA_ARRAY (CURRENT_STN,ELEM).GT.0) THEN

              FLAG=INT (DATA_ARRAY (CURRENT_STN,ELEM))
              CALL BINCONV(FLAG,DAYSTR)

!          The next section applies only to U.K bulletins and checks the
!            station is reporting current element in the correct season.

              IF (BLTNID .EQ. 1) THEN

                IF (     SUMMERTIME .AND. DAYSTR(1:1) .NE. '1' .OR.
     &              .NOT.SUMMERTIME .AND. DAYSTR(2:2) .NE. '1')       !a
     &                                                 CHECK=.FALSE.
              ENDIF
            ELSE
              CHECK=.FALSE.   ! No reporting practices given.
            ENDIF


!           This next section checks the location of each station,
!             by country, to determine if it is affected by a public
!             holiday, should the public holiday flag be set.
!           Flag tablee DAYS holds data determining whether a report
!             is expected on a public holiday.

            IF (PUBLIC .AND. CHECK) THEN

              IF (INDEX (HOLIDAY,CSTR(CURRENT_STN)) .NE. 0) THEN

                IF (DAYSTR(3:3) .NE. '1')  CHECK=.FALSE.             !a
              ELSE

                IF (DAYSTR(DAYOFWEEK:DAYOFWEEK) .NE. '1') CHECK=.FALSE.
              ENDIF

!           If the public holiday flag is not set, then a check on
!             the day of the week is made to determine whether a
!             report is expected for that day.

            ELSEIF (.NOT.PUBLIC .AND. CHECK) THEN

              IF (DAYSTR(DAYOFWEEK:DAYOFWEEK) .NE. '1') CHECK=.FALSE.
            ENDIF


!          Lastly, check a report is expected for the hour being chased.

!          Different flag tables are used to determine the time a report
!            is expected according to the data type being chased.
!          Flag tables SYNTAM and SYNTPM are used for SYNOPTIC
!            data, while flag table NCMTIM is used for NCM data.
!          If a report is expected the station identifier and details of
!            expected elements, if any, are held for further processing.

            IF (CHECK) THEN

!             Check for expected SYNOP reports.

              IF (REPORT_TYPE .EQ. 1) THEN

!               Get flags appropriate to am / pm.
                FLAG=INT (DATA_ARRAY (CURRENT_STN, ELEM +1+
     &                                               CHASE_DATIM(6)))
                CALL BINCONV (FLAG,SYNTIME)

!               Adjust report hour for am / pm.
                HOUR=CHASE_DATIM(5)
                HOUR=(HOUR - (CHASE_DATIM(6) *12)) +1

!               Check whether report is expected for this hour.
                IF (SYNTIME(HOUR:HOUR) .EQ. '1') THEN

                  FLAG=INT(DATA_ARRAY(CURRENT_STN,5)) ! Synop elem.flags
                  CALL BINCONV (FLAG,SYNSTR)

                  STNLIST=STNLIST+1
                  CHASE_STNID(STNLIST)=INT(DATA_ARRAY(CURRENT_STN,1))
                  STNDATA(STNLIST)=SYNSTR

                  ELEM=45
                ELSE
                  ELEM=ELEM+4      ! No valid time - try next set
                ENDIF

!             Check for expected SREW reports.

              ELSEIF (REPORT_TYPE .EQ. 2) THEN

                FLAG=INT(DATA_ARRAY(CURRENT_STN,5))
                CALL BINCONV(FLAG,SYNSTR)

                IF (SYNSTR(13:13) .EQ. '1') THEN

                  STNLIST=STNLIST+1
                  CHASE_STNID(STNLIST)=INT(DATA_ARRAY(CURRENT_STN,1))
                  STNDATA(STNLIST)=SYNSTR
                ENDIF
                ELEM=45

!             Check for expected NCM reports.

              ELSEIF (REPORT_TYPE .EQ. 3) THEN

                FLAG=INT(DATA_ARRAY(CURRENT_STN,ELEM+3))
                CALL BINCONV(FLAG,NCMTIME)

!               Check whether NCM expected for this am / pm.
                IF (NCMTIME(CHASE_DATIM(6)+1:CHASE_DATIM(6)+1)
     &                                             .EQ. '1') THEN

!                 2 elements combined to get full set of NCM flags.
                  FLAG=INT(DATA_ARRAY(CURRENT_STN,6))
                  CALL BINCONV(FLAG,NCMST1)
                  FLAG=INT(DATA_ARRAY(CURRENT_STN,7))                !b
                  CALL BINCONV(FLAG,NCMST2t)                         !b

                  STNLIST=STNLIST+1
                  CHASE_STNID(STNLIST)=INT(DATA_ARRAY(CURRENT_STN,1))
                  STNDATA(STNLIST)=NCMST1//NCMST2T(1:11)             !b
                  ELEM=45
                ELSE
                  ELEM=ELEM+4      ! No valid time - try next set
                ENDIF

!!!!!! future development code !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!             Check for expected upper air reports.

!             ELSEIF (REPORT_TYPE .EQ.  ) THEN

!               FLAG=INT(DATA_ARRAY(CURRENT_STN,4)
!               CALL BINCONV(FLAG,UATIME)
!               FLAG=CHASE_DATIM(5)
!               FLAG=(FLAG+AMPM)/5+(1-AMPM)

!               IF (UATIME(FLAG:FLAG) .EQ. '1') THEN

!                 STNLIST=STNLIST+1
!                 CHASE_STNID(STNLIST)=INT(DATA_ARRAY(CURRENT_STN,1))
!                 ELEM=45
!               ELSE
!                 ELEM=ELEM+4        ! No valid time - try next set
!               ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

              ENDIF

!           If the reporting practise did not contain details about
!             the hour being chased, check the next reporting practise.

            ELSE
              ELEM=ELEM+4
              CHECK=.TRUE.
            ENDIF
          ENDDO    ! Elements loop
        ENDIF
      ENDDO        ! Stations loop

!     Check the return code status from the MDB call.  A value of 4
!       implies that there are more data available.
!     (NB STNS_REQUESTED will still have original value of 1000.)

      IF (ISTAT .EQ. 4)  GOTO 1

      RETURN
      END
