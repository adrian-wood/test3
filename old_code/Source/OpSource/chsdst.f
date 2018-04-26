      SUBROUTINE CHSDST  (CHASE_DATIM , BLTNID,                     !2.0
     &                    PUBLIC      , HOLIDAY    ,
     &                    CHASE_STNID , STNPRAC    , STNLIST)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : CHSDST                                              
!                                                                     
! PURPOSE       : To create a list of station Ids and practice        
!                 expected for a particular Day.                      
!                 This program will only work for a day at a time due 
!                 variations between practices on different days,     
!                 between winter and summer time and on public        
!                 holidays!                                           
!                                                                     
! DESCRIPTION   : An MDB request for a specified list of Stations or  
!                 WMO block is checked sequentially to extablish      
!                 whether a report is expected for given Day.         
!                 A dataset containing the start times of Summer and  
!                 Winter and the YearDay of all Public Holidays       
!                 affecting Scotland, England, Wales and Northern     
!                 Ireland is used with Flag table checks to construct 
!                 the list.                                           
!                                                                     
! DATA TYPE(S)  : SYNOP, SREW, NCM                                    
!  HANDLED                                                            
!                                                                     
! CALLED BY     : CHSDAY                                              
!                                                                     
! CALLS         : MDB     - System used to retrieve Station details.  
!                 BINCONV - Modulr to convert an Integer value to a   
!                           character string representing the Binary  
!                           value of the Integer.                     
!                                                                     
! PARAMETERS    : IN(I) OUT(O)                                        
!                                                                     
!                (1)I   CHASE_DATIM - Of Chaser validity             
!                (7)I   PUBLIC  - Flag set if Today is a holiday      
!                (8)I   HOLIDAY - A list of Stations on holiday       
!               (10)O   CHASE_STNID - Station Ids to be chased        
!               (11)O   STNPRAC - List of reporting hours.            
!                          (1-24)  SYNOP hours (0-23)                 
!                          (25)    SREW reported                      
!                          (26,27) NCM hours (09,21)                  
!               (12)O   STNLIST - The number of stations to be chased 
!                                                                     
! DATASETS USED : FT30F001  The start and end of British Summer Time  
!                           and a list of Public Holidays and the     
!                           countries affected for the current year.  
!                                                                     
! REVISION INFO  :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:45$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chsdst.F,v $
!
! CHANGE RECORD  :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:45    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:33  usmdb
! Removed unused dummy argument REPORT_TYPE, correct HEAD,
! added copyright and modified header - S.Cox
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
      INTEGER       BLTNID        ! Whether UK, Germany or Gib./Cyprus.

      LOGICAL       PUBLIC        ! Determines whether checks are to be
                                  ! made on holiday status of a station.
      CHARACTER*(*) HOLIDAY       ! Yearday of public holidays and
                                  ! countries affected.

      INTEGER       CHASE_STNID(*)! List of station identifiers.
      CHARACTER*(*) STNPRAC(*)    ! List of station identifiers and the
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

      INTEGER       ELEM          ! The displacement within array NELEM
                                  ! being checked.
      INTEGER       FLAG          ! Value of flag table DAYS, SYNELM,
                                  ! SYNTAM, SYNTPM, NCMEL1, NCMEL2,
                                  ! UAHRS, NCMTIM, and pointer to

      INTEGER       TOTALOFSTNS   ! Total of stations retrieved from MDB
      INTEGER       CURRENT_STN   ! Looping through stations retrieved.

      INTEGER       DAYOFWEEK     ! Used with DAYSTR to make code able
                                  !   easier to read.
      LOGICAL       SUMMERTIME    ! Whether "chase" day is within Brit-
                                  !   ish SummerTime period.

      LOGICAL       CHECK         ! Used to skip sections if no
                                  ! further processing required.

      CHARACTER*6   CHASTR        ! Binary representation of flag table
                                  ! CHASER.
      CHARACTER*10  DAYSTR        ! Binary representation of flag table
                                  ! DAYS.
      CHARACTER*2   NCMTIME       ! Binary representation of flag table
                                  ! NCMTIM.
      CHARACTER*13  SYNSTR        ! Binary representation of flag table
                                  ! SYNELM.
      CHARACTER*12  SYNTIME       ! Binary representation of flag tables
                                  ! SYNTAM and SYNTPM.
      CHARACTER*132  HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/chsdst.F,v $
     &'//'$Date: 30/01/2006 20:21:45$ $Revision: 1$'                                   !2.0

!     Initialise variables

      MDB_SUB_TYPE='STNMAS  '
      REQUEST_STR=' '
      CSTR(1)=' '
      NELEM=48

      ISTAT=0
      STNLIST=0

      DAYOFWEEK=CHASE_DATIM(7)+4
c###  PRINT*,'chsdst DAYOFWEEK,CHASE_DATIM(7)',DAYOFWEEK,CHASE_DATIM(7)

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
      IF (BLTNID .EQ. 11) THEN
               REQUEST_STR='PLATFORM 03 ELEMENTS ' // ELEMENT_STR

!     ... German OR ...
      ELSEIF (BLTNID .EQ. 12) THEN
        REQUEST_STR=
     &'PLATFORM 10320 10328 10401 10402 10405 ELEMENTS ' // ELEMENT_STR

!     ... Gibraltar/Akrotiri chaser bulletin.
      ELSEIF (BLTNID .EQ. 13) THEN
        REQUEST_STR=
     &          'PLATFORM 08495 17601 SURFACE ELEMENTS ' // ELEMENT_STR
      ENDIF


!     Check each station's details, in the station master
!'      dataset, to establish which stations should be chased.

1     CONTINUE      ! Allow loop-back to retrieve further staions.
      STNS_REQUESTED=1000
      CALL MDB (MDB_SUB_TYPE, REQUEST_STR    ,
     &          DATA_ARRAY  , STNS_REQUESTED ,
     &          NELEM       , ISTAT          , CSTR , CREP)
      TOTALOFSTNS=STNS_REQUESTED     ! Set number of reports retrieved.

      DO  CURRENT_STN = 1 , TOTALOFSTNS       ! Stations loop

!       Check if the station is closed.
!       If the value returned for the year of station closure is
!         greater than zero then the station is closed and no
!         checks should be made.

        CHECK=DATA_ARRAY (CURRENT_STN,3) .LT. 0.0    ! Station OPEN.

!       Check if the station is to be chased.
!       If the correct flag, in flag table CHASER, is not set
!         for the bulletin being output then the station is not to be
!         chased and no checks are necessary.
c       IF (BLTNID .EQ. 13)
c    & print*,' chsdst ',DATA_ARRAY (CURRENT_STN,1),' station open '
        IF (CHECK) THEN

          FLAG=INT( DATA_ARRAY (CURRENT_STN,48))
          CALL BINCONV(FLAG,CHASTR)

!         IF (CHASTR(1:1) .NE. '1')  CHECK=.FALSE.

!         Flags currently do not differentiate between UK and other stns
          IF (BLTNID .EQ. 11 .AND. CHASTR(1:1) .NE. '1' .OR.
     &        BLTNID .EQ. 12 .AND. CHASTR(2:2) .NE. '1' .OR.
     &        BLTNID .EQ. 13 .AND. CHASTR(3:3) .NE. '1')  CHECK=.FALSE.
        ENDIF
c       IF (BLTNID .EQ. 13)
c    & print*,' chsdst ',DATA_ARRAY (CURRENT_STN,1),' station chased '


!       Check the station reporting practices.

!       There are ten reporting practice variations which must
!         be checked until a reporting practice for the current day
!         and hour is found. If none suitable is found, the station
!         is not chased.


        IF (CHECK) THEN

! Add station to list of those to be checked

          STNLIST=STNLIST+1
          CHASE_STNID(STNLIST)=INT(DATA_ARRAY(CURRENT_STN,1))


          ELEM=8              ! First SYNOP reporting-practice element.

          DO WHILE (ELEM .LE. 48)        ! Elements loop

!           Reporting practice flags (10 * 4)
            IF (DATA_ARRAY (CURRENT_STN,ELEM).GT.0) THEN

              FLAG=INT (DATA_ARRAY (CURRENT_STN,ELEM))
              CALL BINCONV(FLAG,DAYSTR)

!          The next section applies only to U.K bulletins and checks the
!            station is reporting current element in the correct season.

              IF (BLTNID .EQ. 11) THEN

                IF (     SUMMERTIME .AND. DAYSTR(1:1) .NE. '1' .OR.
     &              .NOT.SUMMERTIME .AND. DAYSTR(2:2) .NE. '1')
     &                                                 CHECK=.FALSE.
              ENDIF
            ELSE
              CHECK=.FALSE.   ! No reporting practices given.
            ENDIF


!           This next section checks the location of each station,
!             by country, to determine if it is affected by a public
!             holiday, should the public holiday flag be set.
!           Flag table DAYS holds data determining whether a report
!             is expected on a public holiday.

            IF (PUBLIC .AND. CHECK) THEN

              IF (INDEX (HOLIDAY,CSTR(CURRENT_STN)) .NE. 0) THEN

                IF (DAYSTR(3:3) .NE. '1')  CHECK=.FALSE.
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

! SYNOP practice

!               Get flags appropriate to SYNOP am hours
              FLAG=INT (DATA_ARRAY (CURRENT_STN, ELEM +1+0))
              CALL BINCONV (FLAG,SYNTIME)
              STNPRAC(STNLIST)(1:12)=SYNTIME

!               Get flags appropriate to SYNOP pm hours
              FLAG=INT (DATA_ARRAY (CURRENT_STN, ELEM +1+1))
              CALL BINCONV (FLAG,SYNTIME)
              STNPRAC(STNLIST)(13:24)=SYNTIME

! SREW practice

              FLAG=INT(DATA_ARRAY(CURRENT_STN,5)) ! Synop elem.flags
              CALL BINCONV (FLAG,SYNSTR)

              STNPRAC(STNLIST)(25:25)=SYNSTR(13:13)     !SREW flag

! NCM practice

              FLAG=INT(DATA_ARRAY(CURRENT_STN,ELEM+3))
              CALL BINCONV(FLAG,NCMTIME)
              STNPRAC(STNLIST)(26:27)=NCMTIME

c##     IF (BLTNID .EQ. 13)
c##    print*,' chsdst ',DATA_ARRAY (CURRENT_STN,1),' practice found >>'
c##  &       ,STNPRAC(STNLIST),'<<',syntime,'<>',ncmtime
!           If the reporting practise did not contain details about
!             the hour being chased, check the next reporting practise.

              ELEM=49    !Appropriate practice found. Stop loop
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
