      SUBROUTINE CHSRPS(CHASE_DATIM, BLTNID , REPORT_TYPE,
     &                   CHASE_STNID, STNDATA, STN_MATCH  , STNLIST,
     &                   CHECK_STNID,
     &                   MISNG_RPRTS, MISRCNT,
     &                   MISNG_ELEMS, MISECNT)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : CHSRPS                                             
!                                                                      
! PURPOSE        : To create a list of station identifiers and any     
!                  missing or suspect elements, if applicable, by      
!                  using a list of station identifiers and expected    
!                  elements.                                           
!                                                                      
! DESCRIPTION    : A list of station identifiers and elements expected 
!                  is used to call the MDB datasets in order to        
!                  establish all those stations whose reports have not 
!                  yet been received or have been received with data   
!                  missing or suspect.                                 
!                  New lists are created to hold the lists of station  
!                  identifiers whose reports are missing or have       
!                  missing or suspect elements.                        
!                                                                      
! DATA TYPE(S)   : SYNOPTIC, SREW, NCM AND UPPER AIR                   
! HANDLED                                                             
!                                                                      
! CALLED BY      : CHSMAIN                                             
!                                                                      
! CALLS          : MDB                                                 
!                                                                      
! PARAMETERS     : IN(I) OUT(O)                                        
!                                                                      
!                (I)  CHASE_DATIM - Date/Time of hour being chased     
!                (I)  BLTNID      - Whether UK, Germany or Gib/Cyprus. 
!                (I)  REPORT_TYPE - The Data-type and Bulletin-type    
!                                   identifier                         
!                (I)  CHASE_STNID - List of station identifiers to be  
!                                   chased                             
!                (IO) STNDATA     - List of station elements expected  
!                (O)  STN_MATCH   - Whether station report found here. 
!                (I)  STNLIST     - The number of stations to be chased
!                (I)  CHECK_STNID - Blank array associated with CHASE_ 
!                                   passed from Main only to allow     
!                                   variable dimensioning here - with a
!                                   view to future maintenance.        
!                (O)  MISNG_RPRTS - List of stations whose             
!                                   reports are missing.               
!                (O)  MISRCNT     - Number of stations in MISNG_RPRTS  
!                (O)  MISNG_ELEMS - List of stations with missing      
!                                   or suspect elements.               
!                (O)  MISECNT     - Number of stations in MISNG_ELEMS  
!                                                                      
! REVISION INFO  :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:48$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chsrps.F,v $
!
! CHANGE RECORD  :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:48    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:34  usmdb
! Correct HEAD. Moved data statements before executable code.
! Added copyright and modified header - S.Cox
!
! Revision 1.2  98/09/30  09:53:27  09:53:27  usmdb (Generic MDB account)
! Correct problem with chasing main clod group when sky obscured.
! Reorder definition of variables.
! v(G)=65, ev(G)= 1
! 
! Revision 1.1  98/09/08  16:11:36  16:11:36  usmdb (Generic MDB account
! Initial revision
!
! 05/09/1998      Remove chasing cloud values if station type is      !c  
!                 automatic.                                          
!
! 30/07/1998      Only test NCM fresh snow depth for error value.     !b  
!                                                                      
! 01/07/1998      Tests added for non-reported SYNOP values.          !a  
!                                                                      
! 23/03/1998      NCM reports for preferred version requested.        
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

! Declare arguments :

      INTEGER       CHASE_DATIM(*)! Date/time of chase.
      INTEGER       BLTNID        ! Determines country.
      INTEGER       REPORT_TYPE   ! Determines the data type to check.

      INTEGER       CHASE_STNID(*)! List of stations for which reports
                                  !  are expected for this hour.
      LOGICAL       STN_MATCH(*)  ! Whether station found in data retr-
                                  !  ieved here.
      INTEGER       STNLIST       ! Number of stations in CHASE_STNID
      CHARACTER*(*) STNDATA(*)    ! Array of expected elements for
                                  ! the stations in CHASE_STNID
      INTEGER       CHECK_STNID(*)! Copy of CHASE_STNID values with
                                  !  WMO block number removed.

      CHARACTER*(*) MISNG_RPRTS(*)! Array of station identifiers whose
                                  ! reports have not yet been received
                                  ! in the databanks.
      INTEGER       MISRCNT       ! Count of number of missing reports.
      CHARACTER*(*) MISNG_ELEMS(*)! Array of station identifiers and
                                  ! abbreviations of missing elements as
                                  ! defined by flag tables SYNELM,
                                  ! NCMEL1 and NCMEL2.
      INTEGER       MISECNT       ! Count of number of reports with
                                  ! missing elements.
!     Closely related working variable.
      INTEGER       FLGEPOS       ! Pointer within string MISNG_ELEMS
                                  !   row to where next synonyn is to
                                  !   begin.

! Declare MDB retrieval variables.

      CHARACTER*4   YEAR          ! The year ...
      CHARACTER*2   MONTH         ! ... month ...
      CHARACTER*2   DAY           ! ... day and ...
      CHARACTER*6   HOUR          ! ... hour of chase.

      CHARACTER*8   MDB_SUB_TYPE  ! The data type being chased.
      CHARACTER*500 REQUEST_STR   ! The MDB call request string.
      REAL          SYNARY(64)    ! Synop data array for MDB call.
      REAL          SRWARY(1)     ! Srew data array for MDB call.
      REAL          NCMARY(26)    ! NCM data array for MDB call.

      CHARACTER*50  REQUEST_TIME  ! Used when ...
      CHARACTER*60  REQUEST_AREA  ! ... constructing ...
      INTEGER       AREA_LENGTH   !        ... SYNOP request.

      INTEGER       NOBS          ! The number of observations requested
      INTEGER       NELEM         ! The number of ELEMENTs in the
                                  ! MDB request.
      INTEGER       ISTAT         ! Return value from the MDB call.
      CHARACTER*13  CSTR          ! MDB call parameter not used.
      CHARACTER*500 CREP          ! Retrieve report text.
      INTEGER       REPORT_LEN    ! Length of ditto.


! Declare working variables.

      INTEGER       ELS_IN_GROUP  ! The number of group abbreviations.
                                  ! defined in flag tables SYNELM,
                                  ! NCMEL1,2 or 3.

      INTEGER       ELEMENT       ! The NELEM element being checked.

      INTEGER       EXIT          ! Flag to determine when to leave
                                  !  stations loop.
      INTEGER       CHASE_LIST_PTR ! When checking CHASE_STNID values.

      INTEGER       NCMDATA(23)   ! Pointer to NCM synonyms table ...
      INTEGER       NCMDISP(23)   ! ... and position in table where
                                  !  required synonym resides.

      INTEGER       LOOPFLG       ! Loop for checking flags in multi-
                                  !  value Synop elements
      INTEGER       LOOPGRP       ! Loop for checking GROUP values.
      INTEGER       LOOPINI       ! Loop for initialising ARRAY.

      INTEGER       STNS_MATCHED  ! Count stations checked in Chase list
      INTEGER       SYNGRP(12)    ! Counts of how many Elements have a
                                  !  common abbreviation as defined in
                                  !  flag table SYNELM. Used only to
                                  !  skip through SYNARY when an element
                                  !  is not expected in a report.

      CHARACTER*3   NCMELM1(9)    ! Array of abbreviations as defined by
                                  ! flag tables NCMEL1/2.
      CHARACTER*4   NCMELM2(12)   ! Array of abbreviations as defined by
                                  ! flag tables NCMEL1/2.
      CHARACTER*5   NCMELM3(2)    ! Array of abbreviations as defined by
                                  ! flag tables NCMEL1/2.
      CHARACTER*6   NCMELM4       ! Array of abbreviations as defined by
                                  ! flag tables NCMEL1/2.
      CHARACTER*3   SYNELM(12)    ! Array of abbreviations used with
                                  ! array SYNGRP.

      LOGICAL       ADD_TO_MISE   ! ADD TO MISsing Element string -
                                  !   used in Synop element tests.
                                  ! for more than one missing element.
      LOGICAL       OFFSET        ! Whether Missing-elements message
                                  !  requires 2 lines.
      LOGICAL       STNID_MATCHED ! Whether StnId of retrieved report
                                  !  found in "chase" list. Triggers
                                  !  elements checks.
      LOGICAL       SREW_STN_MATCHED ! Whether StnId of retrieved SREW
                                  !  found in "chase" list.  Does not
                                  !  trigger elements check.
      CHARACTER*132  HEAD

      DATA SYNELM /'VV,' ,'N, ' ,'DD,' ,'FF,' ,'T, ' ,'TD,' ,
     &             'P, ' ,'WW,' ,'WP,' ,'MC,' ,'EE,' ,'8, '  /
      DATA SYNGRP /1,1,1,1,1,1,1,1,2,5,1,12/

!     Note that arrays NCMELM1/2/3, while based on flag tables NCMEL1/2,
!      should not be considered identical to them - they are simply
!      stores of element synonyms, designed to avoid spaces in ultimate
!      message.  The order in which NCM elements are considered is dic-
!      tated by the order of the flags and the arrays NCMDATA and
!      NCMDISP provide the array/element position of each synonym.

      DATA NCMELM1/'TG,' ,'TC,' ,'EC,' ,'DH,' ,'DT,' ,'DF,' ,
     &             'DG,' ,'DS,' ,'SD,' /
      DATA NCMELM2/'5TX,','5TN,','5RT,','SUN,','T30,','FSD,',
     &             '4TX,','4TN,','4RT,','6TX,','6TN,','6RT,'/
      DATA NCMELM3/'E/E",','T100,'/
      DATA NCMELM4/'SYNTAX'/
      DATA NCMDATA/1,1,3,1,2,2,2,2,2,3,1, 1,1,1,1,1,2,2,2,2, 2, 2, 2/
      DATA NCMDISP/1,2,1,3,1,2,3,4,5,2,4, 5,6,7,8,9,6,7,8,9,10,11,12/

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/chsrps.F,v $
     &'//'$Date: 30/01/2006 20:21:48$ $Revision: 1$'                                   !2.0

! Initialise variables

      REQUEST_STR=' '  ! MDB retrieval parameters.
      NOBS=1
      NELEM=1
      ISTAT=0
      CSTR=' '
      CREP=' '

      EXIT=0           ! Station-loop exit flag
      ELS_IN_GROUP=1
      STNS_MATCHED=0
      STNID_MATCHED=.FALSE.
      CHASE_LIST_PTR=1

      DO LOOPINI=1,STNLIST

!       Array of "station-found" flags.
        STN_MATCH(LOOPINI)=.FALSE.

!       Copy CHASE Ids to SYNOP check array without WMO block numbers.
        CHECK_STNID(LOOPINI)=MOD(CHASE_STNID(LOOPINI),1000)
      ENDDO

      MISRCNT=1        ! Start count of missing reports ...
      MISECNT=1        ! ... and missing or suspect elements.

!     Setup REQUEST_STR common to all data types for
!       date and time to be chased.

      WRITE (YEAR ,'(  I4.4  )')     CHASE_DATIM(2)
      WRITE (MONTH,'(  I2.2  )')     CHASE_DATIM(3)
      WRITE (DAY  ,'(  I2.2  )')     CHASE_DATIM(4)
      WRITE (HOUR ,'(A,I2.2,A)') '/',CHASE_DATIM(5),'00Z'

      REQUEST_TIME='START TIME '//YEAR//MONTH//DAY//HOUR//
     &              ' END TIME '//YEAR//MONTH//DAY//HOUR//' '


!     Append REQUEST_STR data for individual subtypes.

      IF (REPORT_TYPE .EQ. 1) THEN

        MDB_SUB_TYPE='LNDSYN  '

        IF (BLTNID .EQ. 1) THEN

           REQUEST_AREA='VERSION PREFERRED PLATFORM 03'
          AREA_LENGTH=29

        ELSEIF (BLTNID .EQ. 2) THEN

          REQUEST_AREA='VERSION PREFERRED PLATFORM '//
     &                 '10320 10328 10401 10402 10405 '
          AREA_LENGTH=57

        ELSEIF (BLTNID .EQ. 3) THEN

           REQUEST_AREA='VERSION PREFERRED PLATFORM 08495 17601'
          AREA_LENGTH=38
        ENDIF

        REQUEST_STR=REQUEST_TIME // REQUEST_AREA(1:AREA_LENGTH) //
     &  ' ELEMENTS '      //
     &  'WMO_STTN_NMBR '  //
     &  'HRZL_VSBLY '     //'TOTL_CLOD_AMNT ' //
     &  'SRFC_WIND_DRCTN SRFC_WIND_SPED '     //
     &  'SRFC_AIR_TMPR   SRFC_DEW_PONT_TMPR ' //
     &  'MSL_PESR '       //
     &  'CRNT_WTHR_TYPE  PRMY_PAST_WTHR_TYPE SCNY_PAST_WTHR_TYPE '//
     &  'CLOUD '          //     ! 5 elements - Main cloud group.
     &  'GRND_STAT_IDNY ' //
     &  'CLOUD_LAYER*4 '  //     ! 4 * 3 elements - Amount-type-height.
     &  'STTN_RPRT_TYPE ' //     ! station report type
     &  'RPRT_TEXT '      //     ! Character text for report.
     &  '+QC_FLAGS '             ! Appears BEFORE Met value !

        ELS_IN_GROUP=12
        NELEM=64

      ELSEIF (REPORT_TYPE.EQ. 2) THEN

        MDB_SUB_TYPE='SREW    '
        REQUEST_STR=REQUEST_TIME //
     &                        'PLATFORM 03 ELEMENTS WMO_STTN_INDX_NMBR'
        NELEM=1

      ELSEIF (REPORT_TYPE.EQ. 3) THEN

        MDB_SUB_TYPE='NCM     '
        REQUEST_STR=REQUEST_TIME //
     &   'PLATFORM 03 VERSION PREFERRED ' //
     &   'ELEMENTS WMO_STTN_INDX_NMBR ERROR_FLAGS RPRT_TEXT'
        ELS_IN_GROUP=23
        NELEM=26
      ENDIF


      DO WHILE (EXIT .EQ. 0)

!       Initialise the return data array to 'missing' data
!         for the relevant data type.



        IF (REPORT_TYPE.EQ. 1) THEN

          DO LOOPINI=1,NELEM

            SYNARY(LOOPINI)=-9999999
          ENDDO


          CALL MDB(MDB_SUB_TYPE, REQUEST_STR,
     &             SYNARY      , NOBS       ,
     &             NELEM       , ISTAT      ,CSTR, CREP)
          REPORT_LEN=SYNARY(62)        ! Length in CREP.

        ELSEIF (REPORT_TYPE.EQ. 2) THEN

          SRWARY(NELEM)=-9999999       !nelem is 1

          CALL MDB(MDB_SUB_TYPE, REQUEST_STR,
     &             SRWARY      , NOBS       ,
     &             NELEM       , ISTAT      ,CSTR, CREP)


        ELSEIF (REPORT_TYPE.EQ. 3) THEN

          DO LOOPINI=1,NELEM

            NCMARY(LOOPINI)=-9999999
          ENDDO

          CALL MDB(MDB_SUB_TYPE, REQUEST_STR,
     &             NCMARY      , NOBS       ,
     &             NELEM       , ISTAT      ,CSTR, CREP)
          REPORT_LEN=NCMARY(25)
        ENDIF



!       If an ISTAT value of 8 is returned, then there are no data
!        available to satisfy the request.  Assume all reports for
!        "chasable" stations are missing and end the call to the MDB
!        for this data type.

        IF (ISTAT .EQ. 8) THEN

          DO CHASE_LIST_PTR=1,STNLIST

            WRITE (MISNG_RPRTS(MISRCNT)(1:6),'(I5.5)')
     &                                      CHASE_STNID(CHASE_LIST_PTR)
!*** Reinstate this line if a list of missing stations and the elements
!           expected is to be held in a dataset. ***
!***        MISNG_RPRTS(CHASE_LIST_PTR)(7:29)=STNDATA(CHASE_LIST_PTR)
          ENDDO
          MISRCNT=STNLIST+1
          EXIT=1

!       If ISTAT is 4 or 0, there are data available.

        ELSEIF (ISTAT .LE. 4) THEN

!         Reset variables used in element checking section.

          ELEMENT=4        ! First element in retrieved report
                           ! (Met value appears AFTER QC flag).
          FLGEPOS=7        ! First free position in mis-els string.
          OFFSET=.FALSE.
          STNID_MATCHED=.FALSE.
          CHASE_LIST_PTR=0

!         If all the stations to be chased have been checked or all
!          reports available have been retrieved, then exit.

          IF (STNS_MATCHED.EQ.STNLIST .OR. ISTAT.EQ.0)  EXIT=1


!         Check synop data. Compare the station identifier retrieved
!           with the whole of the list of stations to be checked as we
!           cannot be certain that the Met data will be retrieved in the
!           same station order as the Station Master details.
!         If there is a match then a report has been received and the
!           elments of that report must be checked. Mark any matched
!           list station; this check list is used later to determine
!           which stations have not reported and so should be chased.
!         If the station identifier retrieved is not on the list, skip
!           checks and retrieve another station's report.          '

! *** Reinstate this line if a list of missing stations and the elements
! expected is to be held in a dataset. ***


          IF (REPORT_TYPE.EQ. 1) THEN
            DO WHILE (CHASE_LIST_PTR .LT. STNLIST
     &                               .AND..NOT. STNID_MATCHED)

              CHASE_LIST_PTR=CHASE_LIST_PTR+1
              IF (CHECK_STNID(CHASE_LIST_PTR).EQ.INT(SYNARY(2))) THEN

                STNS_MATCHED=STNS_MATCHED+1
                STNID_MATCHED=.TRUE.
                STN_MATCH(CHASE_LIST_PTR)=.TRUE.
              ENDIF
            ENDDO
          ELSEIF (REPORT_TYPE.EQ. 2) THEN

!           Check for missing SREW reports as above with the exception
!             of checking elements.
!             Use SREW_STN_MATCHED to control exit from loop.

            SREW_STN_MATCHED=.FALSE.
            DO WHILE (CHASE_LIST_PTR .LT. STNLIST
     &                               .AND..NOT. SREW_STN_MATCHED)

              CHASE_LIST_PTR=CHASE_LIST_PTR+1
              IF (CHASE_STNID(CHASE_LIST_PTR).EQ.INT(SRWARY(1))) THEN

                STNS_MATCHED=STNS_MATCHED+1
                SREW_STN_MATCHED=.TRUE.
                STN_MATCH(CHASE_LIST_PTR)=.TRUE.
              ENDIF
            ENDDO

!          Check for missing NCM reports as explained for SYNOP reports.
! *** Reinstate this line if a list of missing stations and the elements
! expected is to be held in a dataset. ***

          ELSEIF (REPORT_TYPE.EQ. 3) THEN

            DO WHILE (CHASE_LIST_PTR .LT. STNLIST
     &                               .AND..NOT. STNID_MATCHED)

              CHASE_LIST_PTR=CHASE_LIST_PTR+1
              IF (CHASE_STNID(CHASE_LIST_PTR).EQ.INT(NCMARY(1))) THEN

                STNS_MATCHED=STNS_MATCHED+1
                STNID_MATCHED=.TRUE.
                STN_MATCH(CHASE_LIST_PTR)=.TRUE.
              ENDIF
            ENDDO
          ENDIF          ! Station-matching section

!           If an expected report is retrieved, check the elements
!            are correct and there are no syntax errors.

            IF (STNID_MATCHED) THEN


!             Loop the number of elements common to each abbreviation
!               as defined in flag tables SYNELM, NCMEL1 and NCMEL2.

              DO LOOPGRP=1,ELS_IN_GROUP

                ADD_TO_MISE=.FALSE.                                  !c
!               If a meteorological element is expected a bit in the
!                 string of expected elements will be set on.


                IF (STNDATA(CHASE_LIST_PTR)(LOOPGRP:LOOPGRP) .EQ. '1')
     &                                                              THEN

                  IF (REPORT_TYPE.EQ. 1) THEN

!  ELEMENT  1 VISIBILTY          (VV)
!  ELEMENT  2 TOTAL CLOUD AMOUNT (N )
!  ELEMENT  3 WIND DIRECTION     (DD)
!  ELEMENT  4 WIND SPEED         (FF)
!  ELEMENT  5 AIR TEMPERATURE    (T )
!  ELEMENT  6 DEWPOINT           (TD)
!  ELEMENT  7 MSL PRESSURE       (P )
!  ELEMENT  8 PRESENT WEATHER    (WW)
!  ELEMENT  9 PAST WEATHER       (WP)
!  ELEMENT 10 CLOUD              (MC)
!  ELEMENT 11 STATE OF GROUND    (EE)
!  ELEMENT 12 8 GROUPS           (8 )

!                   Check one-value-per-flag elements (VV to WW and EE)
                    IF (LOOPGRP.EQ.2.AND.SYNARY(60).EQ.0              !c
     &                  .AND. BLTNID.EQ.1)THEN                        !c
! TOTAL CLOUD AND UK AUTOMATIC STATION                                !c
! ix value in range 5-7, only chase total cloud if it is missing      !c
                      ADD_TO_MISE=SYNARY(ELEMENT)  .LT.-9999998       !c
                      ELEMENT=ELEMENT+2                               !c

                    ELSEIF (LOOPGRP.EQ.2.AND.SYNARY(60).EQ.0          !c
     &                  .AND. BLTNID.NE.1)THEN                        !c
! TOTAL CLOUD AND OVERSEAS AUTOMATIC STATION                          !c
! ix value in range 5-7, do not chase total cloud                     !c
                      ELEMENT=ELEMENT+2                               !c

                    ELSEIF (LOOPGRP.LE.7) THEN                        !c

                      ADD_TO_MISE=SYNARY(ELEMENT)  .LT.-9999998 .OR.
     &                          SYNARY(ELEMENT-1).EQ.1
                      ELEMENT=ELEMENT+2

                    ELSEIF (LOOPGRP.EQ.8) THEN  ! WW (Present weather)
!  Test if a present or past weather value reported as group is optional
                      IF((SYNARY(18).GT.-9999998 .AND.
     &                    SYNARY(18).LT.508) .OR.
     &                    SYNARY(20).GT.-9999998 .OR.
     &                    SYNARY(22).GT.-9999998 )THEN                !a

                        ADD_TO_MISE=SYNARY(ELEMENT)  .LT.-9999998 .OR.
     &                              SYNARY(ELEMENT-1).EQ.1
                        ELEMENT=ELEMENT+2

                      ENDIF                                           !a

                    ELSEIF (LOOPGRP.EQ.9) THEN  ! WP (Past weather)
!  Test if a present or past weather value reported as group is optional
                      IF((SYNARY(18).GT.-9999998 .AND.
     &                    SYNARY(18).LT.508) .OR.
     &                    SYNARY(20).GT.-9999998 .OR.
     &                    SYNARY(22).GT.-9999998 )THEN                !a

                        ADD_TO_MISE=SYNARY(20).LT.-9999998 .OR.
     &                              SYNARY(22).LT.-9999998 .OR.
     &                              SYNARY(19).EQ.1        .OR.
     &                              SYNARY(21).EQ.1
                        ELEMENT=ELEMENT+4
                      ENDIF                                           !a

                    ELSEIF (LOOPGRP.EQ.11) THEN ! STATE OF GROUND
! Test hour to see if state of ground reported at that hour.
                      IF(SYNARY(34).GT.-9999998 .OR.
     &                   CHASE_DATIM(5).EQ.06 )THEN                   !a
                        ADD_TO_MISE=SYNARY(34)  .LT.-9999998 .OR.
     &                              SYNARY(33-1).EQ.1
                        ELEMENT=ELEMENT+2
                      ENDIF                                           !a

                    ELSEIF (LOOPGRP.EQ.10.AND.SYNARY(60).NE.0)THEN    !c
! MC (MAIN CLOUD GROUP) FOR MANNED STATIONS ONLY
! Test if total cloud amount (N) indicates cloud reported.
                      IF(INT(SYNARY(6)).GE.1.AND.
     &                   INT(SYNARY(6)).LE.8)THEN                 !a,1.2

!                     BOTH NH AND CH ABSENT MEANS THAT GROUP IS MISSING.
                        ADD_TO_MISE=SYNARY(30).LT.-9999998 .AND.
     &                              SYNARY(32).LT.-9999998
                      ENDIF                                           !a

!                      ELSE, CHECK ALL 5 FLAGS station manned
                        LOOPFLG=23
                        DO WHILE (.NOT.ADD_TO_MISE .AND. LOOPFLG.LE.31)

                          ADD_TO_MISE=SYNARY(LOOPFLG).EQ.1
                          LOOPFLG=LOOPFLG+2
                        ENDDO
                      ELEMENT=ELEMENT+10

                    ELSE               ! "8"-groups
! Test if total cloud amount (N) indicates cloud reported.
                      IF(INT(SYNARY(6)).GE.1.AND.
     &                   INT(SYNARY(6)).LE.8)THEN                     !a
!                     Absent first cloud amount proves group(s) missing
                        ADD_TO_MISE=SYNARY(36).LT.-9999998
                      ENDIF                                           !a

!                     Else, check all 12 flags
                      LOOPFLG=35
                      IF(SYNARY(60).NE.0)THEN                         !c
                        DO WHILE (.NOT.ADD_TO_MISE .AND. LOOPFLG.LE.57)

                          ADD_TO_MISE=SYNARY(LOOPFLG).EQ.1
                          LOOPFLG=LOOPFLG+2
                        ENDDO
                      ENDIF                                           !c
                    ENDIF

!                   If at least one element from a group of elements
!                    with a common abbreviation is missing or suspect
!                    add the abbreviation to the list of missing or
!                    suspect elements.

                    IF (ADD_TO_MISE) THEN
                 WRITE(12,'(1X,I2,2X,A)')LOOPGRP,CREP(45:REPORT_LEN)

                      MISNG_ELEMS(MISECNT)(FLGEPOS:FLGEPOS+2)
     &                                                =SYNELM(LOOPGRP)
                      FLGEPOS=FLGEPOS+3
                    ELSE

! *** Reinstate this line if a list of missing stations and the elements
! expected is to be held in a dataset. ***
!                    If the element has been received then change the
!                      appropriate bit in the elements-expedted string
!                      to reflect this.

! ***                 WRITE(STNDATA(CHASE_LIST_PTR)(LOOPGRP:LOOPGRP),
! ***&                                                        '(A1)')'0'
                    ENDIF
                  ELSEIF (REPORT_TYPE.EQ. 3) THEN

!                 If NCM data then use NCMEL1 and NCMEL2 for
!                  abbreviations of missing or suspect elements.

                    IF (CHASE_DATIM(5).EQ.21 .AND. LOOPGRP.GE.18
     &                                       .AND. LOOPGRP.LE.20) THEN

                      IF (NCMARY(LOOPGRP+1) .EQ. -9999999 .OR.
     &                    NCMARY(LOOPGRP+1) .EQ. 1) THEN
                        MISNG_ELEMS(MISECNT)(FLGEPOS:FLGEPOS+3)
     &                                    =NCMELM2(NCMDISP(LOOPGRP))
                        FLGEPOS=FLGEPOS+4
                      ELSE
! ***                   WRITE(STNDATA(CHASE_LIST_PTR)(LOOPGRP:LOOPGRP),
! ***&                                                        '(A1)')'0'
                      ENDIF
                    ELSEIF (CHASE_DATIM(5).EQ.09 .AND.
     &                     (LOOPGRP.LE.17 .OR. LOOPGRP.GE.21)) THEN

                      IF ((LOOPGRP .NE. 17 .AND.                     !b
     &                    NCMARY(LOOPGRP+1) .LT. -9999998 .OR.       !b
     &                    NCMARY(LOOPGRP+1) .EQ. 1) .or.             !b
     &                    (LOOPGRP .EQ. 17 .AND.                     !b
     &                    NCMARY(LOOPGRP+1) .EQ. 1)) THEN            !b


                        IF (NCMDATA(LOOPGRP) .EQ. 1) THEN

                          IF (FLGEPOS .GT. 64) THEN

                            FLGEPOS=7
                            OFFSET=.TRUE.
                            MISECNT=MISECNT+1
                          ENDIF
                          MISNG_ELEMS(MISECNT)(FLGEPOS:FLGEPOS+2)
     &                                    =NCMELM1(NCMDISP(LOOPGRP))
                          FLGEPOS=FLGEPOS+3

                        ELSEIF (NCMDATA(LOOPGRP) .EQ. 2) THEN

                          IF (FLGEPOS .GT. 63) THEN
                            FLGEPOS=7
                            OFFSET=.TRUE.
                            MISECNT=MISECNT+1
                          ENDIF
                          MISNG_ELEMS(MISECNT)(FLGEPOS:FLGEPOS+3)
     &                                      =NCMELM2(NCMDISP(LOOPGRP))
                          FLGEPOS=FLGEPOS+4

                        ELSEIF (NCMDATA(LOOPGRP) .EQ. 3) THEN

                          IF (FLGEPOS .GT. 62) THEN

                            FLGEPOS=7
                            OFFSET=.TRUE.
                            MISECNT=MISECNT+1
                          ENDIF
                          MISNG_ELEMS(MISECNT)(FLGEPOS:FLGEPOS+4)
     &                                      =NCMELM3(NCMDISP(LOOPGRP))
                          FLGEPOS=FLGEPOS+5
                        ENDIF
                      ELSE
!***                    STNDATA(CHASE_LIST_PTR)(LOOPGRP:LOOPGRP)='0'
                      ENDIF
                    ENDIF

!                  If the element has been received then change the
!                    appropriate bit in the elements expected string
!                    to reflect this..

                    IF (LOOPGRP .EQ. ELS_IN_GROUP) THEN

                      IF (NCMARY(NELEM) .EQ. 1) THEN

                        MISNG_ELEMS(MISECNT)(FLGEPOS:FLGEPOS+5)=NCMELM4
                        FLGEPOS=FLGEPOS+6
                      ENDIF
                    ENDIF
                  ENDIF
                ELSE
                  IF (REPORT_TYPE.EQ. 1) THEN

                    ELEMENT=ELEMENT+SYNGRP(LOOPGRP)*2
                  ENDIF
                ENDIF
              ENDDO    ! Eleents loop

!             By checking the value of FLGEPOS it can be established
!              if any elements are missing. (The position within the
!              MISNG_ELEMS string will be higher than the initial
!              value). If any elements are missing then the station
!              identifier is added to the start of the string.

              IF (FLGEPOS .GT. 7) THEN
                IF (OFFSET) THEN

                 WRITE(12,'(1X,A)')CREP(45:REPORT_LEN)
!                 2-line message, add StnId to first line.

                  WRITE (MISNG_ELEMS(MISECNT-1)(1:6),'(I5.5)')
     &                                       CHASE_STNID(CHASE_LIST_PTR)
                  MISNG_ELEMS(MISECNT)(FLGEPOS-1:66)=' '
                  MISNG_ELEMS(MISECNT-1)(67:89)=STNDATA(CHASE_LIST_PTR)
                  MISECNT=MISECNT+1

                ELSE

!                 Add StnId to single-line message.

                  WRITE (MISNG_ELEMS(MISECNT)(1:6),'(I5.5)')
     &                                       CHASE_STNID(CHASE_LIST_PTR)
                  MISNG_ELEMS(MISECNT)(FLGEPOS-1:66)=' '
                  MISNG_ELEMS(MISECNT)(67:89)=STNDATA(CHASE_LIST_PTR)
                  MISECNT=MISECNT+1
                ENDIF

!!!!!!!!!!!!!!! Add report text after message (max 66 chars per line).
!               The report is held slightly differently for Synops and
!                 NCMs in that the admin info. is of different length
!                 (also remove WMO block number in each case) and
!                 whether the terminating "=" is stored.
!
!               IF (REPORT_TYPE.EQ. 1) THEN
!
!                 IF(REPORT_LEN .LE. 112) THEN      ! Synops
!
!                   MISNG_ELEMS(MISECNT)=CREP(47:REPORT_LEN) //' ='
!                   MISECNT=MISECNT+1
!                 ELSE
!                   MISNG_ELEMS(MISECNT)=CREP(47:112)
!                   MISECNT=MISECNT+1
!                   MISNG_ELEMS(MISECNT)=CREP(113:REPORT_LEN) //' ='
!                   MISECNT=MISECNT+1
!                 ENDIF
!
!               ELSEIF (REPORT_TYPE.EQ. 3) THEN
!
!                 IF(REPORT_LEN .LE. 110) THEN      ! NCMs
!
!                   MISNG_ELEMS(MISECNT)=CREP(45:REPORT_LEN)
!                   MISECNT=MISECNT+1
!                 ELSE
!                   MISNG_ELEMS(MISECNT)=CREP(45:110)
!                   MISECNT=MISECNT+1
!                   MISNG_ELEMS(MISECNT)=CREP(111:REPORT_LEN)
!                   MISECNT=MISECNT+1
!                 ENDIF
!               ENDIF
!!!!!!!!!!!!!!!
              ENDIF
            ENDIF
!         ENDIF
        ENDIF
      ENDDO       ! "EXIT" loop - ie till we run out of data !


!     Check to see if any expected reports are missing and, if so,
!      copy station Id to "missing reports" list.

      DO  CHASE_LIST_PTR=1,STNLIST

        IF (.NOT.STN_MATCH(CHASE_LIST_PTR)) THEN

              WRITE (MISNG_RPRTS(MISRCNT)(1:6),'(I5.5)')
     &                                      CHASE_STNID(CHASE_LIST_PTR)
          MISRCNT=MISRCNT+1
        ENDIF
      ENDDO


!     All the array counts are reduced by one to give the true number of
!      reports missing, suspect or contain missing data.

      MISRCNT=MISRCNT-1
      MISECNT=MISECNT-1

      RETURN
      END

