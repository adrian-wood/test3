      SUBROUTINE CHSDRP(CHASE_DATIM, BLTNID , REPORT_TYPE,
     &                   CHASE_STNID, STNPRAC, STN_MATCH  , STNLIST,
     &                   CHECK_STNID,
     &                   MISNG_RPRTS, MISRCNT)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : CHSDRP                                             
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
!                (IO) STNPRAC     - Station daily practice             
!                (O)  STN_MATCH   - Whether station report found here. 
!                (I)  STNLIST     - The number of stations to be chased
!                (I)  CHECK_STNID - Blank array associated with CHASE_ 
!                                   passed from Main only to allow     
!                                   variable dimensioning here - with a
!                                   view to future maintenance.        
!                (O)  MISNG_RPRTS - List of stations whose             
!                                   reports are missing.               
!                (O)  MISRCNT     - Number of stations in MISNG_RPRTS  
!                                                                      
! REVISION INFO  :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:44$
! $Source: /home/us0400/mdb/op/lib/source/RCS/chsdrp.F,v $
!
! CHANGE RECORD  :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:44    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:32  usmdb
! Correct HEAD, added copyright and modified header - S.Cox
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
      CHARACTER*(*) STNPRAC(*)    ! Array of expected elements for
                                  ! the stations in CHASE_STNID
      INTEGER       CHECK_STNID(*)! Copy of CHASE_STNID values with
                                  !  WMO block number removed.

      CHARACTER*(*) MISNG_RPRTS(*)! Array of station identifiers whose
                                  ! reports have not yet been received
                                  ! in the databanks.
      INTEGER       MISRCNT       ! Count of number of missing reports.

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

      INTEGER       hourp1        ! hour plus one
      INTEGER       NOBS          ! The number of observations requested
      INTEGER       NELEM         ! The number of ELEMENTs in the
                                  ! MDB request.
      INTEGER       ISTAT         ! Return value from the MDB call.
      CHARACTER*13  CSTR          ! MDB call parameter not used.
      CHARACTER*500 CREP          ! Retrieve report text.
      INTEGER       REPORT_LEN    ! Length of ditto.

! Declare working variables.

      INTEGER       CHASE_LIST_PTR ! When checking CHASE_STNID values.

      INTEGER       ELS_IN_GROUP  ! The number of group abbreviations.
                                  ! defined in flag tables SYNELM,
                                  ! NCMEL1,2 or 3.

      INTEGER       ELEMENT       ! The NELEM element being checked.

      INTEGER       EXIT          ! Flag to determine when to leave
                                  !  stations loop.
      INTEGER       LOOPINI       ! Loop for initialising ARRAY.

      INTEGER       STNS_MATCHED  ! Count stations checked in Chase list

      LOGICAL       STNID_MATCHED ! Whether StnId of retrieved report
                                  !  found in "chase" list. Triggers
                                  !  elements checks.
      LOGICAL       SREW_STN_MATCHED ! Whether StnId of retrieved SREW
                                  !  found in "chase" list.  Does not
                                  !  trigger elements check.
      CHARACTER*132  HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/chsdrp.F,v $
     &'//'$Date: 30/01/2006 20:21:44$ $Revision: 1$'                                   !2.0

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



!     Setup REQUEST_STR common to all data types for
!       date and time to be chased.

      WRITE (YEAR ,'(  I4.4  )')     CHASE_DATIM(2)
      WRITE (MONTH,'(  I2.2  )')     CHASE_DATIM(3)
      WRITE (DAY  ,'(  I2.2  )')     CHASE_DATIM(4)
      WRITE (HOUR ,'(A,I2.2,A)') '/',CHASE_DATIM(5),'00Z'
      HOURP1=CHASE_DATIM(5)+1

      REQUEST_TIME='START TIME '//YEAR//MONTH//DAY//HOUR//
     &              ' END TIME '//YEAR//MONTH//DAY//HOUR//' '


!     Append REQUEST_STR data for individual subtypes.

      IF (REPORT_TYPE .EQ. 1) THEN

        MDB_SUB_TYPE='LNDSYN  '

        IF (BLTNID .EQ. 11) THEN

           REQUEST_AREA='VERSION PREFERRED PLATFORM 03'
          AREA_LENGTH=29

        ELSEIF (BLTNID .EQ. 12) THEN

          REQUEST_AREA='VERSION PREFERRED PLATFORM '//
     &                 '10320 10328 10401 10402 10405 '
          AREA_LENGTH=57

        ELSEIF (BLTNID .EQ. 13) THEN

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
              IF(.NOT.STN_MATCH(CHASE_LIST_PTR))THEN
                IF(STNPRAC(CHASE_LIST_PTR)(HOURP1:HOURP1).NE.'1')THEN
! SYNOP report not expected from station for this hour
                  STNS_MATCHED=STNS_MATCHED+1
                  STN_MATCH(CHASE_LIST_PTR)=.TRUE.
                ELSEIF(CHECK_STNID(CHASE_LIST_PTR).EQ.
     &                                       INT(SYNARY(2))) THEN
! SYNOP report expected from station for this hour
                  IF(STNPRAC(CHASE_LIST_PTR)(HOURP1:HOURP1).EQ.'1')THEN
! SYNOP report expected from station for this hour and reported
                    STNS_MATCHED=STNS_MATCHED+1
                    STNID_MATCHED=.TRUE.
                    STN_MATCH(CHASE_LIST_PTR)=.TRUE.
                  ELSE
! SYNOP report expected from station for this hour and not received
                    STNID_MATCHED=.TRUE.
                  ENDIF
                ENDIF
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
              IF(.NOT.STN_MATCH(CHASE_LIST_PTR))THEN
                IF(.NOT.((STNPRAC(CHASE_LIST_PTR)(25:25).EQ.'1') .AND.
     &             (STNPRAC(CHASE_LIST_PTR)(HOURP1:HOURP1).EQ.'1')))THEN
! SREW report not expected from station
                  STNS_MATCHED=STNS_MATCHED+1
                  STN_MATCH(CHASE_LIST_PTR)=.TRUE.
                ELSEIF (CHASE_STNID(CHASE_LIST_PTR).EQ.INT(SRWARY(1)))
     &                                                             THEN
! SREW report from expected station
                  IF(STNPRAC(CHASE_LIST_PTR)(HOURP1:HOURP1).EQ.'1'.and.
     &               STNPRAC(CHASE_LIST_PTR)(25:25).EQ.'1')then
! SREW report expected from station for this hour and received
                    STNS_MATCHED=STNS_MATCHED+1
                    SREW_STN_MATCHED=.TRUE.
                    STN_MATCH(CHASE_LIST_PTR)=.TRUE.
                  ELSE
! SREW report expected from station for this hour and not received
                    SREW_STN_MATCHED=.TRUE.
                  ENDIF
                ENDIF
              ENDIF
            ENDDO

!          Check for missing NCM reports as explained for SYNOP reports.
! *** Reinstate this line if a list of missing stations and the elements
! expected is to be held in a dataset. ***

          ELSEIF (REPORT_TYPE.EQ. 3) THEN

            DO WHILE (CHASE_LIST_PTR .LT. STNLIST
     &                               .AND..NOT. STNID_MATCHED)

              CHASE_LIST_PTR=CHASE_LIST_PTR+1
              IF(.NOT.STN_MATCH(CHASE_LIST_PTR))then
                IF(.NOT.((STNPRAC(CHASE_LIST_PTR)(26:26).EQ.'1'
     &                         .AND.CHASE_DATIM(5).EQ.09).OR.
     &                   (STNPRAC(CHASE_LIST_PTR)(27:27).EQ.'1'
     &                              .AND.CHASE_DATIM(5).EQ.21)))THEN
! NCM report not expected from station
                  STNS_MATCHED=STNS_MATCHED+1
                  STN_MATCH(CHASE_LIST_PTR)=.TRUE.
                ELSEIF(CHASE_STNID(CHASE_LIST_PTR).EQ.INT(NCMARY(1)))
     &                                                          THEN
! NCM report expected from station
                  IF((STNPRAC(CHASE_LIST_PTR)(26:26).EQ.'1'
     &                         .AND.CHASE_DATIM(5).EQ.09).OR.
     &               (STNPRAC(CHASE_LIST_PTR)(27:27).EQ.'1'
     &                              .AND.CHASE_DATIM(5).EQ.21))THEN
! NCM report expected from station for this hour and received
                    STNS_MATCHED=STNS_MATCHED+1
                    STNID_MATCHED=.TRUE.
                    STN_MATCH(CHASE_LIST_PTR)=.TRUE.
                  ELSE
! NCM report expected from station for this hour and not received
                    STNID_MATCHED=.TRUE.
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDIF          ! Station-matching section

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

      RETURN
      END

