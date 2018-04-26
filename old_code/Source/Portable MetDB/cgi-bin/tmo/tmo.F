      PROGRAM TMO

!-----------------------------------------------------------------------
!
! ROUTINE       : tmo
!
! DESCRIPTION   : To retrieve the latest METDB_TMO_HOURSBACK hours
!               : LNDSYN data for a single SYNOP station. Data for
!               : several common elements are written to a file for
!               : time series plotting.
!
! CALLED BY     : tmo.pl (cgi script)
!
! CALLS         : DATIM         : Get current time
!               : DT2HRS        : Convert date/time to century hours
!               : HRS2DT        : Convert century hours to date/time
!               : MDB           : Retrieve observations
!               : METDB_GETENV  : get environment variable
!
! VARIABLES     : 3 environment variables (set in tmo.pl)
!               :
!               : METDB_TMO_STATION   - station ID (QC'd in tmo.pl)
!               : METDB_TMO_HOURSBACK - time series period (QC'd in tmo.pl)
!               : METDB_TMO_DATFILE   - output file for data
!                 
! REVISION INFO :
!
! $Revision: 1$
! $Source: /home/us0400/httpd/cgi-bin/PortMetDB/tmo/RCS/tmo.F,v $
! $Date: 14/09/2006 15:44:53$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         14/09/2006 15:44:53    Stan Kellett    
! $
! Revision 1.13  2001/04/10 13:39:39  usmdb
! *** empty log message ***
!
! Revision 1.12  2001/04/09 11:42:50  usmdb
! *** empty log message ***
!
! Revision 1.11  2001/04/06 13:05:53  usmdb
! *** empty log message ***
!
! Written 21-03-2001 Simon Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      IMPLICIT NONE

!-----------------------------------------------------------------------
! Parameter statements
!-----------------------------------------------------------------------                                                                                

      INTEGER IELS                !- Number of elements per ob
      INTEGER IOBS                !- Number of obs per MDB call

      PARAMETER (IELS=50)                                                  
      PARAMETER (IOBS=2000)                                                

!-----------------------------------------------------------------------
! Declare integer variables
!-----------------------------------------------------------------------                                                                                
                                                                                
      INTEGER BLANK                   !- position in string of ' '
      INTEGER CLOD_AMNT(4)            !- cloud amounts pos in ARRAY
      INTEGER CLOD_BASE_HGHT(4)       !- cloud heights pos in ARRAY
      INTEGER COUNT                   !- count of values in list
      INTEGER CRNT_WTHR_TYPE          !- present weather pos in ARRAY
      INTEGER CMTIME                  !- time in century minutes
      INTEGER DAY                     !- day pos in ARRAY
      INTEGER HOUR                    !- hour pos in ARRAY
      INTEGER HRZL_VSBLY              !- visibility pos in ARRAY
      INTEGER I                       !- general loop counter
      INTEGER ISTAT                   !- return code from MDB
      INTEGER J                       !- general loop counter
      INTEGER K                       !- general loop counter
      INTEGER METDB_TMO_IHOURSBACK    !- time series time period                                   
      INTEGER MINT                    !- minute pos in ARRAY
      INTEGER MNTH                    !- month pos in ARRAY
      INTEGER MSL_PESR                !- MSLP pos in ARRAY
      INTEGER MXMM_SRFC_GUST          !- max gust pos in ARRAY
      INTEGER NELEM                   !- number of elements per ob                         
      INTEGER NOBS                    !- number of obs per MDB call
      INTEGER NOW(8)                  !- current time.
      INTEGER Q1HOUR_GLOBL_SOLR_RADTN !- radiation pos in ARRAY
      INTEGER Q1HOUR_SUN_AMNT         !- sunshine pos in ARRAY
      INTEGER RC                      !- general return code variable
      INTEGER SNOW_DPTH               !- snow depth pos in ARRAY                                                    
      INTEGER SRFC_AIR_TMPR           !- air temperature pos in ARRAY         
      INTEGER SRFC_DEW_PONT_TMPR      !- dew point pos in ARRAY 
      INTEGER SRFC_WIND_DRCTN         !- wind direction pos in ARRAY 
      INTEGER SRFC_WIND_SPED          !- wind speed pos in ARRAY
      INTEGER START(8)                !- start time of MDB request.
      INTEGER YEAR                    !- year pos in ARRAY

!-----------------------------------------------------------------------
! Declare real variables
!-----------------------------------------------------------------------                                                                                

      REAL ARRAY(IOBS,IELS)        !- array of obs and elements from MDB                               

!-----------------------------------------------------------------------
! Declare character variables
!-----------------------------------------------------------------------                                                                                

      CHARACTER*12   ARRTXT(4)            !- text representation of
                                          !- ARRAY value.
      CHARACTER*1    CREP(IOBS)           !- report text from MDB
      CHARACTER*500  CREQ                 !- MDB request string                                                    
      CHARACTER*1    CSTR(IOBS)           !- character data from MDB                                             
      CHARACTER*8    CSUBT                !- MDB datatype                                          
      CHARACTER*12   INCREMENT            !- MetDB request INCREMENT                                       
      CHARACTER*9    METDB_TMO_STATION    !- Station ID                                                  
      CHARACTER*200  METDB_TMO_DATFILE    !- Output file for data                                   
      CHARACTER*2    METDB_TMO_HOURSBACK  !- time series time period                                   
      CHARACTER*14   STARTDT              !- MetDB request START TIME                                                      
      CHARACTER*9    PLOT_TYPE            !- Plot type 

!-----------------------------------------------------------------------
! Declare functions
!-----------------------------------------------------------------------                                                                                

      INTEGER DT2HRS    !- Convert date/time to century hours

!-----------------------------------------------------------------------
! Get environment variable METDB_TMO_STATION
!-----------------------------------------------------------------------                                                                                

      CALL METDB_GETENV("METDB_TMO_STATION",METDB_TMO_STATION,RC)

      IF (RC.NE.0) THEN
        WRITE(6,*)'TMO: WARNING: Environment variable '//
     &            'METDB_TMO_STATION not set. Defaulting to 03772'
      ENDIF
!      METDB_TMO_STATION = '03772'

!-----------------------------------------------------------------------
! Get environment variable METDB_TMO_HOURSBACK
!-----------------------------------------------------------------------                                                                                

      CALL METDB_GETENV("METDB_TMO_HOURSBACK",METDB_TMO_HOURSBACK,RC)

      IF (RC.NE.0) THEN
        WRITE(6,*)'TMO: WARNING: Environment variable '//
     &            'METDB_TMO_HOURSBACK not set. Defaulting to 12'
      ENDIF

!      METDB_TMO_HOURSBACK = '24'
      READ(METDB_TMO_HOURSBACK,'(I2)')METDB_TMO_IHOURSBACK

!-----------------------------------------------------------------------
! Get environment variable METDB_TMO_DATFILE
!-----------------------------------------------------------------------                                                                                

      CALL METDB_GETENV("METDB_TMO_DATFILE",METDB_TMO_DATFILE,RC)

      IF (RC.NE.0) THEN
        WRITE(6,*)'TMO: ERROR: Environment variable '//
     &            'METDB_TMO_DATFILE not set. Cannot continue'
        STOP
      ENDIF
!      METDB_TMO_DATFILE='test.out'

!-----------------------------------------------------------------------
! Calculate start date/time. End time is current time. Start time is
! end time minus METDB_TMO_IHOURSBACK hours.
!-----------------------------------------------------------------------                                                                                
                     
      CALL DATIM(NOW)   !- get current time.

      CMTIME = DT2HRS(NOW(8),NOW(7),NOW(6),NOW(5))   !- yyyy,mm,dd,hh

      IF (METDB_TMO_IHOURSBACK.GT.12) THEN
        NOW(4) = 0   !- set minutes to zero to allow INCREMENT keyword
        INCREMENT = 'INCREMENT 01'
      ELSE
        INCREMENT = ' '
      ENDIF
       
      CMTIME = CMTIME * 60 + NOW(4)
      CMTIME = CMTIME - 60*METDB_TMO_IHOURSBACK

      CALL HRS2DT(START(8),START(7),START(6),START(5),CMTIME/60)
      START(4) = MOD(CMTIME,60)
      WRITE(STARTDT(1:4),'(I4.4)')START(8)
      WRITE(STARTDT(5:6),'(I2.2)')START(7)
      WRITE(STARTDT(7:8),'(I2.2)')START(6)
      STARTDT(9:9) = '/'
      WRITE(STARTDT(10:11),'(I2.2)')START(5)
      WRITE(STARTDT(12:13),'(I2.2)')START(4)
      STARTDT(14:14) = 'Z'

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------                                                                                

      CSUBT = 'LNDSYN  '                                                        
      CREQ =  'START TIME ' // STARTDT // ' ' // INCREMENT // ' ' //
     &        'PLATFORM ' // METDB_TMO_STATION // ' ' //
     &        'ELEMENTS YEAR MNTH DAY HOUR MINT SRFC_AIR_TMPR ' //
     &        'SRFC_DEW_PONT_TMPR SRFC_WIND_SPED SRFC_WIND_DRCTN ' //
     &        'MXMM_SRFC_GUST SNOW_DPTH HRZL_VSBLY CRNT_WTHR_TYPE ' //
     &        '(CLOD_AMNT CLOD_BASE_HGHT)*4 MSL_PESR ' //
     &        'Q1HOUR_GLOBL_SOLR_RADTN Q1HOUR_SUN_AMNT '

      NOBS  = IOBS                                                        
      NELEM = IELS                                                              
      ISTAT = 0                                                                 

      YEAR                    = 1
      MNTH                    = 2
      DAY                     = 3
      HOUR                    = 4
      MINT                    = 5
      SRFC_AIR_TMPR           = 6
      SRFC_DEW_PONT_TMPR      = 7
      SRFC_WIND_SPED          = 8
      SRFC_WIND_DRCTN         = 9
      MXMM_SRFC_GUST          = 10
      SNOW_DPTH               = 11
      HRZL_VSBLY              = 12
      CRNT_WTHR_TYPE          = 13
      CLOD_AMNT(1)            = 14
      CLOD_BASE_HGHT(1)       = 15
      CLOD_AMNT(2)            = 16
      CLOD_BASE_HGHT(2)       = 17
      CLOD_AMNT(3)            = 18
      CLOD_BASE_HGHT(3)       = 19
      CLOD_AMNT(4)            = 20
      CLOD_BASE_HGHT(4)       = 21
      MSL_PESR                = 22
      Q1HOUR_GLOBL_SOLR_RADTN = 23
      Q1HOUR_SUN_AMNT         = 24
      
!-----------------------------------------------------------------------
! Call MDB. Expects all data to be returned by a single MDB call.
! This should easily be the case as MDB is called to retrieve a max of
! IOBS obs.
!-----------------------------------------------------------------------                                                                                

      CALL MDB(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)                  

      IF (ISTAT.NE.0) THEN
        IF (ISTAT.EQ.16) THEN
          WRITE(6,*)'TMO: ERROR: ERROR occurred in MDB retrieval'
        ELSEIF (ISTAT.EQ.8) THEN
          WRITE(6,*)'TMO: ERROR: No observations retrieved from MDB'
        ELSEIF (ISTAT.EQ.4) THEN
          WRITE(6,*)'TMO: ERROR: More data to read. Not expected'
        ENDIF
        STOP
      ENDIF                                

      IF (NOBS.EQ.0) THEN
        WRITE(6,*)'TMO: ERROR: No observations retrieved from MD'
        STOP
      ENDIF

!-----------------------------------------------------------------------
! Open file for output
!-----------------------------------------------------------------------                                                                                

      OPEN(7,FILE=METDB_TMO_DATFILE,FORM='FORMATTED',IOSTAT=RC)
      IF (RC.NE.0) THEN
        WRITE(6,*)'TMO: ERROR: Could not open output file ',
     &             METDB_TMO_DATFILE
        STOP
      ENDIF

! Number of data elements - 1 (Present weather not to be plotted)
      WRITE(7,*)'10'

      WRITE(7,'(I2.2,A1,I2.2,A1,I4.4,1X,I2.2,A1,I2.2)')
     &      START(6),'/',START(7),'/',START(8),START(5),':',START(4)

      WRITE(7,'(I2.2,A1,I2.2,A1,I4.4,1X,I2.2,A1,I2.2)')
     &      NOW(6),'/',NOW(7),'/',NOW(8),NOW(5),':',NOW(4)

!-----------------------------------------------------------------------
!- MSL_PESR
!-----------------------------------------------------------------------

      PLOT_TYPE = "MIN MAX 2"
      CALL TMO_OUT(ARRAY,IOBS,IELS,NOBS,DAY,MNTH,YEAR,HOUR,MINT,
     &             MSL_PESR,0.01,0.0,'MSLP (mb)',PLOT_TYPE)

!-----------------------------------------------------------------------
!- SRFC_AIR_TMPR and SRFC_DEW_PONT_TMPR
!-----------------------------------------------------------------------

      WRITE(7,*)'Air temperature and dew point (C)'
      COUNT=0
      DO I=1,NOBS
        IF (ARRAY(I,SRFC_AIR_TMPR).GT.-9000000.0 .OR.
     &      ARRAY(I,SRFC_DEW_PONT_TMPR).GT.-9000000.0) COUNT=COUNT+1
      ENDDO

      PLOT_TYPE = "MIN MAX 5"
      WRITE(7,*)PLOT_TYPE
      WRITE(7,*)COUNT

      DO I=1,NOBS
        IF (ARRAY(I,SRFC_AIR_TMPR).GT.-9000000.0 .OR.
     &      ARRAY(I,SRFC_DEW_PONT_TMPR).GT.-9000000.0) THEN

          IF (ARRAY(I,SRFC_AIR_TMPR).GT.-9000000.0) THEN
            WRITE(ARRTXT(1),'(F12.3)')ARRAY(I,SRFC_AIR_TMPR)-273.1
	  ELSE
	    ARRTXT(1) = ' '
	  ENDIF
	           
          IF (ARRAY(I,SRFC_DEW_PONT_TMPR).GT.-9000000.0) THEN
            WRITE(ARRTXT(2),'(F12.3)')ARRAY(I,SRFC_DEW_PONT_TMPR)-273.1
	  ELSE
	    ARRTXT(2) = ' '
	  ENDIF
	           
          WRITE(7,'(I2.2,A1,I2.2,A1,I4.4,1X,I2.2,A1,I2.2,2(1X,A12))')
     &    NINT(ARRAY(I,DAY)),'/',NINT(ARRAY(I,MNTH)),'/',
     &    NINT(ARRAY(I,YEAR)),NINT(ARRAY(I,HOUR)),':',
     &    NINT(ARRAY(I,MINT)),ARRTXT(1),ARRTXT(2)

        ENDIF
      ENDDO

!-----------------------------------------------------------------------
!- SRFC_WIND_SPED & MXMM_SRFC_GUST
!-----------------------------------------------------------------------

      WRITE(7,*)'Surface wind speed & max gust (kts)'
      COUNT=0
      DO I=1,NOBS
        IF (ARRAY(I,SRFC_WIND_SPED).GT.-9000000.0 .OR.
     &      ARRAY(I,MXMM_SRFC_GUST).GT.-9000000.0) COUNT=COUNT+1
      ENDDO
      
      PLOT_TYPE = "  0 MAX 6"
      WRITE(7,*)PLOT_TYPE
      WRITE(7,*)COUNT

      DO I=1,NOBS
        IF (ARRAY(I,SRFC_WIND_SPED).GT.-9000000.0 .OR.
     &      ARRAY(I,MXMM_SRFC_GUST).GT.-9000000.0) THEN

          IF (ARRAY(I,SRFC_WIND_SPED).GT.-9000000.0) THEN
            WRITE(ARRTXT(1),'(F12.3)')1.9438445*ARRAY(I,SRFC_WIND_SPED)
	  ELSE
	    ARRTXT(1) = ' '
	  ENDIF
	           
          IF (ARRAY(I,MXMM_SRFC_GUST).GT.-9000000.0) THEN
            WRITE(ARRTXT(2),'(F12.3)')1.9438445*ARRAY(I,MXMM_SRFC_GUST)
	  ELSE
	    ARRTXT(2) = '-9999999.0'
	  ENDIF
	           
          WRITE(7,'(I2.2,A1,I2.2,A1,I4.4,1X,I2.2,A1,I2.2,2(1X,A12))')
     &    NINT(ARRAY(I,DAY)),'/',NINT(ARRAY(I,MNTH)),'/',
     &    NINT(ARRAY(I,YEAR)),NINT(ARRAY(I,HOUR)),':',
     &    NINT(ARRAY(I,MINT)),ARRTXT(1),ARRTXT(2)

        ENDIF
      ENDDO
      
!-----------------------------------------------------------------------
!- SRFC_WIND_DRCTN
!-----------------------------------------------------------------------

      WRITE(7,*)'Surface wind direction (deg true)'
      COUNT=0
      DO I=1,NOBS
        IF (ARRAY(I,SRFC_WIND_DRCTN).GT.-9000000.0 .AND.
     &      ARRAY(I,SRFC_WIND_SPED).GT.0) COUNT=COUNT+1
      ENDDO
      PLOT_TYPE = "  0 360 1"
      WRITE(7,*)PLOT_TYPE
      WRITE(7,*)COUNT

      DO I=1,NOBS
        IF (ARRAY(I,SRFC_WIND_DRCTN).GT.-9000000.0  .AND.
     &      ARRAY(I,SRFC_WIND_SPED).GT.0) THEN
          WRITE(7,'(I2.2,A1,I2.2,A1,I4.4,1X,I2.2,A1,I2.2,1X,F12.3)')
     &    NINT(ARRAY(I,DAY)),'/',NINT(ARRAY(I,MNTH)),'/',
     &    NINT(ARRAY(I,YEAR)),NINT(ARRAY(I,HOUR)),':',
     &    NINT(ARRAY(I,MINT)),ARRAY(I,SRFC_WIND_DRCTN)
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
!- SNOW_DPTH
!-----------------------------------------------------------------------

      PLOT_TYPE = "  0 MAX 1"
      CALL TMO_OUT(ARRAY,IOBS,IELS,NOBS,DAY,MNTH,YEAR,HOUR,MINT,
     &             SNOW_DPTH,0.01,0.0,'Snow depth (cm)',PLOT_TYPE)

!-----------------------------------------------------------------------
!- HRZL_VSBLY
!-----------------------------------------------------------------------

      PLOT_TYPE = "  0 MAX 3"
      CALL TMO_OUT(ARRAY,IOBS,IELS,NOBS,DAY,MNTH,YEAR,HOUR,MINT,
     &             HRZL_VSBLY,1.0,0.0,'Horizontal visibilty (m)',
     &             PLOT_TYPE)

!-----------------------------------------------------------------------
!- CLOUD BASE HEIGHTS
!-----------------------------------------------------------------------

      WRITE(7,*)'Cloud base heights (ft)'
      COUNT=0
      DO I=1,NOBS
        IF (ARRAY(I,CLOD_BASE_HGHT(1)).GT.-9000000.0 .OR.
     &      ARRAY(I,CLOD_BASE_HGHT(2)).GT.-9000000.0 .OR.
     &      ARRAY(I,CLOD_BASE_HGHT(3)).GT.-9000000.0 .OR.
     &      ARRAY(I,CLOD_BASE_HGHT(4)).GT.-9000000.0) COUNT=COUNT+1
      ENDDO

      PLOT_TYPE = "  0 MAX 4"
      WRITE(7,*)PLOT_TYPE
      WRITE(7,*)COUNT

      DO I=1,NOBS
        IF (ARRAY(I,CLOD_BASE_HGHT(1)).GT.-9000000.0 .OR.
     &      ARRAY(I,CLOD_BASE_HGHT(2)).GT.-9000000.0 .OR.
     &      ARRAY(I,CLOD_BASE_HGHT(3)).GT.-9000000.0 .OR.
     &      ARRAY(I,CLOD_BASE_HGHT(4)).GT.-9000000.0) THEN

          DO J=1,4
	    IF (ARRAY(I,CLOD_BASE_HGHT(J)).GT.-9000000.0) THEN
              WRITE(ARRTXT(J),'(F12.3)')
     &        3.3333333333*ARRAY(I,CLOD_BASE_HGHT(J))
	    ELSE
	      ARRTXT(J) = '-9999999.000'
            ENDIF
	  ENDDO

          WRITE(7,'(I2.2,A1,I2.2,A1,I4.4,1X,I2.2,A1,I2.2,4(1X,A12))')
     &    NINT(ARRAY(I,DAY)),'/',NINT(ARRAY(I,MNTH)),'/',
     &    NINT(ARRAY(I,YEAR)),NINT(ARRAY(I,HOUR)),':',
     &    NINT(ARRAY(I,MINT)),ARRTXT(1),ARRTXT(2),ARRTXT(3),ARRTXT(4)
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
!- CLOUD AMOUNT, BASE HEIGHT - PRESENT VALUES ONLY
!-----------------------------------------------------------------------

      I=NOBS
      DO J=1,4
        ARRTXT(J)(2:2) = '/'
        IF (ARRAY(I,CLOD_AMNT(J)).GT.-9000000.0) THEN
          WRITE(ARRTXT(J)(1:1),'(I1.1)')NINT(ARRAY(I,CLOD_AMNT(J)))
	ELSE
	  ARRTXT(J)(1:1) = ' '
	ENDIF
        IF (ARRAY(I,CLOD_BASE_HGHT(J)).GT.-9000000.0) THEN
          WRITE(ARRTXT(J)(3:12),'(I10)')
     &    NINT(3.3333333333*ARRAY(I,CLOD_BASE_HGHT(J)))
          BLANK=0
	  DO K=3,LEN(ARRTXT(J))
	    IF (ARRTXT(J)(K:K).EQ.' ') BLANK=K
	  ENDDO
          IF (BLANK.LT.LEN(ARRTXT(J))) THEN
	    ARRTXT(J)(3:3+LEN(ARRTXT(J))-BLANK-1) = 
     &      ARRTXT(J)(BLANK+1:LEN(ARRTXT(J)))
            ARRTXT(J)(BLANK+1:LEN(ARRTXT(J))) = ' '
	  ENDIF
	ELSE
	  ARRTXT(J)(3:12) = ' '
	ENDIF
      ENDDO

      WRITE(7,'(I2.2,A1,I2.2,A1,I4.4,1X,I2.2,A1,I2.2,4(1X,A12))')
     &NINT(ARRAY(I,DAY)),'/',NINT(ARRAY(I,MNTH)),'/',
     &NINT(ARRAY(I,YEAR)),NINT(ARRAY(I,HOUR)),':',
     &NINT(ARRAY(I,MINT)),ARRTXT(1),ARRTXT(2),ARRTXT(3),ARRTXT(4)
      
!-----------------------------------------------------------------------
!- Q1HOUR_GLOBL_SOLR_RADTN
!-----------------------------------------------------------------------

      PLOT_TYPE = "  0 MAX 1"
      CALL TMO_OUT(ARRAY,IOBS,IELS,NOBS,DAY,MNTH,YEAR,HOUR,MINT,
     &             Q1HOUR_GLOBL_SOLR_RADTN,0.001,0.0,
     &             'Global solar radiation in last hour (KJ/m)',
     &             PLOT_TYPE)

!-----------------------------------------------------------------------
!- Q1HOUR_SUN_AMNT
!-----------------------------------------------------------------------

      PLOT_TYPE = "  0  60 1"
      CALL TMO_OUT(ARRAY,IOBS,IELS,NOBS,DAY,MNTH,YEAR,HOUR,MINT,
     &             Q1HOUR_SUN_AMNT,1.0,0.0,
     &             'Sunshine in last hour (mins)',PLOT_TYPE)

!-----------------------------------------------------------------------
!- CRNT_WTHR_TYPE - PRESENT VALUE ONLY
!-----------------------------------------------------------------------

      WRITE(7,*)'Present weather'

      I=NOBS
      COUNT=0
      IF (ARRAY(I,CRNT_WTHR_TYPE).GT.-9000000.0) COUNT=1
      
      PLOT_TYPE = "  0 200 1"
      WRITE(7,*)PLOT_TYPE
      WRITE(7,*)COUNT

      IF (COUNT.GT.0) THEN
        IF (ARRAY(I,CRNT_WTHR_TYPE).GT.-9000000.0) THEN
          WRITE(ARRTXT(1),'(F12.3)')ARRAY(I,CRNT_WTHR_TYPE)
        ELSE
          ARRTXT(1) = ' '
        ENDIF

        WRITE(7,'(I2.2,A1,I2.2,A1,I4.4,1X,I2.2,A1,I2.2,4(1X,A12))')
     &  NINT(ARRAY(I,DAY)),'/',NINT(ARRAY(I,MNTH)),'/',
     &  NINT(ARRAY(I,YEAR)),NINT(ARRAY(I,HOUR)),':',
     &  NINT(ARRAY(I,MINT)),ARRTXT(1)
      ENDIF

!-----------------------------------------------------------------------
! Close output file and exit.
!-----------------------------------------------------------------------                                                                                

      CLOSE(7)
                                                                                
      STOP                                                                      
      END                                                                       

!=======================================================================
! TMO_OUT
!=======================================================================

      SUBROUTINE TMO_OUT(ARRAY,IOBS,IELS,NOBS,DAY,MNTH,YEAR,HOUR,MINT,
     &                   ELEMENT,SCALE,REF,TITLE,PLOT_TYPE)

      IMPLICIT NONE

      INTEGER COUNT               !- Count of non-missing elements.
      INTEGER DAY                 !- Day of ob
      INTEGER ELEMENT             !- Position of element in ARRAY
      INTEGER HOUR                !- Hour of ob
      INTEGER I                   !- Loop variable
      INTEGER IELS                !- Dimension 2 of ARRAY
      INTEGER IOBS                !- Dimension 1 of ARRAY
      INTEGER MINT                !- Minute of ob
      INTEGER MNTH                !- Month of ob
      INTEGER YEAR                !- Year of ob
      INTEGER NOBS                !- Number of obs retrieved from MDB

      REAL    ARRAY(IOBS,IELS)    !- Array of observations
      REAL    REF                 !- Reference value
      REAL    SCALE               !- Scaling factor

      CHARACTER*(*) TITLE         !- Title of ELEMENT.
      CHARACTER*9   PLOT_TYPE     
      SAVE

      WRITE(7,*)TITLE
      COUNT=0
      DO I=1,NOBS
        IF (ARRAY(I,ELEMENT).GT.-9000000.0) COUNT=COUNT+1
      ENDDO

      WRITE(7,*)PLOT_TYPE
      WRITE(7,*)COUNT

      DO I=1,NOBS
        IF (ARRAY(I,ELEMENT).GT.-9000000.0) THEN

          WRITE(7,'(I2.2,A1,I2.2,A1,I4.4,1X,I2.2,A1,I2.2,1X,F12.3)')
     &    NINT(ARRAY(I,DAY)),'/',NINT(ARRAY(I,MNTH)),'/',
     &    NINT(ARRAY(I,YEAR)),NINT(ARRAY(I,HOUR)),':',
     &    NINT(ARRAY(I,MINT)),(SCALE*ARRAY(I,ELEMENT))+REF

        ENDIF
      ENDDO

      RETURN
      END
