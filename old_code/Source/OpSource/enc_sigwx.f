!---------------------------------------------------------------------
!                                                                      
! PROGRAM       : enc_sigwx main (on HP)                               
!                                                                      
! PURPOSE       : to encode significant weather reports (SIGWX) into  
!               : BUFR.                                                
!                                                                      
! DESCRIPTION   : Significant weather reports are presented to us in  
!               : a browsable dataset. enc_sigwx reads these reports  
!               : and produces a SIGWX BUFR message by calling the    
!               : BUFR encode suite of programs. Documentation
!               : on the Met Office WWW                                                                      !
!                                                                    
!               : MOWWW  http://www01/metdb/documentation/other/
!               :        sigwx.html                          
!                                                                      
! DATA TYPE(S)  : see code                                             
!                                                                      
! CALLED BY     : none                                
!                                                                      
! CALLS         : external : ENBUFV2 and associated routines            
!               : internal : ENC_START, ENC_JET,  ENC_FRONT,          
!                          : ENC_CLOUD, ENC_TROP, ENC_VOLCANO,        
!                          : ENC_ICING, ENC_TURB, ENC_STORM,
!                          : ENC_RADIATION,ENC_MCLOUD            !1.6
!                                                                      
! PARAMETERS    : see code                                             
!
! REVISION INFO (RCS)
!
! $Revision: 4$
! $Date: 07/07/2011 08:55:11$
! $Source: /home/us0400/mdb/op/lib/source/RCS/enc_sigwx.f,v $                                                                      
!
! CHANGE RECORD
!
! $Log:
!  4    Met_DB_Project 1.3         07/07/2011 08:55:11    Sheila Needham  Allow
!        MCLOUD groups in high level cloud
!  3    Met_DB_Project 1.2         12/07/2006 11:06:53    Alison Weir
!       Comments describing medium level cloud descriptors corrected.
!  2    Met_DB_Project 1.1         06/07/2006 12:52:06    Alison Weir
!       Is_Medium initialised as FALSE
!  1    Met_DB_Project 1.0         30/01/2006 20:22:15    Sheila Needham  
! $
! Revision 1.6  2005/03/21 12:26:33  usmdb
! Alison Weir 21st March 2005.
! Amendments for medium level.
!
! Revision 1.5  2004/03/29 14:24:10  usmdb
! Stan Kellett. 29th March 2004.
! 1.5a allow compilation with f90 compiler under HPUX operating system.
! 1.5b number of continuation lines reduced where too many to compile.
! 1.5c change to allow top and base of a jet to be encoded.
!
! Revision 1.4  2001/06/29 12:54:33  usmdb
! Added ICETURB feature - a combined icing/turbulence area.
! Also use C I/O to write out the BUFR message - S.Cox
!
! Revision 1.3  2000/02/25 13:29:43  usmdb
! Addition of routine to encode a SigWx radiation symbol
! into BUFR. Requested by Horace SigWx team. The routine
! represents the radiation symbol as a point position of
! the reactor. Also coded are the reactor name and time
! of release - S.Cox
!
! Revision 1.2  99/12/23  09:48:56  09:48:56  usmdb (Generic MDB account)
! Cloud amount 020011 changed to cloud distribution
! 020008 at request of Horace SigWx team - S.Cox
! 
! Revision 1.1  98/10/08  08:54:28  08:54:28  usmdb (Generic MDB account)
! Initial revision (under RCS)
! 
! S.Cox 29/10/97 : Volcano name (160 bits) changed to Name Of 
!                  Feature (224 bits)
!
! S.Cox 08/01/97 : Calls new BUFR encode routine ENBUFV2 to code
!                  message in BUFR edition 2
!
! S.Cox 02/05/96 : Put end of section descriptors within replication  
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors        
!
! S.Cox 16/10/95 : New descriptors following WMO EMDR&C meeting         
!
! S.Cox 01/09/95 : written
!                                                                      
!---------------------------------------------------------------------

      PROGRAM ENC_SIGWX
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER         MAXDESC           !- max no. of descriptors.
      PARAMETER       (MAXDESC=12000)   !- set max descs.
      
      REAL            VALUES(MAXDESC)   !- BUFR values array.
      INTEGER         DESCR(MAXDESC)    !- BUFR descriptors array.
      INTEGER         DATIME(5)         !- date/time array.
      INTEGER         NOBS              !- no. of obs.
      INTEGER         NELEM             !- no. of elements.
      INTEGER         NDESC             !- no. of descriptors.
      INTEGER         L                 !- length of BUFR message.
      INTEGER         NAMES_CNT         !- NAMES position indicator.
      INTEGER         S1_YEAR           !- BUFR section 1 date (yyyy)
      INTEGER         S1_MONTH          !- BUFR section 1 date (mm)
      INTEGER         S1_DAY            !- BUFR section 1 date (dd)
      INTEGER         S1_HOUR           !- BUFR section 1 date (hh)
      INTEGER         S1_MIN            !- BUFR section 1 date (mm)
      INTEGER         Edition           !- BUFR edition number     
      INTEGER         MasterTable       !- BUFR Master table no.      
      INTEGER         OrigCentre        !- Originating Centre
      INTEGER         DataType          !- data category type
      INTEGER         DataSubType       !- data category subtype
      INTEGER         VerMasTab         !- Version no. of master tables
      INTEGER         VerLocTab         !- Version no. of local tables
      INTEGER         Sect3Type         !- BUFR section 3 data type
      INTEGER         RC                !- Return code from METDB_COPEN
      INTEGER         WRITE_SIZE        !- Amount written to o/p file
      LOGICAL         CMP               !- BUFR compression flag.
      LOGICAL         ExtraSect1        !- F for no extra BUFR s.1 data
      LOGICAL         ExtraSect2        !- FALSE for no BUFR section 2
      LOGICAL         Is_Medium         !- TRUE if medium level sigwx  !1.6
      CHARACTER*10240 MESAGE            !- final BUFR message.
      CHARACTER*1000  NAMES             !- BUFR names string.
      CHARACTER*40    SIGWX_ELEMENT     !- i/p SIGWX element names.
      CHARACTER*1     CharSect1         !- extra BUFR sect 1 data     
      CHARACTER*240   CharSect2         !- BUFR sect 2 data          !1.6

! ----------------------------------------------------------------------
! --- initialise variables.
! ----------------------------------------------------------------------

      NDESC=0               !- start of BUFR message, NDESC = 0.
      NELEM=0               !- start of BUFR message, NELEM = 0.
      NOBS=1                !- only code one SIGWX report.
      L=0                   !- eventual output from enbufr, octets.
      NAMES(:)=' '          !- initialise names character string.      
      NAMES_CNT=1           !- position indicator for names char string.
      CMP=.FALSE.           !- don't perform BUFR compression.
      Is_Medium=.FALSE.     !- initialise as high level           !ST 2
      
! ----------------------------------------------------------------------
! --- open the SIGWX dataset for formatted read.
! ----------------------------------------------------------------------

      OPEN(10,FILE='sigwx.dat',STATUS='UNKNOWN',FORM='FORMATTED')
      
! ----------------------------------------------------------------------
! --- code the start of the BUFR data section.
! ----------------------------------------------------------------------

      CALL ENC_START(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &               NAMES_CNT,S1_YEAR,S1_MONTH,S1_DAY,S1_HOUR,
     &               S1_MIN,IS_MEDIUM,CHARSECT2)                 !1.6

      DATIME(1)=S1_YEAR        !- BUFR section 1 date (yyyy)
      DATIME(2)=S1_MONTH       !- BUFR section 1 date (mm)
      DATIME(3)=S1_DAY         !- BUFR section 1 date (dd)
      DATIME(4)=S1_HOUR        !- BUFR section 1 date (hh)
      DATIME(5)=S1_MIN         !- BUFR section 1 date (mm)
   
! ----------------------------------------------------------------------
! --- read the SIGWX element name, whilst there is data to read.
! ----------------------------------------------------------------------

   1  READ(10,'(A40)',END=99) SIGWX_ELEMENT
   
! ----------------------------------------------------------------------
! --- call the correct SIGWX element subroutine to fill the DESC, VALUES
! --- and NAMES arrays/string and update the position counters.
! ----------------------------------------------------------------------

      IF (IS_MEDIUM) THEN                                       !1.6
    
        IF (SIGWX_ELEMENT(1:4).EQ.'MJET') THEN                    !1.6     
          CALL ENC_JET(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)  
        ELSEIF (SIGWX_ELEMENT(1:5).EQ.'MTURB') THEN               !1.6
          CALL ENC_TURB(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT) 
        ELSEIF (SIGWX_ELEMENT(1:5).EQ.'STORM') THEN
          CALL ENC_STORM(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT) 
        ELSEIF (SIGWX_ELEMENT(1:6).EQ.'MCLOUD') THEN               !1.6
          CALL ENC_MCLOUD(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,  !1.6
     &    NAMES_CNT)                                               !1.6
        ELSEIF (SIGWX_ELEMENT(1:6).EQ.'MFRONT') THEN               !1.6
          CALL ENC_FRONT(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)  
        ELSEIF (SIGWX_ELEMENT(1:5).EQ.'MTROP') THEN                !1.6
          CALL ENC_TROP(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSEIF (SIGWX_ELEMENT(1:6).EQ.'MICING') THEN               !1.6
          CALL ENC_ICING(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSEIF (SIGWX_ELEMENT(1:7).EQ.'VOLCANO') THEN
          CALL ENC_VOLCANO(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSEIF (SIGWX_ELEMENT(1:9).EQ.'RADIATION') THEN
          CALL ENC_RADIATION(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSEIF (SIGWX_ELEMENT(1:8).EQ.'MICETURB') THEN              !1.6
          CALL ENC_ICETURB(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSE
          WRITE(6,*)'SIGWX element identifier not recognised!!'
        ENDIF
      
      ELSE   !1.6  not medium
      
        IF (SIGWX_ELEMENT(1:3).EQ.'JET') THEN  
          CALL ENC_JET(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)  
        ELSEIF (SIGWX_ELEMENT(1:4).EQ.'TURB') THEN 
          CALL ENC_TURB(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT) 
        ELSEIF (SIGWX_ELEMENT(1:5).EQ.'STORM') THEN
          CALL ENC_STORM(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT) 
        ELSEIF (SIGWX_ELEMENT(1:5).EQ.'CLOUD') THEN
          CALL ENC_CLOUD(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)  
        ELSEIF (SIGWX_ELEMENT(1:6).EQ.'MCLOUD') THEN
           CALL ENC_MCLOUD(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &     NAMES_CNT)
        ELSEIF (SIGWX_ELEMENT(1:5).EQ.'FRONT') THEN 
          CALL ENC_FRONT(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)  
        ELSEIF (SIGWX_ELEMENT(1:4).EQ.'TROP') THEN    
          CALL ENC_TROP(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSEIF (SIGWX_ELEMENT(1:5).EQ.'ICING') THEN
          CALL ENC_ICING(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSEIF (SIGWX_ELEMENT(1:7).EQ.'VOLCANO') THEN
          CALL ENC_VOLCANO(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSEIF (SIGWX_ELEMENT(1:9).EQ.'RADIATION') THEN
          CALL ENC_RADIATION(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSEIF (SIGWX_ELEMENT(1:7).EQ.'ICETURB') THEN 
          CALL ENC_ICETURB(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &    NAMES_CNT)   
        ELSE
          WRITE(6,*)'SIGWX element identifier not recognised!!'
        ENDIF
      
      ENDIF

! ----------------------------------------------------------------------
! --- read the next SIGWX element.
! ----------------------------------------------------------------------
 
      GOTO 1
     
! ----------------------------------------------------------------------
! --- no more elements to read, close the dataset.
! ----------------------------------------------------------------------

  99  CONTINUE
   
      CLOSE(10)

! ----------------------------------------------------------------------
! --- call enbufv2 to code the BUFR message in BUFR edition 2.
! ----------------------------------------------------------------------

      Edition      = -99        !- use default = 2      
      MasterTable  = -99        !- use default = 0
      OrigCentre   =  74        !- UK Met Office
      DataType     =   7        !- Synoptic features
      DataSubType  = -99        !- use default = 255
      VerMasTab    = -99        !- use default = 2
      VerLocTab    = -99        !- use default = 1
      Sect3Type    =   0        !- BUFR sect 3, Byte 7 = Other data        
      ExtraSect1   = .FALSE.    !- no extra BUFR section 1 data
      IF (Is_Medium) THEN
        ExtraSect2   = .TRUE.   !- BUFR section 2 data holds med level map areas
      ELSE
        ExtraSect2   = .FALSE.  !- no BUFR section 2 data
      ENDIF
      CharSect1    = ' '        !- dummy extra BUFR section 1 data
!     CharSect2    = ' '        !- dummy BUFR section 2 data    !1.6 set in enc_start

      CALL ENBUFV2(DESCR,VALUES,NDESC,NELEM,NOBS,NAMES,DATIME,
     &             MESAGE,CMP,L,Edition,MasterTable,OrigCentre,
     &             DataType,DataSubType,VerMasTab,VerLocTab,
     &             ExtraSect1,CharSect1,ExtraSect2,CharSect2,Sect3Type)      

! ----------------------------------------------------------------------
! --- write the BUFR message to a dataset.
! ----------------------------------------------------------------------

      CALL METDB_COPEN(20,'sigwx.bufr',2,RC)
      IF (RC.NE.0) THEN
        WRITE(6,*)'ENC_SIGWX: ERROR. Could not open file ',
     &            'sigwx.bufr for write. RC = ',RC
        STOP
      ENDIF

      CALL METDB_CWRITE(20,MESAGE(1:L),WRITE_SIZE)                  
      CALL METDB_CCLOSE(20)
           
      STOP
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_start subroutine (on HP)                         
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX dataset header     
!                : into BUFR.                                           
!                                                                      
! DESCRIPTION    : The start of the SIGWX report is read from the      
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.
!                : For a medium level report, map areas are stored
!                : in Charsect2, and passed back.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  :
!
! A.Weir 21/09/04: Medium level sigwx reports handled.
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors
!         
! S.Cox 01/09/95 : written                             
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE ENC_START(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                     NAMES_CNT,S1_YEAR,S1_MONTH,S1_DAY,S1_HOUR,
     &                     S1_MIN,IS_MEDIUM,CHARSECT2)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=15)         !- used to describe start.

      INTEGER       MAXDESC             !- max no. of descriptors. !1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.
      CHARACTER*240 CHARSECT2           !- BUFR section 2  - holds map   !1.6
                                        !- areas for medium mevel sigwx  !1.6
      CHARACTER*60  AREA_STRING1        !- Medium Level area 1           !1.6
      CHARACTER*60  AREA_STRING2        !- Medium Level area 2           !1.6 
      CHARACTER*60  AREA_STRING3        !- Medium Level area 3           !1.6
      CHARACTER*60  AREA_STRING4        !- Medium Level area 4           !1.6
      INTEGER       SPACE_PADDING       !- Used to add space padding     !1.6
      
      INTEGER       NAMES_CNT           !- names postion indicator

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       SDESCR(SECTND)      !- descriptors for start.
      INTEGER       ORIG_CENTRE         !- originating/gen centre.
      INTEGER       S1_YEAR             !- BUFR section 1 year.
      INTEGER       S1_MONTH            !- BUFR section 1 month.
      INTEGER       S1_DAY              !- BUFR section 1 day.
      INTEGER       S1_HOUR             !- BUFR section 1 hour.
      INTEGER       S1_MIN              !- BUFR section 1 minute.
      INTEGER       YEAR                !- year.
      INTEGER       MONTH               !- month.
      INTEGER       DAY                 !- day.
      INTEGER       HOUR                !- hour.
      INTEGER       MINUTE              !- minute.
      REAL          B_HEIGHT            !- base height of chart.
      REAL          T_HEIGHT            !- top height of chart. 
      LOGICAL       IS_MEDIUM           !- TRUE if medium level sigwx  !1.6

      DATA          SDESCR/001031,      !- originating/gen centre.
     &                     008021,      !- data time (analysis)
     &                     004001,      !- year.
     &                     004002,      !- month.
     &                     004003,      !- day.
     &                     004004,      !- hour.
     &                     004005,      !- minute.
     &                     008021,      !- validity time (forecast)
     &                     004001,      !- year.
     &                     004002,      !- month.
     &                     004003,      !- day.
     &                     004004,      !- hour.
     &                     004005,      !- minute.
     &                     007002,      !- base height of chart.
     &                     007002/      !- top height of chart.

! ----------------------------------------------------------------------
! --- Enter the descriptors into the descriptor array.
! ----------------------------------------------------------------------

      DO I=1,SECTND
         DESCR(NDESC+I)=IDES(SDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND            !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- Read the originating/generating centre.
! ----------------------------------------------------------------------

      READ(10,*) ORIG_CENTRE

      NELEM=NELEM+1
      VALUES(NELEM)=ORIG_CENTRE     !- corresponds to desc 001031.

! ----------------------------------------------------------------------
! --- Add data time (analysis) to the values array.
! ----------------------------------------------------------------------

      NELEM=NELEM+1
      VALUES(NELEM)=16              !- corresponds to desc 008021.

! ----------------------------------------------------------------------
! --- Read the data time year, month, day, hour, minute and put them in
! --- the values array.
! ----------------------------------------------------------------------

      READ(10,*) YEAR, MONTH, DAY, HOUR, MINUTE

      NELEM=NELEM+1
      VALUES(NELEM)=YEAR            !- corresponds to desc 004001.
      NELEM=NELEM+1
      VALUES(NELEM)=MONTH           !- corresponds to desc 004002.
      NELEM=NELEM+1
      VALUES(NELEM)=DAY             !- corresponds to desc 004003.
      NELEM=NELEM+1
      VALUES(NELEM)=HOUR            !- corresponds to desc 004004.
      NELEM=NELEM+1
      VALUES(NELEM)=MINUTE          !- corresponds to desc 004005.

! ----------------------------------------------------------------------
! --- Add validity time (forecast) to the values array.
! ----------------------------------------------------------------------

      NELEM=NELEM+1
      VALUES(NELEM)=4               !- corresponds to desc 008021.

! ----------------------------------------------------------------------
! --- Read the validity time year, month, day, hour, minute and put them
! --- in the values array.
! ----------------------------------------------------------------------

      READ(10,*) YEAR, MONTH, DAY, HOUR, MINUTE

      NELEM=NELEM+1
      VALUES(NELEM)=YEAR            !- corresponds to desc 004001.
      NELEM=NELEM+1
      VALUES(NELEM)=MONTH           !- corresponds to desc 004002.
      NELEM=NELEM+1
      VALUES(NELEM)=DAY             !- corresponds to desc 004003.
      NELEM=NELEM+1
      VALUES(NELEM)=HOUR            !- corresponds to desc 004004.
      NELEM=NELEM+1
      VALUES(NELEM)=MINUTE          !- corresponds to desc 004005.

! ----------------------------------------------------------------------
! --- Set the BUFR message section 1 date/time variables to the SIGWX
! --- message validity time.
! ----------------------------------------------------------------------

      S1_YEAR=YEAR
      S1_MONTH=MONTH
      S1_DAY=DAY
      S1_HOUR=HOUR
      S1_MIN=MINUTE

! ----------------------------------------------------------------------
! --- Read the bottom and top heights for the chart and put them in the
! --- values array.
! ----------------------------------------------------------------------

      READ(10,*) B_HEIGHT, T_HEIGHT

      NELEM=NELEM+1
      VALUES(NELEM)=B_HEIGHT        !- corresponds to desc 007002.
      NELEM=NELEM+1
      VALUES(NELEM)=T_HEIGHT        !- corresponds to desc 007002.

! ----------------------------------------------------------------------
! --- Determine whether this is a medium level sigwx report -     !1.6
! --- medium = 3048 to 13716 metres, high = 7620 to 19202 metres. !1.6
! ----------------------------------------------------------------------

      IF  (B_HEIGHT.GE.3048.0 .AND. T_HEIGHT.LE.13720.0) THEN     !1.6
        IS_MEDIUM = .TRUE.                                        !1.6
      ENDIF                                                       !1.6
      
! ----------------------------------------------------------------------
! --- For Medium Level SigWx, add the 4 area lines to CHARSECT2,  !1.6
! --- 60 characters for each.                                     !1.6
! ----------------------------------------------------------------------

      IF (IS_MEDIUM) THEN                                         !1.6
        READ(10,'(A60)')AREA_STRING1                              !1.6
        READ(10,'(A60)')AREA_STRING2                              !1.6
        READ(10,'(A60)')AREA_STRING3                              !1.6
        READ(10,'(A60)')AREA_STRING4                              !1.6

! ----------------------------------------------------------------------
! --- Quick check that the first area string does not contain a   !1.6
! --- sigwx element, indicating that this isn't actually a medium !1.6
! --- level chart. Output warning, but continue as medium.        !1.6
! ----------------------------------------------------------------------

        IF (AREA_STRING1(1:3).EQ.'JET' .OR.                       !1.6
     &      AREA_STRING1(1:4).EQ.'TURB' .OR.                      !1.6
     &      AREA_STRING1(1:5).EQ.'CLOUD' .OR.                     !1.6
     &      AREA_STRING1(1:5).EQ.'FRONT' .OR.                     !1.6
     &      AREA_STRING1(1:4).EQ.'TROP' .OR.                      !1.6
     &      AREA_STRING1(1:5).EQ.'ICING' .OR.                     !1.6
     &      AREA_STRING1(1:7).EQ.'ICETURB') THEN                  !1.6
          WRITE(6,*) '** Medium level chart heights of ',         !1.6
     &               B_HEIGHT, T_HEIGHT                           !1.6
          WRITE(6,*) '** but high level element found ',          !1.6
     &               AREA_STRING1(1:7)                            !1.6
          WRITE(6,*) '** Continuing as medium.'                   !1.6
	    ENDIF                                                     !1.6

! pad each area string with spaces                                !1.6
        SPACE_PADDING = 60 - LEN(AREA_STRING1)                    !1.6
        DO I=LEN(AREA_STRING1)+1,                                 !1.6
     &       	LEN(AREA_STRING1)+SPACE_PADDING                   !1.6
	      AREA_STRING1(I:I)=' '                                   !1.6
        ENDDO                                                     !1.6

        SPACE_PADDING = 60 - LEN(AREA_STRING2)                    !1.6
        DO I=LEN(AREA_STRING2)+1,                                 !1.6
     &      	LEN(AREA_STRING2)+SPACE_PADDING                   !1.6
	      AREA_STRING2(I:I)=' '                                   !1.6
        ENDDO                                                     !1.6
	                      
        SPACE_PADDING = 60 - LEN(AREA_STRING3)                    !1.6
        DO I=LEN(AREA_STRING3)+1,                                 !1.6
     &      	LEN(AREA_STRING3)+SPACE_PADDING                   !1.6
	      AREA_STRING3(I:I)=' '                                   !1.6
        ENDDO                                                     !1.6
	
        SPACE_PADDING = 60 - LEN(AREA_STRING4)                    !1.6
        DO I=LEN(AREA_STRING4)+1,                                 !1.6
     &      	LEN(AREA_STRING4)+SPACE_PADDING                   !1.6
	      AREA_STRING4(I:I)=' '                                   !1.6
        ENDDO                                                     !1.6
	
        CHARSECT2=AREA_STRING1//AREA_STRING2//                    !1.6
     &            AREA_STRING3//AREA_STRING4                      !1.6
      ENDIF                                                       !1.6


      RETURN
      END
      
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_jet subroutine (on HP)                           
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX jet streams        
!                : into BUFR.                                           
!                                                                     
! DESCRIPTION    : The SIGWX jet streams data are read from the        
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  : 
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors
!
! S.Cox 16/10/95 : New descriptors following WMO EMDR&C meeting
!
! S.Cox 01/09/95 : written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE ENC_JET(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                   NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=17)         !- used to describe jet. !1.5c

      INTEGER       MAXDESC             !- max no. of descriptors. !1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       IJETS               !- loop counter.
      INTEGER       IPNTS               !- loop counter.
      INTEGER       NUM_JETS            !- number of jet streams.
      INTEGER       NUM_PNTS            !- number of pnts in jet.
      INTEGER       JDESCR(SECTND)      !- descriptors for jet stream.
      INTEGER       NAMES_CNT           !- names postion indicator.
      REAL          LAT                 !- jet stream lats.
      REAL          LON                 !- jet stream lons.
      REAL          HEIGHT              !- jet stream heights.
      REAL          WNDSPD              !- jet stream wind speeds.
      REAL          FLVLA               !- Flight level above jet in metres !1.5c
      REAL          FLVLB               !- Flight level below jet in metres !1.5c

      DATA          JDESCR/115000,      !- delayed replication.
     &                     031001,      !- no. of jet streams to follow.
     &                     008011,      !- meteorological feature.
     &                     008007,      !- dimensional significance.
     &                     109000,031001, !- delayed replication.no. of points to follow.!1.5b
     &                     005002,      !- latitude.
     &                     006002,      !- longitude.
     &                     007010,      !- flight level                                  !1.6
     &                     011002,      !- wind speed.
     &                     008040,007010, ! Flight level significance set to 60 for above jet !1.5c
                                        ! and flight level !1.5c
     &                     008040,007010, ! Flight level significance set to 61 for below jet !1.5c
                                        ! and flight level !1.5c
     &                     008040,      ! Cancel level sig. !1.5c				
     &                     008007,      !- cancel dimensional sig.
     &                     008011/      !- cancel met feature.

! ----------------------------------------------------------------------
! --- Read the number of jet streams. Add number of jet streams to
! --- values array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_JETS
      NELEM=NELEM+1
      VALUES(NELEM)=NUM_JETS        !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe a jet
! --- stream and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
        DESCR(NDESC+I)=IDES(JDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND            !- increment number of descriptors.
      

! ----------------------------------------------------------------------
! --- loop through number of jet streams if one or more.
! ----------------------------------------------------------------------

      IF (NUM_JETS.NE.0) THEN
            
        DO IJETS=1,NUM_JETS

! ----------------------------------------------------------------------
! --- Add value to values array to indicate a cloud area.
! --- Add value to values array specifying dimensional significance.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=10            !- desc 008011. Jet stream = 10.
          NELEM=NELEM+1
          VALUES(NELEM)=1             !- desc 008007. Line = 1.

! ----------------------------------------------------------------------
! --- Read the number of points used to describe the jet stream. Add
! --- this value to the values array.
! ----------------------------------------------------------------------

          READ(10,*) NUM_PNTS
          NELEM=NELEM+1
          VALUES(NELEM)=NUM_PNTS      !- corresponds to desc 031001.
                
! ----------------------------------------------------------------------
! --- loop through number of points describing jet stream. Read lat,
! --- lon, height, wind speed, top and base of jet stream and add to the 
! --- values array.
! ----------------------------------------------------------------------

          DO IPNTS=1,NUM_PNTS

            READ(10,*) LAT,LON,HEIGHT,WNDSPD,FLVLA,FLVLB !1.5c
	    
            NELEM=NELEM+1
            VALUES(NELEM)=LAT         !- corresponds to desc 005002.
            NELEM=NELEM+1
            VALUES(NELEM)=LON         !- corresponds to desc 006002.
            NELEM=NELEM+1
            VALUES(NELEM)=HEIGHT      !- corresponds to desc 007010.
            NELEM=NELEM+1
            VALUES(NELEM)=WNDSPD      !- corresponds to desc 011002.
	    NELEM=NELEM+1                                    !1.5c
	    VALUES(NELEM)=60.0        !- corresponds to desc 008040,      !1.5c
	                              !- Flight level significance above  !1.5c
	    NELEM=NELEM+1                                    !1.5c
	    VALUES(NELEM)=FLVLA       !- corresponds to desc 007010         !1.5c
	    NELEM=NELEM+1     
	    VALUES(NELEM)=61.0        !- corresponds to desc 008040,      !1.5c
	                              !- Flight level significance below  !1.5c
	    NELEM=NELEM+1                                    !1.5c
	    VALUES(NELEM)=FLVLB       !- corresponds to desc 007010         !1.5c	
	    NELEM=NELEM+1                                    !1.5c
	    VALUES(NELEM)=-9999999.0  !- corresponds to desc 008040,      !1.5c	
	                              !- Cancel previous 008040           !1.5c	      
             
          ENDDO  !- ipnts do loop.

! ----------------------------------------------------------------------
! --- End of jet stream section. Cancel dimensional significance
! --- and meteorological feature.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0      !- corresponds to desc 008007.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0      !- corresponds to desc 008011.

        ENDDO  !- ijets do loop.
      ENDIF  !- num_jets if block.

      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_turb subroutine (on HP)                          
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX turbulence areas   
!                : into BUFR.                                           
!                                                                     
! DESCRIPTION    : The SIGWX turbulence areas data are read from the   
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  : 
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors
!
! S.Cox 16/10/95 : New descriptors following WMO EMDR&C meeting
!
! S.Cox 01/09/95 : written
!                                                                      
!-----------------------------------------------------------------------
     
      SUBROUTINE ENC_TURB(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                    NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=13)         !- used to describe turb.

      INTEGER       MAXDESC             !- max no. of descriptors. !1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       ITURBS              !- loop counter.
      INTEGER       IPNTS               !- loop counter.
      INTEGER       NUM_TURBS           !- number of turbulence areas.
      INTEGER       NUM_PNTS            !- number of pnts in area.
      INTEGER       TDESCR(SECTND)      !- descriptors for TURB.
      INTEGER       TURBULENCE          !- degree of turbulence.
      INTEGER       NAMES_CNT           !- names postion indicator.
      REAL          LAT                 !- turbulence area lats.
      REAL          LON                 !- turbulence area lons.
      REAL          B_HEIGHT            !- turbulence base height.
      REAL          T_HEIGHT            !- turbulence top height.

      DATA          TDESCR/111000,      !- delayed replication.
     &                     031001,      !- no. of areas's to follow.
     &                     008011,      !- turbulence code table value.
     &                     008007,      !- dimensional significance.
     &                     007002,      !- height (base of layer).
     &                     007002,      !- height (top of layer).
     &                     102000,      !- delayed replication.
     &                     031001,      !- no. of points to follow.
     &                     005002,      !- latitude.
     &                     006002,      !- longitude.
     &                     011030,      !- degree of turbulence.      !1.6
     &                     008007,      !- cancel dimensional signif.
     &                     008011/      !- cancel turbulence section.

! ----------------------------------------------------------------------
! --- Read the number of areas. Add number of areas to values array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_TURBS
      NELEM=NELEM+1
      VALUES(NELEM)=NUM_TURBS        !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe a turb
! --- area and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
        DESCR(NDESC+I)=IDES(TDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND             !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- loop through number of areas if one or more.
! ----------------------------------------------------------------------

      IF (NUM_TURBS.NE.0) THEN
      
        DO ITURBS=1,NUM_TURBS

! ----------------------------------------------------------------------
! --- Add value to values array to indicate a cloud area.
! --- Add value to values array specifying dimensional significance.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=13            !- desc 008011. Turbulence = 13.
          NELEM=NELEM+1
          VALUES(NELEM)=2             !- desc 008007. Area = 2.

! ----------------------------------------------------------------------
! --- Read the base height of the area and the top height of the area.
! --- Add these values to the values array.
! ----------------------------------------------------------------------

          READ(10,*) B_HEIGHT,T_HEIGHT

          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT      !- corresponds to desc 007002.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT      !- corresponds to desc 007002.
                
! ----------------------------------------------------------------------
! --- Read the number of points used to describe the area. Add this
! --- value to the values array.
! ----------------------------------------------------------------------

          READ(10,*) NUM_PNTS
          NELEM=NELEM+1
          VALUES(NELEM)=NUM_PNTS      !- corresponds to desc 031001.
                
! ----------------------------------------------------------------------
! --- loop through number of points describing the area. Read lat and
! --- lon of the area and add to the values array.
! ----------------------------------------------------------------------

          DO IPNTS=1,NUM_PNTS

            READ(10,*) LAT,LON

            NELEM=NELEM+1
            VALUES(NELEM)=LAT         !- corresponds to desc 005002.
            NELEM=NELEM+1
            VALUES(NELEM)=LON         !- corresponds to desc 006002.
             
          ENDDO  !- ipnts do loop.

! ----------------------------------------------------------------------
! --- Read the degree of turbulence of the area and add to the values
! --- array.
! ----------------------------------------------------------------------

          READ(10,*) TURBULENCE

          NELEM=NELEM+1
          VALUES(NELEM)=TURBULENCE    !- corresponds to desc 011030.
 
! ----------------------------------------------------------------------
! --- End of turbulence section. Cancel dimensional significance and
! --- meteorological feature.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0      !- corresponds to desc 008007.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0      !- corresponds to desc 008011.

        ENDDO  !- iturbs do loop.
      ENDIF  !- num_turbs if block.

      RETURN
      END
      
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_storm subroutine (on HP)                         
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX storms             
!                : into BUFR.                                           
!                                                                     
! DESCRIPTION    : The SIGWX storms data are read from the             
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  : 
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors
!
! S.Cox 01/09/95 : written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE ENC_STORM(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                     NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=10)         !- used to describe cloud.

      INTEGER       MAXDESC             !- max no. of descriptors.!1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      INTEGER       NAMES_CNT           !- names postion indicator.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       ISTORMS             !- loop counter.
      INTEGER       NUM_STORMS          !- number of storms.
      INTEGER       TYP_STORM           !- type of storm.
      INTEGER       SDESCR(SECTND)      !- descriptors for storm.
      REAL          LAT                 !- storm lats.
      REAL          LON                 !- storm lons.
      CHARACTER*8   NAM_STORM           !- Storm name.

      DATA          SDESCR/108000,      !- delayed replication.
     &                     031001,      !- no. of storms to follow.
     &                     008005,      !- storm code table value.
     &                     008007,      !- dimensional sig.
     &                     001026,      !- storm name.
     &                     005002,      !- latitude.
     &                     006002,      !- longitude.
     &                     019001,      !- type of storm.
     &                     008007,      !- cancel dimensional sig.
     &                     008005/      !- end of storm section.

      NAM_STORM(:)=' '                  !- initialise storm name.
       
! ----------------------------------------------------------------------
! --- Read the number of storms. Add number of storms to values array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_STORMS
      NELEM=NELEM+1
      VALUES(NELEM)=NUM_STORMS      !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe a storm
! --- and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
        DESCR(NDESC+I)=IDES(SDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND            !- increment number of descriptors.
      
! ----------------------------------------------------------------------
! --- loop through number of storms if one or more. Add values to
! --- values array to indicate a storm centre and a point.
! ----------------------------------------------------------------------

      IF (NUM_STORMS.NE.0) THEN
      
        DO ISTORMS=1,NUM_STORMS

          NELEM=NELEM+1
          VALUES(NELEM)=1           !- corresponds to desc 008005.
          NELEM=NELEM+1
          VALUES(NELEM)=0           !- corresponds to desc 008007.
 
! ----------------------------------------------------------------------
! --- Read the storm name. The value in the values array corresponds to
! --- the displacement of the storm name in the names character string.
! --- Put the name of the storm in the names character string and update
! --- the names position indicator.
! ----------------------------------------------------------------------

          READ(10,'(A8)') NAM_STORM

          NELEM=NELEM+1
          VALUES(NELEM)=NAMES_CNT    !- corresponds to desc 001026.

          NAMES(NAMES_CNT:NAMES_CNT+7)=NAM_STORM
          NAMES_CNT=NAMES_CNT+8

! ----------------------------------------------------------------------
! --- Read the latitude and longitude of the storm centre.
! ----------------------------------------------------------------------

          READ(10,*) LAT,LON
          NELEM=NELEM+1
          VALUES(NELEM)=LAT      !- corresponds to desc 005002.
          NELEM=NELEM+1
          VALUES(NELEM)=LON      !- corresponds to desc 006002.
 
! ----------------------------------------------------------------------
! --- Read the type of storm and add this value to the values array.
! ----------------------------------------------------------------------

          READ(10,*) TYP_STORM

          NELEM=NELEM+1
          VALUES(NELEM)=TYP_STORM     !- corresponds to desc 019001.

! ----------------------------------------------------------------------
! --- End of storm section. Cancel dimensional sig and storm
! --- co-ordinate descriptor.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0      !- corresponds to desc 008007.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0      !- corresponds to desc 008005.

        ENDDO  !- istorms loop.
      ENDIF  !- num_storms if block.

      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_cloud subroutine (on HP)                         
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX cloud areas        
!                : into BUFR.                                           
!                                                                     
! DESCRIPTION    : The SIGWX cloud areas data are read from the        
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  : 
!
! S.Cox 23/12/99 : Could amount 020011 changed to cloud distribution 
!                  020008 at request of Horace SigWx team
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors
!
! S.Cox 16/10/95 : New descriptors following WMO EMDR&C meeting
!
! S.Cox 01/09/95 : written
!                                                                      
!-----------------------------------------------------------------------
     
      SUBROUTINE ENC_CLOUD(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                     NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       MAXDESC             !- max no. of descriptors.!1.5a
      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=14)         !- used to describe cloud.

      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       ICLOUDS             !- loop counter.
      INTEGER       IPNTS               !- loop counter.
      INTEGER       NUM_CLOUDS          !- number of cloud areas.
      INTEGER       NUM_PNTS            !- number of pnts in area.
      INTEGER       CDESCR(SECTND)      !- descriptors for clouds.
      INTEGER       NAMES_CNT           !- names postion indicator.
      REAL          LAT                 !- cloud area lats.
      REAL          LON                 !- cloud area lons.
      REAL          B_HEIGHT            !- cloud area base height.
      REAL          T_HEIGHT            !- cloud area top height.
      REAL          C_AMOUNT            !- cloud amount.
      REAL          C_TYPE              !- cloud type.

      DATA          CDESCR/112000,      !- delayed replication.
     &                     031001,      !- no. of cloud areas to follow.
     &                     008011,      !- cloud code table value.
     &                     008007,      !- dimensional significance.
     &                     007002,      !- height (base of layer).
     &                     007002,      !- height (top of layer).
     &                     102000,      !- delayed replication.
     &                     031001,      !- no. of points to follow.
     &                     005002,      !- latitude.
     &                     006002,      !- longitude.
     &                     020008,      !- cloud distribution.
     &                     020012,      !- cloud type.
     &                     008007,      !- cancel dimensional signif.
     &                     008011/      !- cancel cloud area section.

! ----------------------------------------------------------------------
! --- Read the number of cloud areas. Add number of areas to values
! --- array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_CLOUDS
      NELEM=NELEM+1
      VALUES(NELEM)=NUM_CLOUDS      !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe a cloud
! --- area and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
        DESCR(NDESC+I)=IDES(CDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND            !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- loop through number of areas if one or more.
! ----------------------------------------------------------------------
      
      IF (NUM_CLOUDS.NE.0) THEN
      
        DO ICLOUDS=1,NUM_CLOUDS

! ----------------------------------------------------------------------
! --- Add value to values array to indicate a cloud area.
! --- Add value to values array specifying dimensional significance.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=12          !- desc 008011. Cloud = 12.
          NELEM=NELEM+1
          VALUES(NELEM)=2           !- desc 008007. Area = 2.

! ----------------------------------------------------------------------
! --- Read the base height of the area and the top height of the area.
! --- Add these values to the values array.
! ----------------------------------------------------------------------

          READ(10,*) B_HEIGHT,T_HEIGHT

          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT    !- corresponds to desc 007002.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT    !- corresponds to desc 007002.
                
! ----------------------------------------------------------------------
! --- Read the number of points used to describe the area. Add this
! --- value to the values array.
! ----------------------------------------------------------------------

          READ(10,*) NUM_PNTS
          NELEM=NELEM+1
          VALUES(NELEM)=NUM_PNTS    !- corresponds to desc 031001.
                
! ----------------------------------------------------------------------
! --- loop through number of points describing the area. Read lat and
! --- lon of the area and add to the values array.
! ----------------------------------------------------------------------

          DO IPNTS=1,NUM_PNTS

            READ(10,*) LAT,LON

            NELEM=NELEM+1
            VALUES(NELEM)=LAT       !- corresponds to desc 005002.
            NELEM=NELEM+1
            VALUES(NELEM)=LON       !- corresponds to desc 006002.
              
          ENDDO  !- ipnts do loop.

! ----------------------------------------------------------------------
! --- Read the cloud distribution and the cloud type for this area of 
! --- cloud and add these to the vales array.
! ----------------------------------------------------------------------

          READ(10,*) C_AMOUNT,C_TYPE

          NELEM=NELEM+1
          VALUES(NELEM)=C_AMOUNT    !- corresponds to desc 020008.
          NELEM=NELEM+1
          VALUES(NELEM)=C_TYPE      !- corresponds to desc 020012.

! ----------------------------------------------------------------------
! --- End of cloud area section. Cancel dimensional significance
! --- and meteorological feature.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008007.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008011.

        ENDDO  !- iclouds do loop.
      ENDIF  !- num_clouds if block.

      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_mcloud subroutine (on HP)                         
!                                                                      
! PURPOSE        : a pre-step to encoding the medium level SIGWX cloud 
!                : areas into BUFR.                                           
!                                                                     
! DESCRIPTION    : The SIGWX cloud areas data are read from the        
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  : 
!
! A.Weir 12/07/06 : Corrected comments describing descriptors.    !ST 3
! A.Weir 21/09/04 : written (enc_sigwx v1.6)
!                                                                      
!-----------------------------------------------------------------------
     
      SUBROUTINE ENC_MCLOUD(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                     NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       MAXDESC             !- max no. of descriptors.!1.5a
      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=28)         !- used to describe cloud.

      INTEGER       MAXDIST             !- max no. of cloud
      PARAMETER     (MAXDIST=17)        !- distributions.
      INTEGER       MAXTYPE             !- max no. of cloud
      PARAMETER     (MAXTYPE=63)        !- types.

      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       IAREA               !- loop counter.
      INTEGER       IDISTS              !- loop counter.
      INTEGER       IPNTS               !- loop counter.
      INTEGER       ITYPES              !- loop counter.
      INTEGER       NUM_AREAS           !- number of cloud areas.
      INTEGER       NUM_DISTS           !- number of cloud distributions.
      INTEGER       NUM_PNTS            !- number of pnts in area.
      INTEGER       NUM_TYPES           !- number of cloud types.
      INTEGER       CDESCR(SECTND)      !- descriptors for clouds.
      INTEGER       DEGREE              !- degree (of turbulence or icing).
      INTEGER       C_DIST(MAXDIST)     !- cloud distributionS.
      INTEGER       C_TYPE(MAXTYPE)     !- cloud types.
      INTEGER       NAMES_CNT           !- names postion indicator.
      INTEGER       CB                  !- 1 if Cb present.
      INTEGER       ICING               !- 1 if icing present.
      INTEGER       TURB                !- 1 if turbulence present.
      REAL          LAT                 !- cloud area lats.
      REAL          LON                 !- cloud area lons.
      REAL          B_HEIGHT            !- cloud area base height.
      REAL          T_HEIGHT            !- cloud area top height.

      DATA          CDESCR/008011,008007, !- cloud code table value !ST 3
                                          !- dimensional significance (area) !ST 3
     &                     102000,031001, !- delayed replication  !ST 3
                                          !- no. of points to follow!ST 3
     &                     005002,006002, !- latitude,longitude     !ST 3
     &                     101000,031001, !- delayed replication  !ST 3
                                          !- no. of non-CB cloud distributions !ST 3
     &                     020008,101000, !- cloud distribution   !ST 3
                                          !- delayed replication  !ST 3
     &                     031001,020012, !- no. of non-CB cloud types !ST 3
                                          !- cloud types          !ST 3
     &                     103000,031000, !- delayed replication  !ST 3
                                          !- 1 if turbulence, 0 if not !ST 3
     &                     007010,007010, !- turbulence base, top !ST 3
     &                     011030,103000, !- degree of turbulence !ST 3
                                          !- delayed replication  !ST 3
     &                     031000,007010, !- 1 if icing, 0 if not !ST 3
                                          !- icing base           !ST 3
     &                     007010,020041, !- icing top            !ST 3
                                          !- degree of icing      !ST 3
     &                     104000,031000, !- delayed replication  !ST 3
                                          !- 1 if CB, 0 if not    !ST 3
     &                     007010,007010, !- CB base              !ST 3
                                          !- CB top               !ST 3
     &                     020008,020012/ !- CB cloud distribution!ST 3
                                          !- cloud type           !ST 3
      
! ----------------------------------------------------------------------
! --- Add the first descriptor, indicating a cloud section, to the
! --- DESCR array and increment NDESC.
! ----------------------------------------------------------------------
        
      DESCR(NDESC+1)=IDES(CDESCR(1))
      NDESC=NDESC+1            !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- Add value to values array to indicate a cloud area.
! ----------------------------------------------------------------------
 
      NELEM=NELEM+1
      VALUES(NELEM)=12              !- desc 008011. Cloud = 12.

! ----------------------------------------------------------------------
! --- Read in the number of cloud areas to follow. This value is not 
! --- stored in the BUFR message, but is just used to loop around.
! ----------------------------------------------------------------------

      READ(10,*) NUM_AREAS
    
      DO IAREA=1,NUM_AREAS

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe a cloud
! --- area and put in DESCR array. Increment NDESC.
! --- The first descriptor has already been included at the start, and  
! --- is not repeated for each cloud area.
! ----------------------------------------------------------------------

        DO I=1,SECTND-1
          DESCR(NDESC+I)=IDES(CDESCR(I+1))
        ENDDO
        NDESC=NDESC+SECTND-1            !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- Add value to values array specifying dimensional significance.
! ----------------------------------------------------------------------

        NELEM=NELEM+1
        VALUES(NELEM)=2               !- desc 008007. Area = 2.

! ----------------------------------------------------------------------
! --- Read the the number of points round area and add to values array.
! ----------------------------------------------------------------------

        READ(10,*) NUM_PNTS
      
        NELEM=NELEM+1
        VALUES(NELEM)=NUM_PNTS        !- corresponds to desc 031001

! ----------------------------------------------------------------------
! --- Loop round the number of points used to describe the area.
! --- Read the latitude and longitude of each point and add these 
! --- values to the values array.
! ----------------------------------------------------------------------

        DO IPNTS=1,NUM_PNTS

          READ(10,*) LAT,LON

          NELEM=NELEM+1
          VALUES(NELEM)=LAT          !- corresponds to desc 005002.
          NELEM=NELEM+1
          VALUES(NELEM)=LON          !- corresponds to desc 006002.

        ENDDO                        !- ipnts do loop.
                
! ----------------------------------------------------------------------
! --- Read the number of non-Cb cloud distributions.
! --- Add to values array.
! ----------------------------------------------------------------------

        READ(10,*) NUM_DISTS
       
        NELEM=NELEM+1
        VALUES(NELEM)=NUM_DISTS            !- corresponds to desc 031001

! ----------------------------------------------------------------------
! --- If there are one or more non-Cb cloud distributions,
! --- read each cloud distribution code and add to the values array.
! ----------------------------------------------------------------------

        IF (NUM_DISTS.GE.1) THEN
          
	  READ(10,*) (C_DIST(IDISTS),IDISTS=1,NUM_DISTS)

          DO IDISTS=1,NUM_DISTS
            NELEM=NELEM+1
            VALUES(NELEM)=C_DIST(IDISTS)   !- corresponds to desc 020008.
          ENDDO                            !- idists do loop.
        ENDIF	
                
! ----------------------------------------------------------------------
! --- Read the number of non-Cb cloud types.
! --- Add to values array.
! ----------------------------------------------------------------------

        READ(10,*) NUM_TYPES

        NELEM=NELEM+1
        VALUES(NELEM)=NUM_TYPES            !- corresponds to desc 031001

! ----------------------------------------------------------------------
! --- If there are one or more non-Cb cloud types,
! --- read each cloud type code and add to the values array.
! ----------------------------------------------------------------------

        IF (NUM_TYPES.GE.1) THEN
          
	  READ(10,*) (C_TYPE(ITYPES),ITYPES=1,NUM_TYPES)
	  
          DO ITYPES=1,NUM_TYPES
            NELEM=NELEM+1
            VALUES(NELEM)=C_TYPE(ITYPES)   !- corresponds to desc 020012.
          ENDDO                            !- itypes do loop.
        ENDIF	
                
! ----------------------------------------------------------------------
! --- Read the the optional turbulence section indicator.
! --- Add to values array.
! ----------------------------------------------------------------------

        READ(10,*) TURB

        NELEM=NELEM+1
        VALUES(NELEM)=TURB          !- corresponds to desc 031000

!----------------------------------------------------------------------
! --- If there is a turbulence section (TURB=1), read the base height,
! --- top height and degree of turbulence.
! --- Add to values array.
! ----------------------------------------------------------------------

        IF (TURB.EQ.1) THEN

          READ(10,*) B_HEIGHT,T_HEIGHT
          READ(10,*) DEGREE

          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT    !- corresponds to desc 007010.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT    !- corresponds to desc 007010.
          NELEM=NELEM+1
          VALUES(NELEM)=DEGREE      !- corresponds to desc 011030.

        ENDIF
                
! ----------------------------------------------------------------------
! --- Read the the optional icing section indicator.
! --- Add to values array.
! ----------------------------------------------------------------------

        READ(10,*) ICING

        NELEM=NELEM+1
        VALUES(NELEM)=ICING        !- corresponds to desc 031000

! ----------------------------------------------------------------------
! --- If there is an icing section (ICING=1), read the base height,
! --- top height and degree of icing.
! --- Add to values array.
! ----------------------------------------------------------------------

        IF (ICING.EQ.1) THEN

          READ(10,*) B_HEIGHT,T_HEIGHT
          READ(10,*) DEGREE

          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT   !- corresponds to desc 007010.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT   !- corresponds to desc 007010.
          NELEM=NELEM+1
          VALUES(NELEM)=DEGREE     !- corresponds to desc 020041.

        ENDIF

! ----------------------------------------------------------------------
! --- Read the the optional Cb section indicator.
! --- Add to values array.
! ----------------------------------------------------------------------

        READ(10,*) CB

        NELEM=NELEM+1
        VALUES(NELEM)=CB           !- corresponds to desc 031000

! ----------------------------------------------------------------------
! --- If there is a Cb section (CB=1), read the base height,
! --- top height, cloud distribution and cloud type.
! --- Add to values array.
! ----------------------------------------------------------------------

        IF (CB.EQ.1) THEN

          READ(10,*) B_HEIGHT,T_HEIGHT
          READ(10,*) C_DIST(1)
          READ(10,*) C_TYPE(1)
             
          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT   !- corresponds to desc 007010.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT   !- corresponds to desc 007010.
          NELEM=NELEM+1
          VALUES(NELEM)=C_DIST(1)  !- corresponds to desc 020008.
          NELEM=NELEM+1
          VALUES(NELEM)=C_TYPE(1)  !- corresponds to desc 020012.

        ENDIF
                
! ----------------------------------------------------------------------
! --- End of medium level cloud area. 
! ----------------------------------------------------------------------

      ENDDO                       !- num_areas loop
  
      RETURN
      END
      
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_front subroutine (on HP)                         
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX fronts             
!                : into BUFR.                                           
!                                                                      
! DESCRIPTION    : The SIGWX icing fronts are read from the            
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors
!
! S.Cox 16/10/95 : New descriptors following WMO EMDR&C meeting
!
! S.Cox 01/09/95 : written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE ENC_FRONT(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                     NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=12)         !- used to describe front.

      INTEGER       MAXDESC             !- max no. of descriptors. !1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       IFRONTS             !- loop counter.
      INTEGER       IPNTS               !- loop counter.
      INTEGER       NUM_FRONTS          !- number of fronts.
      INTEGER       NUM_PNTS            !- number of pnts in front.
      INTEGER       FDESCR(SECTND)      !- descriptors for front.
      INTEGER       NAMES_CNT           !- names postion indicator.
      INTEGER       TYP_FRONT           !- type of front.
      REAL          LAT                 !- front lats.
      REAL          LON                 !- front lons.
      REAL          F_DIR               !- front wind speeds.
      REAL          F_SPEED             !- front heights.

      DATA          FDESCR/110000,      !- delayed replication.
     &                     031001,      !- no. of fronts to follow.
     &                     008011,      !- type of front.
     &                     008007,      !- dimensional significance.
     &                     104000,      !- delayed replication.
     &                     031001,      !- no. of points to follow.
     &                     005002,      !- latitude.
     &                     006002,      !- longitude.
     &                     019005,      !- front direction.
     &                     019006,      !- front speed.
     &                     008007,      !- cancel dimensional signif.
     &                     008011/      !- cancel front section.

! ----------------------------------------------------------------------
! --- Read the number of fronts. Add number of fronts to values array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_FRONTS
      NELEM=NELEM+1
      VALUES(NELEM)=NUM_FRONTS    !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe a cloud
! --- area and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
        DESCR(NDESC+I)=IDES(FDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND            !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- loop through number of fronts if one or more. Read in the type
! --- of front.
! ----------------------------------------------------------------------

      IF (NUM_FRONTS.NE.0) THEN
            
        DO IFRONTS=1,NUM_FRONTS

          READ(10,*) TYP_FRONT
                  
! ----------------------------------------------------------------------
! --- Add value to values array to indicate type of front.
! --- Add value to values array specifying dimensional significance.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=TYP_FRONT   !- desc 008011. Type of front.
          NELEM=NELEM+1
          VALUES(NELEM)=1           !- desc 008007. Line = 1.

! ----------------------------------------------------------------------
! --- Read the number of points used to describe the front. Add this
! --- value to the values array.
! ----------------------------------------------------------------------

          READ(10,*) NUM_PNTS
          NELEM=NELEM+1
          VALUES(NELEM)=NUM_PNTS    !- corresponds to desc 031001.
                 
! ----------------------------------------------------------------------
! --- loop through number of points describing the front. Read lat, lon,
! --- direction and speed of the front and add to the values array.
! ----------------------------------------------------------------------

          DO IPNTS=1,NUM_PNTS

            READ(10,*) LAT,LON,F_DIR,F_SPEED
            NELEM=NELEM+1
            VALUES(NELEM)=LAT       !- corresponds to desc 005002.
            NELEM=NELEM+1
            VALUES(NELEM)=LON       !- corresponds to desc 006002.
            NELEM=NELEM+1
            VALUES(NELEM)=F_DIR     !- corresponds to desc 019005.
            NELEM=NELEM+1
            VALUES(NELEM)=F_SPEED   !- corresponds to desc 019006.
             
          ENDDO  !- ipnts do loop.

! ----------------------------------------------------------------------
! --- End of front section. Cancel dimensional significance and
! --- meteorological feature.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008007.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008011.
  
        ENDDO  !- ifronts do loop.
      ENDIF  !- num_fronts if block.
   
      RETURN
      END
      
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_trop subroutine (on HP)                          
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX tropopauses        
!                : into BUFR.                                           
!                                                                      
! DESCRIPTION    : The SIGWX tropopause data are read from the         
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors
!
! S.Cox 01/09/95 : written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE ENC_TROP(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                    NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=13)         !- used to describe trop.

      INTEGER       MAXDESC             !- max no. of descriptors.!1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       ITROPS              !- loop counter.
      INTEGER       IPNTS               !- loop counter.
      INTEGER       NUM_TROPS           !- number of tropopause groups.
      INTEGER       NUM_PNTS            !- number of tropopause points.
      INTEGER       TDESCR(SECTND)      !- descriptors for tropopause.
      INTEGER       NAMES_CNT           !- names postion indicator.
      INTEGER       TYP_TROP            !- type of tropopause value.
      REAL          LAT                 !- tropopause lats.
      REAL          LON                 !- tropopause lons.
      REAL          HEIGHT              !- tropopause heights.

      DATA          TDESCR/111000,      !- delayed replication.
     &                     031001,      !- no. of trop groups to follow.
     &                     008001,      !- vertical significance.
     &                     008007,      !- dimensional sig.
     &                     008023,      !- type of tropopause value.
     &                     103000,      !- delayed replication.
     &                     031001,      !- no. of trop points to follow.
     &                     005002,      !- latitude
     &                     006002,      !- longitude.
     &                     010002,      !- height of tropopause.
     &                     008023,      !- cancel statistic section.
     &                     008007,      !- cancel dimensional sig.
     &                     008001/      !- cancel vertical sig.

! ----------------------------------------------------------------------
! --- Read the number of trop groups. Add this number to values array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_TROPS

      NELEM=NELEM+1
      VALUES(NELEM)=NUM_TROPS    !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe a trop
! --- point and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
         DESCR(NDESC+I)=IDES(TDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND           !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- loop through number of trop groups if one or more
! ----------------------------------------------------------------------

      IF (NUM_TROPS.NE.0) THEN
            
        DO ITROPS=1,NUM_TROPS
    
! ----------------------------------------------------------------------
! --- Set bit 3 of descriptor 008001 to indicate a tropopause value.
! --- Values worked out as 2^(w-n) where w is the total bit width of
! --- the table and bit n is what we want to represent. So to set bit 3
! --- we do 2^(7-3) =2^4 = 16. Setting bit 3 indicates the tropopause.
! --- Add value to values array specifying dimensional significance.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=16         !- corresponds to desc 008001.
          NELEM=NELEM+1
          VALUES(NELEM)=0          !- corresponds to desc 008007.

! ----------------------------------------------------------------------
! --- Read in the type of tropopause group (min, max or spot). Put this
! --- in the values array.
! ----------------------------------------------------------------------

          READ(10,*) TYP_TROP

        
          NELEM=NELEM+1
          VALUES(NELEM)=TYP_TROP   !- corresponds to desc 008023.

! ----------------------------------------------------------------------
! --- Read the number of tropopause values in each group. Add this value
! --- to the values array.
! ----------------------------------------------------------------------

          READ(10,*) NUM_PNTS
          NELEM=NELEM+1
          VALUES(NELEM)=NUM_PNTS    !- corresponds to desc 031001.
                
! ----------------------------------------------------------------------
! --- loop through number of tropopause values. Read lat, lon and trop
! --- height and add these to the values array.
! ----------------------------------------------------------------------

          DO IPNTS=1,NUM_PNTS

            READ(10,*) LAT,LON,HEIGHT

            NELEM=NELEM+1
            VALUES(NELEM)=LAT       !- corresponds to desc 005002.
            NELEM=NELEM+1
            VALUES(NELEM)=LON       !- corresponds to desc 006002.
            NELEM=NELEM+1
            VALUES(NELEM)=HEIGHT    !- corresponds to desc 010002.
             
          ENDDO  !- ipnts do loop.

! ----------------------------------------------------------------------
! --- End of tropopause section. Cancel statistic co-ordinate,
! --- dimensional significance and meteorological feature.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008023.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008007.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008001.

        ENDDO  !- itrops do loop.
      ENDIF  !- num_trops if block.
   
      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_icing subroutine (on HP)                         
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX icing areas        
!                : into BUFR.                                           
!                                                                      
! DESCRIPTION    : The SIGWX icing areas data are read from the        
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors
!
! S.Cox 16/10/95 : New descriptors following WMO EMDR&C meeting
!
! S.Cox 01/09/95 : written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE ENC_ICING(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                     NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=13)         !- used to describe icing.

      INTEGER       MAXDESC             !- max no. of descriptors.!1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       IICES               !- loop counter.
      INTEGER       IPNTS               !- loop counter.
      INTEGER       NUM_ICES            !- number of icing areas.
      INTEGER       NUM_PNTS            !- number of pnts in area.
      INTEGER       IDESCR(SECTND)      !- descriptors for ICING.
      INTEGER       ICETYPE             !- type of icing.
      INTEGER       NAMES_CNT           !- names postion indicator.
      REAL          LAT                 !- icing area lats.
      REAL          LON                 !- icing area lons.
      REAL          B_HEIGHT            !- icing base height.
      REAL          T_HEIGHT            !- icing top height.

      DATA          IDESCR/111000,      !- delayed replication.
     &                     031001,      !- no. of areas's to follow.
     &                     008011,      !- icing code table value.
     &                     008007,      !- dimensional significance.
     &                     007002,      !- height (base of layer).
     &                     007002,      !- height (top of layer).
     &                     102000,      !- delayed replication.
     &                     031001,      !- no. of points to follow.
     &                     005002,      !- latitude.
     &                     006002,      !- longitude.
     &                     020041,      !- type of icing.
     &                     008007,      !- cancel dimensional signif.
     &                     008011/      !- cancel icing section.

! ----------------------------------------------------------------------
! --- Read the number of areas. Add number of areas to values array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_ICES
      NELEM=NELEM+1
      VALUES(NELEM)=NUM_ICES       !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe an icing
! --- area and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
        DESCR(NDESC+I)=IDES(IDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND             !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- loop through number of areas if one or more.
! ----------------------------------------------------------------------

      IF (NUM_ICES.NE.0) THEN
      
        DO IICES=1,NUM_ICES

! ----------------------------------------------------------------------
! --- Add value to values array to indicate icing area.
! --- Add value to values array specifying dimensional significance.
! ----------------------------------------------------------------------

         NELEM=NELEM+1
         VALUES(NELEM)=15          !- desc 008011. Icing = 15.
         NELEM=NELEM+1
         VALUES(NELEM)=2           !- desc 008007. Area = 2.
         
! ----------------------------------------------------------------------
! --- Read the base height of the area and the top height of the area.
! --- Add these values to the values array.
! ----------------------------------------------------------------------

          READ(10,*) B_HEIGHT,T_HEIGHT

          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT    !- corresponds to desc 007002.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT    !- corresponds to desc 007002.
                
! ----------------------------------------------------------------------
! --- Read the number of points used to describe the area. Add this
! --- value to the values array.
! ----------------------------------------------------------------------

          READ(10,*) NUM_PNTS
          NELEM=NELEM+1
          VALUES(NELEM)=NUM_PNTS    !- corresponds to desc 031001.
                
! ----------------------------------------------------------------------
! --- loop through number of points describing the area. Read lat and
! --- lon of the area and add to the values array.
! ----------------------------------------------------------------------

          DO IPNTS=1,NUM_PNTS

            READ(10,*) LAT,LON

            NELEM=NELEM+1
            VALUES(NELEM)=LAT       !- corresponds to desc 005002.
            NELEM=NELEM+1
            VALUES(NELEM)=LON       !- corresponds to desc 006002.
             
          ENDDO  !- ipnts do loop.

! ----------------------------------------------------------------------
! --- Read the type of icing and add to the values array.
! ----------------------------------------------------------------------

          READ(10,*) ICETYPE

          NELEM=NELEM+1
          VALUES(NELEM)=ICETYPE     !- corresponds to desc 020041.
 
! ----------------------------------------------------------------------
! --- End of icing section. Cancel dimensional significance
! --- and meteorological feature.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008007.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008011.

        ENDDO  !- iices do loop.
      ENDIF  !- num_ices if block.

      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_volcano subroutine (on HP)                       
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX volcanoes          
!                : into BUFR.                                           
!                                                                      
! DESCRIPTION    : The SIGWX volcanoes data are read from the          
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 29/10/97 : Volcano name (160 bits) changed to Name Of 
!                  Feature (224 bits)
!
! S.Cox 16/02/96 : Changed descriptors or order of decriptors
!
! S.Cox 16/10/95 : New descriptors following WMO EMDR&C meeting
!
! S.Cox 01/09/95 : written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE ENC_VOLCANO(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                       NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=19)         !- used to describe volcano.

      INTEGER       MAXDESC             !- max no. of descriptors. !1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      INTEGER       NAMES_CNT           !- names postion indicator.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       IVOLCS              !- loop counter.
      INTEGER       NUM_VOLCS           !- number of volcanos.
      INTEGER       VDESCR(SECTND)      !- descriptors for volcano.
      INTEGER       YEAR                !- volcano eruption year.
      INTEGER       MONTH               !- volcano eruption month.
      INTEGER       DAY                 !- volcano eruption day.
      INTEGER       HOUR                !- volcano eruption hour.
      INTEGER       MINUTE              !- volcano eruption minute.
      REAL          LAT                 !- volcano lats.
      REAL          LON                 !- volcano lons.
      CHARACTER*28  NAM_VOLCANO         !- volcano name.

      DATA          VDESCR/117000,      !- delayed replication.
     &                     031001,      !- no. of volcanos.
     &                     008011,      !- meteorological feature.
     &                     001022,      !- volcano name.
     &                     008007,      !- dimensional significance.
     &                     102000,      !- delayed replication.
     &                     031001,      !- number of points
     &                     005002,      !- volcano latitude.
     &                     006002,      !- volcano longitude.
     &                     008021,      !- time sig OF ERUPTION.
     &                     004001,      !- year.
     &                     004002,      !- month.
     &                     004003,      !- day.
     &                     004004,      !- hour.
     &                     004005,      !- minute.
     &                     020090,      !- special phenomena clouds.
     &                     008021,      !- cancel time signif.
     &                     008007,      !- cancel dimensional signif.
     &                     008011/      !- cancel met feature.

      NAM_VOLCANO(:)=' '                !- initialise volcano name.
       
! ----------------------------------------------------------------------
! --- Read the number of volcanos. Add number to values array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_VOLCS
      NELEM=NELEM+1
      VALUES(NELEM)=NUM_VOLCS     !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe a volcano
! --- and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
        DESCR(NDESC+I)=IDES(VDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND            !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- Loop through number of volcanos if one or more.
! ----------------------------------------------------------------------

      IF (NUM_VOLCS.NE.0) THEN
            
        DO IVOLCS=1,NUM_VOLCS
        
! ----------------------------------------------------------------------
! --- Add value to values array indicating volcano.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=17         !- desc 008011. volcano = 17

! ----------------------------------------------------------------------
! --- Read the name of the volcano. The value in the values array 
! --- corresponds to the displacement of the volcano name in the names
! --- character string. Put the name of the volcano in the names
! --- character string and update the names position indicator.
! ----------------------------------------------------------------------

          READ(10,'(A28)') NAM_VOLCANO
          NELEM=NELEM+1
          VALUES(NELEM)=NAMES_CNT  !- corresponds to desc 001022.

          NAMES(NAMES_CNT:NAMES_CNT+27)=NAM_VOLCANO
          NAMES_CNT=NAMES_CNT+28

! ----------------------------------------------------------------------
! --- Add value to values array indicating dimensional significance.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=0          !- desc 008007. Point = 0.
 
! ----------------------------------------------------------------------
! --- Number of lat/lon points. For SIGWX this is 1, the point of the
! --- erupting volcano.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=1          !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Read the latitude and longitude of the volcano
! ----------------------------------------------------------------------

          READ(10,*) LAT,LON
          NELEM=NELEM+1
          VALUES(NELEM)=LAT    !- corresponds to desc 005002.
          NELEM=NELEM+1
          VALUES(NELEM)=LON    !- corresponds to desc 006002.

! ----------------------------------------------------------------------
! --- Add time significance (observed = 17) to the values array.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=17     !- corresponds to desc 008021.

! ----------------------------------------------------------------------
! --- Read the eruption year, month, day, hour, minute.
! ----------------------------------------------------------------------

          READ(10,*) YEAR,MONTH,DAY,HOUR,MINUTE
          NELEM=NELEM+1
          VALUES(NELEM)=YEAR    !- corresponds to desc 004001.
          NELEM=NELEM+1
          VALUES(NELEM)=MONTH   !- corresponds to desc 004002.
          NELEM=NELEM+1
          VALUES(NELEM)=DAY     !- corresponds to desc 004003.
          NELEM=NELEM+1
          VALUES(NELEM)=HOUR    !- corresponds to desc 004004.
          NELEM=NELEM+1
          VALUES(NELEM)=MINUTE  !- corresponds to desc 004005.

! ----------------------------------------------------------------------
! --- Add value to values array. It is "special clouds", code value = 5
! --- "clouds from volcanic eruptions"
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=5   !- corresponds to desc 020090.

! ----------------------------------------------------------------------
! --- End of volcano section. Cancel time significance, dimensional
! --- significance and meteorological feature.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008021.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008007.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008011.

        ENDDO  !- ivolcs loop.
      ENDIF  !- num_volcs if block.

      RETURN
      END
      
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_radiation subroutine (on HP)                       
!                                                                      
! PURPOSE        : a pre-step to encoding the SIGWX radiation symbols          
!                : into BUFR.                                           
!                                                                      
! DESCRIPTION    : The SIGWX radiation data are read from the          
!                : SIGWX browsable dataset. From this, the BUFR        
!                : descriptors and values arrays are updated to        
!                : contain this information and passed back to the     
!                : calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 25/02/00 : written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE ENC_RADIATION(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                         NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=15)         !- used to describe radiation.

      INTEGER       MAXDESC             !- max no. of descriptors.!1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- radiation array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      INTEGER       NAMES_CNT           !- names postion indicator.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       IREACS              !- loop counter.
      INTEGER       NUM_REACS           !- number of reactors.
      INTEGER       RDESCR(SECTND)      !- descriptors for radiation.
      INTEGER       YEAR                !- reactor release year.
      INTEGER       MONTH               !- reactor release month.
      INTEGER       DAY                 !- reactor release day.
      INTEGER       HOUR                !- reactor release hour.
      INTEGER       MINUTE              !- reactor release minute.
      REAL          LAT                 !- reactor lat.
      REAL          LON                 !- reactor lon.
      CHARACTER*28  NAM_REACTOR         !- reactor name.

      DATA          RDESCR/113000,      !- delayed replication.
     &                     031001,      !- no. of reactors.
     &                     023002,      !- feature.
     &                     008007,      !- dimensional significance.
     &                     001022,      !- reactor name.
     &                     005002,      !- reactor latitude.
     &                     006002,      !- reactor longitude.
     &                     008021,      !- time sig of release.
     &                     004001,      !- year.
     &                     004002,      !- month.
     &                     004003,      !- day.
     &                     004004,      !- hour.
     &                     004005,      !- minute.
     &                     008021,      !- cancel time signif.
     &                     008007/      !- cancel dimensional signif.

      NAM_REACTOR(:)=' '                !- initialise reactor name.
       
! ----------------------------------------------------------------------
! --- Read the number of radiation symbols. Add number to values array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_REACS
      NELEM=NELEM+1
      VALUES(NELEM)=NUM_REACS     !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe a
! --- radiation symbol and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
        DESCR(NDESC+I)=IDES(RDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND            !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- Loop through number of radiation symbols if one or more.
! ----------------------------------------------------------------------

      IF (NUM_REACS.NE.0) THEN
            
        DO IREACS=1,NUM_REACS
        
! ----------------------------------------------------------------------
! --- Add value to values array indicating radiation/reactor. Descriptor
! --- used is 023002 "Activity or facility involved in incident". This
! --- is not a significance descriptor, but I don't think it really
! --- matters.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=1          !- desc 023002. reactor on ground = 1

! ----------------------------------------------------------------------
! --- Add value to values array indicating dimensional significance.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=0          !- desc 008007. Point = 0.

! ----------------------------------------------------------------------
! --- Read the name of the reactor. The value in the values array 
! --- corresponds to the displacement of the reactor name in the names
! --- character string. Put the name of the reactor in the names
! --- character string and update the names position indicator.
! ----------------------------------------------------------------------

          READ(10,'(A28)') NAM_REACTOR
          NELEM=NELEM+1
          VALUES(NELEM)=NAMES_CNT  !- corresponds to desc 001022.

          NAMES(NAMES_CNT:NAMES_CNT+27)=NAM_REACTOR
          NAMES_CNT=NAMES_CNT+28

! ----------------------------------------------------------------------
! --- Read the latitude and longitude of the reactor
! ----------------------------------------------------------------------

          READ(10,*) LAT,LON
          NELEM=NELEM+1
          VALUES(NELEM)=LAT    !- corresponds to desc 005002.
          NELEM=NELEM+1
          VALUES(NELEM)=LON    !- corresponds to desc 006002.

! ----------------------------------------------------------------------
! --- Add time significance (observed = 17) to the values array.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=17     !- corresponds to desc 008021.

! ----------------------------------------------------------------------
! --- Read the release year, month, day, hour, minute.
! ----------------------------------------------------------------------

          READ(10,*) YEAR,MONTH,DAY,HOUR,MINUTE
          NELEM=NELEM+1
          VALUES(NELEM)=YEAR    !- corresponds to desc 004001.
          NELEM=NELEM+1
          VALUES(NELEM)=MONTH   !- corresponds to desc 004002.
          NELEM=NELEM+1
          VALUES(NELEM)=DAY     !- corresponds to desc 004003.
          NELEM=NELEM+1
          VALUES(NELEM)=HOUR    !- corresponds to desc 004004.
          NELEM=NELEM+1
          VALUES(NELEM)=MINUTE  !- corresponds to desc 004005.

! ----------------------------------------------------------------------
! --- End of reactor section. Cancel time significance and dimensional
! --- significance.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008021.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008007.

        ENDDO  !- ireacs loop.
      ENDIF  !- num_reacs if block.

      RETURN
      END

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM        : enc_iceturb subroutine
!                                                                      
! PURPOSE        : a pre-step to encoding SIGWX icing and turbulence
!                : areas (combined) into BUFR.                                           
!                                                                      
! DESCRIPTION    : From here, the BUFR descriptors and values arrays
!                : are updated to contain this information and passed
!                : back to the calling program.                                    
!                                                                     
! DATA TYPE(S)   : see code                                             
!                                                                      
! CALLED BY      : enc_sigwx main                                       
!                                                                      
! CALLS          : external : IDES                                      
!                : internal : -                                        
!                                                                      
! PARAMETERS     : see code                                             
!                                                                      
! CHANGE RECORD  :
!
! S.Cox 29/06/01 : written
!                                                                      
!-----------------------------------------------------------------------

      SUBROUTINE ENC_ICETURB(DESCR,VALUES,MAXDESC,NDESC,NELEM,NAMES,
     &                       NAMES_CNT)
      
      IMPLICIT NONE

! ----------------------------------------------------------------------
! --- declare variables.
! ----------------------------------------------------------------------

      INTEGER       SECTND              !- number of descriptors
      PARAMETER     (SECTND=24)         !- used to describe iceturb.

      INTEGER       MAXDESC             !- max no. of descriptors.!1.5a
      INTEGER       DESCR(MAXDESC)      !- descriptors array.
      REAL          VALUES(MAXDESC)     !- values array (1 ob).
      INTEGER       NDESC               !- no. of descriptors.
      INTEGER       NELEM               !- no. of values.
      CHARACTER*(*) NAMES               !- BUFR storage for chars.

      INTEGER       IDES                !- external IDES function.

      INTEGER       I                   !- loop counter.            
      INTEGER       IICES               !- loop counter.
      INTEGER       IPNTS               !- loop counter.
      INTEGER       NUM_ICES            !- number of icing areas.
      INTEGER       NUM_PNTS            !- number of pnts in area.
      INTEGER       IDESCR(SECTND)      !- descriptors for ICING.
      INTEGER       ICETYPE             !- type of icing.
      INTEGER       TURBULENCE          !- degree of turbulence.
      INTEGER       C_AMOUNT            !- clound amount.
      INTEGER       C_TYPE              !- cloud type.
      INTEGER       NAMES_CNT           !- names postion indicator.
      REAL          LAT                 !- icing area lats.
      REAL          LON                 !- icing area lons.
      REAL          B_HEIGHT            !- icing base height.
      REAL          T_HEIGHT            !- icing top height.

      DATA          IDESCR/122000,031001,!1.5b- delayed replication.- no. of areas's to follow.
     &                     008011,008007, !1.5b- icing code table value.- dimensional significance.
     &                     020041,007002, !1.5b- type of icing.- height (base of layer).
     &                     007002,011030, !1.6 - height (top of layer).- degree of turbulence.
     &                     007002,007002, !1.5b- height (base of layer).- height (top of layer).
     &                     020008,020012, !1.5b- cloud distribution. (icing)- cloud type. (icing)
     &                     007002,      !- height (base of layer). (icing)
     &                     007002,      !- height (top of layer). (icing)
     &                     020008,      !- cloud distribution. (turb)
     &                     020012,      !- cloud type. (turb)
     &                     007002,      !- height (base of layer). (turb)
     &                     007002,      !- height (top of layer). (turb)
     &                     102000,      !- delayed replication.
     &                     031001,      !- no. of points to follow.
     &                     005002,      !- latitude.
     &                     006002,      !- longitude.
     &                     008007,      !- cancel dimensional signif.
     &                     008011/      !- cancel icing section.

! ----------------------------------------------------------------------
! --- Read the number of areas. Add number of areas to values array.
! ----------------------------------------------------------------------

      READ(10,*) NUM_ICES
      NELEM=NELEM+1
      VALUES(NELEM)=NUM_ICES       !- corresponds to desc 031001.

! ----------------------------------------------------------------------
! --- Loop through the number of descriptors used to describe an icing
! --- area and put in DESCR array. Increment NDESC.
! ----------------------------------------------------------------------

      DO I=1,SECTND
        DESCR(NDESC+I)=IDES(IDESCR(I))
      ENDDO
      NDESC=NDESC+SECTND             !- increment number of descriptors.

! ----------------------------------------------------------------------
! --- loop through number of areas if one or more.
! ----------------------------------------------------------------------

      IF (NUM_ICES.NE.0) THEN
      
        DO IICES=1,NUM_ICES

! ----------------------------------------------------------------------
! --- Add value to values array to indicate icing area.
! --- Add value to values array specifying dimensional significance.
! ----------------------------------------------------------------------

         NELEM=NELEM+1
         VALUES(NELEM)=15          !- desc 008011. Icing = 15.
         NELEM=NELEM+1
         VALUES(NELEM)=2           !- desc 008007. Area = 2.
         
! ----------------------------------------------------------------------
! --- Read the type of icing and add to the values array.
! ----------------------------------------------------------------------

          READ(10,*) ICETYPE

          NELEM=NELEM+1
          VALUES(NELEM)=ICETYPE     !- corresponds to desc 020041.
 
! ----------------------------------------------------------------------
! --- Read the base and top height of the icing area.
! --- Add these values to the values array.
! ----------------------------------------------------------------------

          READ(10,*) B_HEIGHT,T_HEIGHT

          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT    !- corresponds to desc 007002.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT    !- corresponds to desc 007002.
                
! ----------------------------------------------------------------------
! --- Read the degree of turbulence and add to the values array.
! ----------------------------------------------------------------------

          READ(10,*) TURBULENCE

          NELEM=NELEM+1
          VALUES(NELEM)=TURBULENCE    !- corresponds to desc 011030.
 
! ----------------------------------------------------------------------
! --- Read the base and top height of the turbulence area
! --- Add these values to the values array.
! ----------------------------------------------------------------------

          READ(10,*) B_HEIGHT,T_HEIGHT

          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT    !- corresponds to desc 007002.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT    !- corresponds to desc 007002.
                
! ----------------------------------------------------------------------
! --- CLOUD(1) - icing reference.
! --- Read the cloud amount and type and add these to the vales array.
! --- Read the base and top height of the cloud area and add these
! ---  values to the values array.
! ----------------------------------------------------------------------

          READ(10,*) C_AMOUNT,C_TYPE

          NELEM=NELEM+1
          VALUES(NELEM)=C_AMOUNT    !- corresponds to desc 020008.
          NELEM=NELEM+1
          VALUES(NELEM)=C_TYPE      !- corresponds to desc 020012.

          READ(10,*) B_HEIGHT,T_HEIGHT

          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT    !- corresponds to desc 007002.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT    !- corresponds to desc 007002.
                
! ----------------------------------------------------------------------
! --- CLOUD(2) - turbulence reference.
! --- Read the cloud amount and type and add these to the vales array.
! --- Read the base and top height of the cloud area and add these
! --- values to the values array.
! ----------------------------------------------------------------------

          READ(10,*) C_AMOUNT,C_TYPE

          NELEM=NELEM+1
          VALUES(NELEM)=C_AMOUNT    !- corresponds to desc 020008.
          NELEM=NELEM+1
          VALUES(NELEM)=C_TYPE      !- corresponds to desc 020012.

          READ(10,*) B_HEIGHT,T_HEIGHT

          NELEM=NELEM+1
          VALUES(NELEM)=B_HEIGHT    !- corresponds to desc 007002.
          NELEM=NELEM+1
          VALUES(NELEM)=T_HEIGHT    !- corresponds to desc 007002.
                
! ----------------------------------------------------------------------
! --- Read the number of points used to describe the area. Add this
! --- value to the values array.
! ----------------------------------------------------------------------

          READ(10,*) NUM_PNTS
          NELEM=NELEM+1
          VALUES(NELEM)=NUM_PNTS    !- corresponds to desc 031001.
                
! ----------------------------------------------------------------------
! --- loop through number of points describing the area. Read lat and
! --- lon of the area and add to the values array.
! ----------------------------------------------------------------------

          DO IPNTS=1,NUM_PNTS

            READ(10,*) LAT,LON

            NELEM=NELEM+1
            VALUES(NELEM)=LAT       !- corresponds to desc 005002.
            NELEM=NELEM+1
            VALUES(NELEM)=LON       !- corresponds to desc 006002.
             
          ENDDO  !- ipnts do loop.

! ----------------------------------------------------------------------
! --- End of icing section. Cancel dimensional significance
! --- and meteorological feature.
! ----------------------------------------------------------------------

          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008007.
          NELEM=NELEM+1
          VALUES(NELEM)=-9999999.0    !- corresponds to desc 008011.

        ENDDO  !- iices do loop.
      ENDIF  !- num_ices if block.

      RETURN
      END
