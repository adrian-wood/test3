       PROGRAM NAME_SEARCH
!***11111111111111111111111111111111111111111111111111111111111111111111        
!                                                                               
! FILE NAME: name_seach.f
!
! TYPE: program
!
! LANGUAGE: FORTRAN 77         
!                                                                               
!***22222222222222222222222222222222222222222222222222222222222222222222        
!                                                                               
! PURPOSE:  To search the pattern submitted from the Station
!           name field in a html web form.
!
!***33333333333333333333333333333333333333333333333333333333333333333333        
!                                                                               
! STRUCTURE: 
!  Works by writing out the html heading, followed by a check through 
!  each line of abbreviated station list, It searches the name part
!  for a matching pattern and if found writing out the matching
!  Stations as html forms.
!  Program then completes the html and finishes.
!                                           
! REFERENCES:                                                                
!   Abbreviate Station Master List.                                     
!                                                                               
!***44444444444444444444444444444444444444444444444444444444444444444444        
!                                                                               
! EXTERNAL ROUTINES:                                                             
!                                                                               
! EXTERNAL I/O (FILES) EXPLICITLY ALLOCATED                                           
!  INPUT:                                                                       
!    abrv_stnmas.txt                                                                       
!  INPUT/OUTPUT:                                                                
!    NONE                                                                       
!  OUTPUT :                                                                     
!    NONE                                                                       
!                                                                               
! EXTERNAL I/O (FILES) DYNAMICALLY ALLOCATED                                            
!   INPUT: NONE                                                                 
!   INPUT/OUTPUT: NONE                                                          
!   OUTPUT : NONE                                                               
!                                                                               
!***55555555555555555555555555555555555555555555555555555555555555555555        
!                                                                               
! RETURN CODES                                                                  
!      0 - NO ERROR
!     99 - ERROR DETECTED                                                          
!                                                                               
!***66666666666666666666666666666666666666666666666666666666666666666666
!
! INTERFACE
!
!  FUNCTION WRITE_HTML_HEAD(CHARACYER*80 TITLE)
!  FUNCTION CHECK_PATTERN(CHARACTER*42 NAME, CHARACTER*89 DETAILS,
!                         INTEGER*4 NAME_LENGTH,
!                         INTEGER*4 POSITION,
!                         INTEGER*4 STRING_LENGTH )
!  FUNCTION WRITE_ICAO_DETAILS(INDEX,ICAO_ID,DETAILS)
!  FUNCTION WRITE_HTML_TAIL ()
!
!***77777777777777777777777777777777777777777777777777777777777777777777     
!
! Code written by Stanley Kellett, MetDB Team, April 2000   
!
! CHANGE HISTORY                                                                
!        
! $Revision: 4$
! $Date: 07/02/2014 15:48:22$
! $Source: /home/h01/usmdb/station_search/source/RCS/name_search.F,v $
!                                                                       
! $Log:
!  4    Met_DB_Project 1.3         07/02/2014 15:48:22    John Norton
!       updated handling of lat and long values from DETAILS.
!  3    Met_DB_Project 1.2         07/02/2014 11:59:15    John Norton
!       Updated the transfer of station name string from DETAIL.
!  2    Met_DB_Project 1.1         31/01/2014 16:01:39    John Norton
!       Updated to change: web retrieval link; us0400 to h01; usmdb-test to
!       usmdb and remove revision information. This should then make code work
!        again.
!  1    Met_DB_Project 1.0         13/11/2006 16:19:48    Kudsia Gwangwaa 
! $
! Revision 1.8  2006/08/29 08:12:49  usmdb
! changed mdb folder to mdb_new folder.
! Stan Kellett.
!
! Revision 1.7  2002/12/19 13:08:23  usmdb
! corrected title spelling from abbreviated to abreviated.
! Stan Kellett - 19th december 2002.
!
! Revision 1.6  2001/09/04 08:31:20  usmdb
! updated for f90 compiler.
!
! Revision 1.5  2001/05/10 12:06:15  usmdb
! chaned station height to pressure sensor height
!
! Revision 1.4  2000/09/21 13:32:56  usmdb
! Correct spelling mistakes and ICAO DETAILS position for longitude.
!
! Revision 1.3  2000/08/02  16:29:15  16:29:15  usmdb (Generic MDB account)
! change retrieve alignment to center.
! 
! Revision 1.2  2000/08/02  16:10:14  16:10:14  usmdb (Generic MDB account)
! added facility to retrieve latest Synops and Metars.
! 
! Revision 1.1  2000/06/09  10:49:50  10:49:50  usmdb (Generic MDB account)
! Initial revision
! 
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 

! Declare Functions
      INTEGER WRITE_HTML_HEAD
      INTEGER WRITE_NAME_DETAILS
      INTEGER WRITE_HTML_TAIL
      LOGICAL CHECK_PATTERN

! Declare Program variables
      CHARACTER*80 INDATA            !1.6 Input from form
      DATA         INDATA /" "/      !1.6
      CHARACTER*80 TITLE             !Title for results page !1.2
      CHARACTER*89 DETAILS           !1.6 Line of Station Details read in
      DATA         DETAILS /""/      !1.6
      CHARACTER*80 SEARCH_ABRV       !1.6 Name of Abreviated file
      DATA         SEARCH_ABRV /""/  !1.6
      CHARACTER*80 SEARCH_ICAO       !1.6 Name of ICAO file
      DATA         SEARCH_ICAO /""/  !1.6
      CHARACTER*42 NAME              !1.6 String pattern to search for
      DATA         NAME /""/         !1.6 Station Name
      INTEGER   INDEX                !1.6 Used for length of pattern for
      DATA      INDEX /5/            !1.6                   ! search
      INTEGER          I,            !1.6 Control Variable
     &          STATUS,              ! Return status from Function Calls
     &          FILESTAT,            ! Status of read from Station Details  
     &          NAME_LENGTH,         ! Length of pattern string
     &          ABREV_INDEX          !1.6 Index of start of name in 
      DATA      ABREV_INDEX /57/     !1.6 Abreviated Staion Master
      INTEGER   ICAO_INDEX           !1.6 Index of staart of name in
      DATA      ICAO_INDEX /25/      !1.6 ICAO Station list
      INTEGER   STRING_LENGTH        !1.6 max length of string for
      DATA      STRING_LENGTH /42/   !1.6 CHECK_PATTERN function 

      LOGICAL FOUND                  ! Set to true if pattern found in
                                     ! details name

      DATA TITLE /'ABREVIATED NAME SEARCH From MetDB'/    !1.7

! Open Standard input
      OPEN(5)

! Open Standard output
      OPEN(6)

! Open Station Details File
      SEARCH_ABRV="/home/h01/mdb_new/op/data/MDB.STNMAS.ABRVLST"      !wmo abrev. list
      OPEN(10,FILE=SEARCH_ABRV)                             !1.6
      SEARCH_ICAO="/home/h01/mdb_new/op/data/MDB.ICAO.LIST"           !ICAO list
      OPEN(11,FILE=SEARCH_ICAO)                             !1.6

! Read in WMO details for searching
      READ(5,'(A80)')INDATA

! Write out html header information
      STATUS = WRITE_HTML_HEAD(TITLE)

! If Ok then continue
      IF (STATUS.EQ.0) THEN
         I=6
         DO WHILE ((INDATA(I:I).NE."&").AND.
     &             (INDATA(I:I).NE.""))
            INDEX=I
            I=I+1
! Set Station Name
            NAME(INDEX-5:INDEX-5)=INDATA(INDEX:INDEX)
            IF (INDATA(INDEX:INDEX).EQ."+") THEN
               NAME(INDEX-5:INDEX-5)=" "
            ENDIF
         ENDDO
         NAME_LENGTH=INDEX-5

! loop over number of Records and read in details for each record
! read first line which just contains header information
         READ (10,'(A89)',IOSTAT=FILESTAT)DETAILS

         WRITE(6,'("<BR><B>",A,"</BR>")')                        
     &             "Abbreviated WMO List Results:"          !1.4   
! Write out the table header for results
         WRITE (6,'("<TABLE WIDTH=1000 COLS=5 BORDER=0 ")') !1.3
         WRITE (6,'(A)')'ALIGN="CENTER">'                   !1.3
         WRITE(6,'("<TR>")')
         WRITE(6,'("<TH WIDTH=450>Station Name</TH>")') !1.2
         WRITE(6,'("<TH WIDTH=50>WMO Number</TH>")')
         WRITE(6,'("<TH WIDTH=100>Pressure Sensor Height</TH>")')
         WRITE(6,'("<TH WIDTH=125>Latitude</TH>")')
         WRITE(6,'("<TH WIDTH=125>Longitude</TH>")')
         WRITE(6,'("<TH WIDTH=150>Latest MetDB LNDSYN</TH>")') !1.2
         WRITE(6,'("</TR>")')
            
         DO WHILE (FILESTAT.GE.0)
            READ (10,'(A89)',IOSTAT=FILESTAT)DETAILS            

! Write out Search Matches
            IF (INDEX.EQ.5) THEN ! Write out No name number entered 
               WRITE(6,'("<BR>No Station Name entered<BR>")')
            ELSEIF ((INDEX.GE.6).AND.(INDEX.LE.43)) THEN
               FOUND = CHECK_PATTERN(NAME,DETAILS,NAME_LENGTH,    
     &                               ABREV_INDEX,STRING_LENGTH)     
               IF (FOUND) THEN
                  STATUS = WRITE_NAME_DETAILS(DETAILS)
                  IF (STATUS.NE.0) THEN
                      WRITE(6,'("<BR>Error in WRITE_NAME_DETAILS")')
                  ENDIF
               ENDIF
            ELSE
               WRITE(6,'("<BR>Error with Name entered")')
            ENDIF
         ENDDO

! Close Table
         WRITE(6,'("</TABLE>")')

! Now Search ICAO List

! loop over number of Records and read in details for each record
! read first line which just contains header information
         READ (11,'(A80)',IOSTAT=FILESTAT)DETAILS             

! Write out the table header for results
         WRITE(6,'("<BR><BR><BR>")')                             
         WRITE(6,'("<BR><B>",A,"</BR>")')                      
     &             "Abbreviated ICAO List Results:"           !1.4  

         WRITE (6,'("<TABLE WIDTH=1050 COLS=5 BORDER=0 ")')   !1.3
         WRITE (6,'(A)')'ALIGN="CENTER">'                     !1.3    
         WRITE(6,'("<TR>")')                                      
         WRITE(6,'("<TH WIDTH=450>Station Name</TH>")')       !1.2    
         WRITE(6,'("<TH WIDTH=100>ICAO ID</TH>")')               
         WRITE(6,'("<TH WIDTH=50>WMO No</TH>")')         
         WRITE(6,'("<TH WIDTH=125>Latitude</TH>")')              
         WRITE(6,'("<TH WIDTH=125>Longitude</TH>")')           
         WRITE(6,'("<TH WIDTH=150>Latest MetDB Metar</TH>")')  !1.2            
         WRITE(6,'("</TR>")')                                    
            
         DO WHILE (FILESTAT.GE.0)                                
            READ (11,'(A80)',IOSTAT=FILESTAT)DETAILS            
! Write out Search Matches from ICAO list 
            IF (INDEX.EQ.5) THEN ! Write out No Name number entered 
               WRITE(6,'("<BR>No Station Name entered<BR>")')    
            ELSEIF ((INDEX.GE.6).AND.(INDEX.LE.43)) THEN            
               FOUND = CHECK_PATTERN(NAME,DETAILS,NAME_LENGTH,      
     &                               ICAO_INDEX,STRING_LENGTH)      
               IF (FOUND) THEN                                      

! write out station details
! NOTE. These alignments should be the same as those in write_icao_details.F

                  WRITE(6,'("<TR>")')                               
                  WRITE(6,'("<TD ALIGN=CENTER>",A,"</TD>")')        
     &                    DETAILS(39:81)                            
                  WRITE(6,'(6A)')'<TD ALIGN="CENTER">',             
     &'<A HREF="http://www-metdb/cgi-bin/',
     &'station_search/search_full_icaolist.sh?',
     &    DETAILS(1:4),'">',DETAILS(1:4),'</A></TD>'                 
                  WRITE(6,'("<TD ALIGN=CENTER>",A,"</TD>")')        
     &                    DETAILS(6:10)                             
                  WRITE(6,'("<TD ALIGN=CENTER>",A,"</TD>")')        
     &                    DETAILS(12:18)                       !1.4     
                  WRITE(6,'("<TD ALIGN=CENTER>",A,"</TD>")')        
     &                    DETAILS(20:27)                       !1.4

          WRITE(6,'("<TD ALIGN=CENTER>",13A,"</TD>")')  !1.3
     &     '<A HREF="',                                 !1.3
     &   'http://mdbdb-prod/cgi-bin/moods/webret.pl?',!1.2
     &   'subtype=METARS&stn01=',DETAILS(1:4),          !1.2
     &   '&stn02=&stn03=&stn04=&stn05=&stn06=&stn07=',  !1.2
     &   '&stn08=&stn09=&stn10=',                       !1.2
     &   '&submit=Retrieve+Latest+Report',              !1.2
     &   '&startdate=20000101&starthour=00',            !1.2
     &   '&startminute=00&enddate=20000101',            !1.2
     &   '&endhour=23&endminute=59&maxobs=0500&header=no',!1.2
     &   '&versions=no&increment=00&area_n=&area_s=',   !1.2
     &   '&area_w=&area_e=">',                          !1.2
     &   'Retrieve </A>'                                !1.2                      
                  WRITE(6,'("</TR>")')                              
                  IF (STATUS.NE.0) THEN                             
                      WRITE(6,'("<BR>Error in WRITE_ICAO_DETAILS")')
                  ENDIF                                             
               ENDIF                                                
            ELSE                                                    
               WRITE(6,'("<BR>Error with Name entered")')           
            ENDIF                                                   
         ENDDO                                                      

! Close Table
         WRITE(6,'("</TABLE>")')                                    

! Close html page
         STATUS = WRITE_HTML_TAIL()
         IF (STATUS.NE.0) THEN
            WRITE (6,'("<BR>Problem with WRITE_HTML_TAIL")')
         ENDIF
      ELSE
         WRITE(6,'("<BR>Problem with WRITE_HTML_HEAD")')
      ENDIF


! Close Files
      CLOSE(5)
      CLOSE(6)
      CLOSE(10)
      CLOSE(11)                                                  

! Stop and End
      STOP
      END
