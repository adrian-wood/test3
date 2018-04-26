       PROGRAM WMO_SEARCH
!***11111111111111111111111111111111111111111111111111111111111111111111        
!                                                                               
! FILE NAME: wmo_seach.f
!
! TYPE: program
!
! LANGUAGE: FORTRAN 77         
!                                                                               
!***22222222222222222222222222222222222222222222222222222222222222222222        
!                                                                               
! PURPOSE:  To search the pattern submitted from the wmo number field
!    in a html web form.
!
!***33333333333333333333333333333333333333333333333333333333333333333333        
!                                                                               
! STRUCTURE: 
!  Works by writing out the html heading, followed by a check through 
!  each line of abbreviated station list, writing out the matching
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
!  FUNCTION WRITE_ABBV_DETAILS(INTEGER*4 INDEX, CHARACTER*5 WMO_NUMBER,
!                                CHARACTER*89 DETAILS)
!  FUNCTION WRITE_HTML_TAIL ()
!
!***77777777777777777777777777777777777777777777777777777777777777777777    
!
! Original Code written by Stanley Kellett, MetDB team, April 2000    
!
! CHANGE HISTORY                                                                
!
! $Revision: 1$
! $Date: 31/08/2006 14:16:13$
! $Source: /home/us0400/usmdb/station_search/source/RCS/wmo_search.F,v $                             
!                                                  
! $Log:
!  1    Met_DB_Project 1.0         31/08/2006 14:16:13    Stan Kellett    
! $
! Revision 1.5  2006/08/29 08:04:48  usmdb
! changed mdb folder to mdb_new folder for list.
! Stan Kellett.
!
! Revision 1.4  2001/09/04 08:20:51  usmdb
! updated for f90 compiler.
!
! Revision 1.3  2001/05/10 12:07:28  usmdb
! changed station height to pressure sensor height
!
! Revision 1.2  2000/08/01 15:20:04  usmdb
! add field to retrieve latest Synop, and and MetDB to heading.
!
! Revision 1.1  2000/06/07  14:55:03  14:55:03  usmdb (Generic MDB account)
! Initial revision
! 
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 

! Revision control variables      
      CHARACTER*132 HEAD
      LOGICAL HEADSET                !1.4
      DATA    HEADSET /.FALSE./      !1.4

! Declare Functions
      INTEGER WRITE_HTML_HEAD        ! function to write out html header
      INTEGER WRITE_ABBV_DETAILS     ! function to write out abbreviated details
      INTEGER WRITE_HTML_TAIL        ! function to write out html tail

! Declare Program variables
      CHARACTER*80 INDATA            !1.4 Input from form
      DATA         INDATA /" "/      !1.4
      CHARACTER*80 SEARCH_FILE       !1.4 Station Details file
      DATA         SEARCH_FILE /" "/ !1.4
      CHARACTER*80 TITLE             !Title for results  !1.2
      CHARACTER*89 DETAILS           !1.4 Line of Station Details read in
      DATA         DETAILS /" "/     !1.4
      CHARACTER*5  WMO_NUMBER        !1.4 String pattern to search for WMO
      DATA         WMO_NUMBER /" "/  !1.4 Number
      INTEGER   INDEX                !1.4 Used for length of pattern for
      DATA      INDEX  /7/           !1.4 search
      INTEGER   I,                   ! Control Variable
     &          STATUS,              ! Return status from Function Calls
     &          FILESTAT             ! Status of read from Station Details  

      DATA TITLE /"ABREVIATED WMO SEARCH FROM METDB"/ !1.2

! Revision Control Code
      IF (.NOT.HEADSET) THEN
        HEAD='
     &        $SOURCE: $
     &       '//'$Date: 31/08/2006 14:16:13$ $Revision: 1$'
        HEADSET=.TRUE.
      ENDIF

! Open Standard input
      OPEN(5)

! Open Standard output
      OPEN(6)

! Open Station Details File
      SEARCH_FILE = "/home/us0400/mdb_new/op/data/MDB.STNMAS.ABRVLST" ! Abreviated list
      OPEN(10,file=SEARCH_FILE)                          

! Read in WMO details for searching
      READ(5,'(A80)')INDATA

! Write out html header information
      STATUS = WRITE_HTML_HEAD(TITLE)

! If Ok then continue
      IF (STATUS.EQ.0) THEN
         I=8
         DO WHILE ((INDATA(I:I).NE."&").AND.(INDATA(I:I).NE." ").AND.
     &             (INDATA(I:I).NE.""))
            INDEX=I
            I=I+1
! Set WMO_NUMBER
            WMO_NUMBER(INDEX-7:INDEX-7)=INDATA(INDEX:INDEX)
         ENDDO

! loop over number of Records and read in details for each record
! read first line which just contains header information
         READ (10,'(A89)',IOSTAT=FILESTAT)DETAILS

! Write out instructions to ask user to click on WMO number if 
! the full station details are required.

         WRITE (6,'("<BR>")')                                          
         WRITE (6,'("For the full station details of any Station")')   
         WRITE (6,'("<BR>")')                                          
         WRITE (6,'("Please click the WMO number in the table below")')
         WRITE (6,'("<BR>")')                                          
         WRITE (6,'("<BR>")')       
         WRITE (6,'("For the latest Land Synop report,")') !1.2
         WRITE (6,'("Click retrieve")')     !1.2                              
         WRITE (6,'("<BR>")')                                             

! Write out the table header for results
         WRITE (6,'("<TABLE WIDTH=1000 COLS=6 BORDER=0>")') !1.2
         WRITE(6,'("<TR>")')
         WRITE(6,'("<TH WIDTH=50>WMO Number</TH>")')
         WRITE(6,'("<TH WIDTH=450>Station Name</TH>")')
         WRITE(6,'("<TH WIDTH=100>Pressure Sensor Height</TH>")') !1.3
         WRITE(6,'("<TH WIDTH=125>Latitude</TH>")')
         WRITE(6,'("<TH WIDTH=125>Longitude</TH>")')
         WRITE(6,'("<TH WIDTH=150>Latest MetDB LNDSYN</TH>")') !1.2
         WRITE(6,'("</TR>")')
            
         DO WHILE (FILESTAT.GE.0)
            READ (10,'(A89)',IOSTAT=FILESTAT)DETAILS            

! Write out Search Matches
            IF (INDEX.EQ.7) THEN ! Write out No WMO number entered
               WRITE(6,'("<BR>No WMO number entered<BR>")')
            ELSEIF ((INDEX.GE.8).AND.(INDEX.LE.12)) THEN
               STATUS = WRITE_ABBV_DETAILS(INDEX,WMO_NUMBER,DETAILS)
               IF (STATUS.NE.0) THEN
                  WRITE(6,'("<BR>Error in WRITE_ABBV_DETAILS")')
               ENDIF
            ELSE
               WRITE(6,'("<BR>Error with Number entered")')
            ENDIF
         ENDDO

! Close Table
         WRITE(6,'("</TABLE>")')

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

! Stop and End
      STOP
      END
