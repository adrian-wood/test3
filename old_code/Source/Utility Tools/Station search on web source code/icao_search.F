       PROGRAM ICAO_SEARCH
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
! PURPOSE:  To search the pattern submitted from the icao number field
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
!  FUNCTION WRITE_ICAO_DETAILS(INTEGER*4 INDEX, CHARACTER*4 ICAO_ID,
!                                CHARACTER*86 DETAILS)
!  FUNCTION WRITE_HTML_TAIL ()
!
!***77777777777777777777777777777777777777777777777777777777777777777777        
!
! Program written by Stanley Kellett, MetDB team, April 2000
!
! CHANGE HISTORY                                                                
!           
! $Revision: 2$
! $Date: 08/10/2010 13:58:30$
! $Source: /home/us0400/usmdb/station_search/source/RCS/icao_search.F,v $
!                                                                    
! $Log:
!  2    Met_DB_Project 1.1         08/10/2010 13:58:30    Brian Barwell   Don't
!        skip a header line but check for blank line at end of ICAO data set.
!       Also change 'deg/min' to 'degrees' in headings.
!  1    Met_DB_Project 1.0         31/08/2006 15:52:49    Stan Kellett    
! $
! Revision 1.6  2006/08/29 08:10:04  usmdb
! changed mdb folder to mdb_new folder.
! Stan Kellett.
!
! Revision 1.5  2002/12/19 12:05:59  usmdb
! Corrected spelling mistake of Abbreviated to Abreviated.
! Stan Kellett - 19th December 2002.
!
! Revision 1.4  2001/09/04 10:24:46  usmdb
! ipdated for f90 compiler
!
! Revision 1.3  2001/07/11 09:06:18  usmdb
! added deg/min descriptor to lat and long headings.
!
! Revision 1.2  2000/08/01 15:52:51  usmdb
! added Metar retrieval field.
!
! Revision 1.1  2000/06/07  14:24:06  14:24:06  usmdb (Generic MDB account)
! Initial revision
! 
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 
      
! Revision control variables
      CHARACTER*80 HEAD              !2
      LOGICAL HEADSET                !1.4
      DATA    HEADSET  /.FALSE./     !1.4

! Declare Functions
      INTEGER WRITE_HTML_HEAD
      INTEGER WRITE_ICAO_DETAILS
      INTEGER WRITE_HTML_TAIL

! Declare Program variables
      CHARACTER*80 INDATA            !1.4 Input from form
      DATA         INDATA /" "/      !1.4
      CHARACTER*80 TITLE             !Title for results   !1.2                                                  ! page
      CHARACTER*80 DETAILS           !1.4 Line of Station Details read in
      DATA         DETAILS /""/      !1.4
      CHARACTER*80 SEARCH_FILE       !1.4 ICAO station list
      DATA         SEARCH_FILE /""/  !1.4
      CHARACTER*4  ICAO_ID           !1.4 String pattern to search for WMO
      DATA         ICAO_ID /""/      !1.4 Number
      INTEGER*4 INDEX                !1.4 Used for length of pattern for
      DATA      INDEX /5/            !1.4 search
      INTEGER   I,                   !1.4 Control Variable
     &          STATUS,              ! Return status from Function Calls
     &          FILESTAT             ! Status of read from Station Details  

      DATA TITLE /"ABREVIATED ICAO SEARCH FROM METDB"/    !1.5
! Revision control code
      IF (.NOT.HEADSET) THEN
        HEAD='$Workfile: icao_search.F$ ' //
     &       '$Date: 08/10/2010 13:58:30$ $Revision: 2$'
        HEADSET=.TRUE.
      ENDIF

! Open Standard input
      OPEN(5)

! Open Standard output
      OPEN(6)

! Open Station Details File
      SEARCH_FILE="/home/us0400/mdb_new/op/data/MDB.ICAO.LIST"  ! ICAO list
      OPEN(10,FILE=SEARCH_FILE)       !1.4                   

! Read in WMO details for searching
      READ(5,'(A80)')INDATA

! Write out html header information
      STATUS = WRITE_HTML_HEAD(TITLE)

! If Ok then continue
      IF (STATUS.EQ.0) THEN
         I=6
         DO WHILE ((INDATA(I:I).NE."&").AND.(INDATA(I:I).NE." ").AND.
     &             (INDATA(I:I).NE.""))
            INDEX=I
            I=I+1
! Set WMO_NUMBER
            ICAO_ID(INDEX-5:INDEX-5)=INDATA(INDEX:INDEX)
         ENDDO

! Write out the table header for results
         WRITE(6,'("<TABLE WIDTH=1000 COLS=5 BORDER=0>")')
         WRITE(6,'("<TR>")')
         WRITE(6,'("<TH WIDTH=50>ICAO ID</TH>")')
         WRITE(6,'("<TH WIDTH=50>WMO Number</TH>")')
         WRITE(6,'("<TH WIDTH=400>Station Name</TH>")')
         WRITE(6,'("<TH WIDTH=125>Latitude (degrees)</TH>")')  !2
         WRITE(6,'("<TH WIDTH=125>Longitude (degrees)</TH>")') !2
         WRITE(6,'("<TH WIDTH=150>Latest MetDB Metar</TH>")') !1.2
         WRITE(6,'("</TR>")')

! loop over number of Records and read in details for each record            
         DO WHILE (FILESTAT.GE.0)
            READ (10,'(A80)',IOSTAT=FILESTAT)DETAILS            

! Write out Search Matches
            IF (DETAILS.EQ.' ') THEN  ! End of list      !2
               FILESTAT = .TRUE.                         !2
            ELSE                                         !2
               IF (INDEX.EQ.5) THEN ! Write out No WMO number entered
                  WRITE(6,'("<BR>No ICAO entered<BR>")')
               ELSEIF ((INDEX.GE.6).AND.(INDEX.LE.9)) THEN
                  STATUS = WRITE_ICAO_DETAILS(INDEX,ICAO_ID,DETAILS)
                  IF (STATUS.NE.0) THEN
                     WRITE(6,'("<BR>Error in WRITE_ICAO_DETAILS")')
                  ENDIF
               ELSE
                  WRITE(6,'("<BR>Error with ICAO entered")')
               ENDIF
            END IF                                       !2
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
