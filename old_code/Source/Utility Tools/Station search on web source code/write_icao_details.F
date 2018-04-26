      INTEGER FUNCTION WRITE_ICAO_DETAILS(INDEX,ICAO_ID,DETAILS)
!***11111111111111111111111111111111111111111111111111111111111111111111        
!                                                                               
! FILE NAME: write_icao_details.f
!
! TYPE: function
!
! LANGUAGE: FORTRAN 77         
!                                                                               
!***22222222222222222222222222222222222222222222222222222222222222222222        
!                                                                               
! PURPOSE:  This function writes out the form which holds the found 
!           Station details
!
!***33333333333333333333333333333333333333333333333333333333333333333333        
!                                                                               
! STRUCTURE: 
!  WRITE_ICAO_DETAILS checks the size of the string holding ICAO_ID;
!  then checks if the DETAILS string holds data for the required station.
!  If it does then a row is added to the table with the details.
!                                           
! REFERENCES:                                                                
!                                         
!                                                                               
!***44444444444444444444444444444444444444444444444444444444444444444444        
!                                                                               
! EXTERNAL ROUTINES:                                                             
!                                                                               
! EXTERNAL I/O (FILES) EXPLICITLY ALLOCATED                                           
!  INPUT:                                                                       
!    NONE                                                                       
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
!     99 - Error found                                                            
!                                                                               
!***66666666666666666666666666666666666666666666666666666666666666666666
!
! INTERFACE
!
!
!***77777777777777777777777777777777777777777777777777777777777777777777     
!
! Original Code written by Stanley Kellett, MetDB team, April 2000.   
!
! CHANGE HISTORY                                                                
!
! $Workfile: write_icao_details.F$ $Folder: Station search on web source code$
! $Revision: 5$ $Date: 10/04/2012 14:33:55$
!                                                                     
! $Log:
!  5    Met_DB_Project 1.4         10/04/2012 14:33:55    Paul Barnham
!       Change from rexx script to use perl script for retrieval.
!  4    Met_DB_Project 1.3         08/10/2010 14:02:01    Brian Barwell
!       Change column numbers for new ICAO list format.
!  3    Met_DB_Project 1.2         08/09/2006 09:15:55    Stan Kellett
!       changed els018 link to www-metdb-test alias
!  2    Met_DB_Project 1.1         31/08/2006 17:16:01    Stan Kellett    Link
!       changed to run cgi script from Linux rather than HP-UX
!  1    Met_DB_Project 1.0         31/08/2006 15:54:03    Stan Kellett    
! $
! Revision 1.3  2001/09/04 10:13:30  usmdb
! updated for f90 compiler
!
! Revision 1.2  2000/08/01 15:53:22  usmdb
! addition of Metar retrieval field.
!
! Revision 1.1  2000/06/09  10:32:34  10:32:34  usmdb (Generic MDB account)
! Initial revision
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 
      
! Revision Control Variables
      CHARACTER*80 HEAD           !4
      LOGICAL HEADSET             !1.3
      DATA    HEADSET /.FALSE./   !1.3

! Scalar arguments with intent in
      INTEGER*4 INDEX             ! Last charact of string sent from form
      CHARACTER*4 ICAO_ID         ! String holding WMO_number
      CHARACTER*80 DETAILS        ! String holding Station details

! Local Scalars
      INTEGER*4 ICAO_LENGTH       ! length of WMO_NUMBER search string

! Revision Code
      IF (.NOT.HEADSET) THEN
        HEAD='$Workfile: write_icao_details.F$ ' //
     &       '$Date: 10/04/2012 14:33:55$ $Revision: 5$'
        HEADSET=.TRUE.
      ENDIF

! Calculate ICAO_LENGTH
      ICAO_LENGTH = (INDEX-5)
      
! Check if current details string has the searched WMO string within
      IF (DETAILS(1:ICAO_LENGTH).EQ.ICAO_ID) THEN
         WRITE(6,'("<TR ALIGN=CENTER>")')
         WRITE(6,'(6A)')'<TD ALIGN="CENTER">',             
     &'<A HREF="http://www-metdb-test/cgi-bin/station_search/', !2
     &'search_full_icaolist.sh?',
     &   DETAILS(1:4),'">',DETAILS(1:4),'</A></TD>'                  
         WRITE(6,'("<TD>",A5,"</TD>")')DETAILS(6:10)
         WRITE(6,'("<TD>",A42,"</TD>")')DETAILS(39:80)  !4 
         WRITE(6,'("<TD>",A8,"</TD>")')DETAILS(12:18)   !4
         WRITE(6,'("<TD>",A8,"</TD>")')DETAILS(20:27)   !4

         WRITE(6,'("<TD>",13A,"</TD>")')'<A HREF="',    !1.2
     &   'http://www01/SysCGI/MetDB_webret.perl?',		!1.2
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
      ENDIF

! Set return status to 0 for ok
      write_icao_details=0

! Return and end
      RETURN
      END
