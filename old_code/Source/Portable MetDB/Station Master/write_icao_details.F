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
!  Function first of all checks the size of strng holding ICAO ID
!  then the function checks if current details string hols the required
!  search string required. If it does then a row is added to table with
!  required details.
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
! $Revision: 4$
! $Date: 07/02/2014 15:48:22$
! $Source: /home/h01/usmdb/station_search/source/RCS/write_icao_details.F,v $
!                                                                     
! $Log:
!  4    Met_DB_Project 1.3         07/02/2014 15:48:22    John Norton
!       updated handling of lat and long values from DETAILS.
!  3    Met_DB_Project 1.2         06/02/2014 14:17:28    John Norton
!       Updated station name position obtained from details string.
!  2    Met_DB_Project 1.1         31/01/2014 16:01:39    John Norton
!       Updated to change: web retrieval link; us0400 to h01; usmdb-test to
!       usmdb and remove revision information. This should then make code work
!        again.
!  1    Met_DB_Project 1.0         13/11/2006 16:25:24    Kudsia Gwangwaa 
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
! 
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 
 
! Scalar arguments with intent in
      INTEGER*4 INDEX             ! Last charact of string sent from form
      CHARACTER*4 ICAO_ID         ! String holding WMO_number
      CHARACTER*80 DETAILS        ! String holding Station details

! Local Scalars
      INTEGER*4 ICAO_LENGTH        ! length of WMO_NUMBER search string


! Calculate ICAO_LENGTH
      ICAO_LENGTH = (INDEX-5)
      

! Check if current details string has the searched WMO string within
! NOTE. These alignments should be the same as those in name_search.F

      IF (DETAILS(1:ICAO_LENGTH).EQ.ICAO_ID) THEN
         WRITE(6,'("<TR ALIGN=CENTER>")')
         WRITE(6,'(6A)')'<TD ALIGN="CENTER">',             
     &'<A HREF="http://www-metdb/cgi-bin/station_search/', !2
     &'search_full_icaolist.sh?',
     &   DETAILS(1:4),'">',DETAILS(1:4),'</A></TD>'                  
         WRITE(6,'("<TD>",A5,"</TD>")')DETAILS(6:10)
         WRITE(6,'("<TD>",A42,"</TD>")')DETAILS(39:81)
         WRITE(6,'("<TD>",A8,"</TD>")')DETAILS(12:18)
         WRITE(6,'("<TD>",A8,"</TD>")')DETAILS(20:27)

         WRITE(6,'("<TD>",13A,"</TD>")')'<A HREF="',    !1.2
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
      ENDIF

! Set return status to 0 for ok
      write_icao_details=0

! Return and end
      RETURN
      END
