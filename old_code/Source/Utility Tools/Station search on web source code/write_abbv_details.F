      INTEGER FUNCTION WRITE_ABBV_DETAILS(INDEX,WMO_NUMBER,DETAILS)
!***11111111111111111111111111111111111111111111111111111111111111111111        
!                                                                               
! FILE NAME: write_abbv_details.f
!
! TYPE: subroutine/function/program
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
!  Function first of all checks the size of strng holding wmo number
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
! Original code written by Stanley Kellett, MetDB team, April 2000
!
! CHANGE HISTORY 
!
! $Revision: 4$
! $Date: 10/04/2012 14:17:32$
! $Source: /home/us0400/usmdb/station_search/source/RCS/write_abbv_details.F,v $                                                               
!                                                                               
! $Log:
!  4    Met_DB_Project 1.3         10/04/2012 14:17:32    Paul Barnham
!       Change from rexx script to use perl script for retrieval.
!  3    Met_DB_Project 1.2         08/09/2006 09:15:49    Stan Kellett
!       changed els018 link to www-metdb-test alias
!  2    Met_DB_Project 1.1         31/08/2006 17:15:33    Stan Kellett    Link
!       changed so it will run from Linux rather than us0400
!  1    Met_DB_Project 1.0         31/08/2006 14:19:28    Stan Kellett    
! $
! Revision 1.4  2001/09/04 08:14:15  usmdb
! updated to compile on f90 compiler.
!
! Revision 1.3  2001/05/10 12:08:42  usmdb
! fixed a bug which displayed airfield height rather than sensor height.
!
! Revision 1.2  2000/08/01 15:19:17  usmdb
! add field to retrieve latest LNDSYN.
!
! Revision 1.1  2000/06/09  12:15:22  12:15:22  usmdb (Generic MDB account)
! Initial revision
!   
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 
      
! Revision Control Variables
      CHARACTER*132 HEAD
      LOGICAL HEADSET            !1.4
      DATA    HEADSET /.FALSE./  !1.4

! Scalar arguments with intent in
      INTEGER   INDEX             !1.4 Last charact of string sent from form
      CHARACTER*5 WMO_NUMBER      ! String holding WMO_number
      CHARACTER*89 DETAILS        ! String holding Station details

! Local Scalars
      INTEGER   WMO_LENGTH        !1.3 length of WMO_NUMBER search string

! Revision Code
      IF (.NOT.HEADSET) THEN
        HEAD='
     &        $Source: /home/us0400/usmdb/station_search/source/RCS/write_abbv_details.F,v $
     &       '//'$Date: 10/04/2012 14:17:32$ $Revision: 4$'
        HEADSET=.TRUE.
      ENDIF

! Calculate WMO_LENGTH
      WMO_LENGTH = (INDEX-7)

! Chck if current details string has the searched WMO string within
      IF (DETAILS(2:(WMO_LENGTH+1)).EQ.WMO_NUMBER) THEN
         WRITE(6,'("<TR ALIGN=CENTER>")')
         WRITE(6,'("<TD>",A,A,A5,A,A5,A,"</TD>")')
     &   '<A HREF="http://www-metdb-test/cgi-bin/station_search', !2
     &   '/search_full_list.sh?',
     &   DETAILS(2:6),'">',
     &   DETAILS(2:6),'</A>'
         WRITE(6,'("<TD>",A32,"</TD>")')DETAILS(58:89)
         WRITE(6,'("<TD>",A7," metres</TD>")')DETAILS(28:33)!1.3
         WRITE(6,'("<TD>",A8,"</TD>")')DETAILS(9:16)
         WRITE(6,'("<TD>",A8,"</TD>")')DETAILS(17:24)

         WRITE(6,'("<TD>",13A,"</TD>")')'<A HREF="',    !1.2
     &   'http://www01/SysCGI/MetDB_webret.perl?',		!1.2
     &   'subtype=LNDSYN&stn01=',DETAILS(2:6),          !1.2
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
      write_abbv_details=0

! Return and end
      RETURN
      END
