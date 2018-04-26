      INTEGER FUNCTION WRITE_HTML_HEAD(TITLE)
!***11111111111111111111111111111111111111111111111111111111111111111111        
!                                                                               
! FILE NAME: write_html_head.f
!
! TYPE: function
!
! LANGUAGE: FORTRAN 77         
!                                                                               
!***22222222222222222222222222222222222222222222222222222222222222222222        
!                                                                               
! PURPOSE: Writes out a Standard start html web page.
!
!***33333333333333333333333333333333333333333333333333333333333333333333        
!                                                                               
! STRUCTURE: 
!  Writes out html tags which tends to be written to standard output
!  the Title to give the page is input to the function.
!  if the function completes ok a status of 0 is returned otherwise 
!  99 is returned to indicate an error.
!                                           
! REFERENCES:                                                                
!   Web reference books                 change here                     
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
!     99 - ERROR                                                        
!                                                                               
!***66666666666666666666666666666666666666666666666666666666666666666666
!
! INTERFACE
!  
!
!***77777777777777777777777777777777777777777777777777777777777777777777 
!
! Code written by Stanley Kellett, MetDB team, April 2000       
!
! CHANGE HISTORY
!
! $Revision: 1$
! $Date: 31/08/2006 14:18:40$
! $Source: /home/us0400/usmdb/station_search/source/RCS/write_html_head.F,v $                                                                
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         31/08/2006 14:18:40    Stan Kellett    
! $
! Revision 1.2  2001/09/03 15:07:16  usmdb
! updated to compile on f90 compiler.
!
! Revision 1.1  2000/06/07 15:25:59  usmdb
! Initial revision
!
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 
      
! Scalar arguments with intent in
      CHARACTER*(*) TITLE          ! Title for web page

! Code for Revision software
      CHARACTER*132 HEAD 
      LOGICAL HEADSET              !1.2
      DATA HEADSET /.FALSE./       !1.2
      
      IF (.NOT.HEADSET) THEN
        HEAD='
     &        $Source: /home/us0400/usmdb/station_search/source/RCS/write_html_head.F,v $
     &       '//'$Date: 31/08/2006 14:18:40$ $Revision: 1$'
        HEADSET=.TRUE.
      ENDIF

! Set return to 99 in case rest of program fails
      WRITE_HTML_HEAD=99 

! Write Out HTML code
      WRITE(6,'("<HTML>")')
      WRITE(6,'("<HEAD>")')
      WRITE(6,'("<TITLE>",A,"</TITLE>")')TITLE             
      WRITE(6,'("</HEAD>")')
      WRITE(6,'("<BODY BGCOLOR=WHITE TEXT=BLACK>")')
      WRITE(6,'("<H2>",A,"</H2>")')TITLE                   

! Now complete so set return to 0
      WRITE_HTML_HEAD=0

! Return and end
      RETURN
      END
