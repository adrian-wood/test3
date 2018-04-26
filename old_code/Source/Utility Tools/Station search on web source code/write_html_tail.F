      INTEGER FUNCTION WRITE_HTML_TAIL()
!***11111111111111111111111111111111111111111111111111111111111111111111        
!                                                                               
! FILE NAME: write_html_tail.f
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
!  These are the tags which end a html web page.
!                                           
! REFERENCES:                                                                
!   Web reference books                                      
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
! Code Written by Stanley Kellett, MetDB team, April 2000
!
! CHANGE HISTORY                                                                
!            
! $Revision: 1$
! $Date: 31/08/2006 14:20:36$
! $Source: /home/us0400/usmdb/station_search/source/RCS/write_html_tail.F,v $
!                                                                   
! $Log:
!  1    Met_DB_Project 1.0         31/08/2006 14:20:36    Stan Kellett    
! $
! Revision 1.2  2001/09/03 15:09:06  usmdb
! updated to compile on f90 compiler
!
! Revision 1.1  2000/06/07 15:47:11  usmdb
! Initial revision
!
! Revision 1.1  2000/06/07  15:34:08  15:34:08  usmdb (Generic MDB account)
! Initial revision
! 
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 
      
! Code for Revision software
      CHARACTER*132 HEAD
      LOGICAL HEADSET              !1.2
      DATA HEADSET /.FALSE./       !1.2
 
      IF (.NOT.HEADSET) THEN
        HEAD='
     &        $Source: /home/us0400/usmdb/station_search/source/RCS/write_html_tail.F,v $
     &       '//'$Date: 31/08/2006 14:20:36$ $Revision: 1$'
        HEADSET=.TRUE.
      ENDIF


! Set return to 99 in case rest of program fails
      WRITE_HTML_TAIL=99 

! Write Out HTML code
      WRITE(6,'("</BODY>")')
      WRITE(6,'("</HTML>")')

! Now complete so set return to 0
      WRITE_HTML_TAIL=0

! Return and end
      RETURN
      END
