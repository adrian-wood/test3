      INTEGER FUNCTION WRITE_NAME_DETAILS(DETAILS)
!***11111111111111111111111111111111111111111111111111111111111111111111        
!                                                                               
! FILE NAME: write_name_details.F
!
! TYPE: function
!
! LANGUAGE: FORTRAN 77         
!                                                                               
!***22222222222222222222222222222222222222222222222222222222222222222222        
!                                                                               
! PURPOSE: Writes out a line of details as a row in a web table.
!
!***33333333333333333333333333333333333333333333333333333333333333333333        
!                                                                               
! STRUCTURE: 
!          Write to Standard output the station details as a row.
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
! Original Code by Stanley Kellett, MetDB team, April 2000   
!
! CHANGE HISTORY      
!
! $Revision: 4$
! $Date: 10/04/2012 15:38:47$
! $Source: /home/us0400/usmdb/station_search/source/RCS/write_name_details.F,v $                                                          
!                                                                               
! $Log:
!  4    Met_DB_Project 1.3         10/04/2012 15:38:47    Paul Barnham
!       Change from rexx script to use perl script for retrieval
!  3    Met_DB_Project 1.2         08/09/2006 09:16:03    Stan Kellett
!       changed els018 link to www-metdb-test alias
!  2    Met_DB_Project 1.1         31/08/2006 17:31:57    Stan Kellett    Link
!       changed to run cgi from Linux server rather than us0400.
!  1    Met_DB_Project 1.0         31/08/2006 15:58:25    Stan Kellett    
! $
! Revision 1.5  2001/09/04 08:36:31  usmdb
! updated for f90 compiler.
!
! Revision 1.4  2000/08/02 16:29:55  usmdb
! set retrieve align to center.
!
! Revision 1.3  2000/08/02  16:11:12  16:11:12  usmdb (Generic MDB account)
! added facility to retrieve latest Synop.
! 
! Revision 1.2  2000/06/29  11:21:42  11:21:42  usmdb (Generic MDB account)
! corected 4A to 5A
! 
! Revision 1.1  2000/06/09  10:56:36  10:56:36  usmdb (Generic MDB account)
! Initial revision
! 
!                                                                               
!***88888888888888888888888888888888888888888888888888888888888888888888
        
      IMPLICIT NONE 
      
! Scalar arguments with intent in
      CHARACTER*89 DETAILS          ! Line of Station Details

! Code for Revision software
      CHARACTER*132 HEAD
      LOGICAL HEADSET               !1.5
      DATA    HEADSET /.FALSE./     !1.5

      IF (.NOT.HEADSET) THEN
        HEAD='
     &        $Source: /home/us0400/usmdb/station_search/source/RCS/write_name_details.F,v $
     &       '//'$Date: 10/04/2012 15:38:47$ $Revision: 4$'
        HEADSET=.TRUE.
      ENDIF

! Set return to 99 in case rest of program fails
      WRITE_NAME_DETAILS=99 

! Write Out HTML code
      WRITE(6,'(''<TR>'')')
      WRITE(6,'(''<TD ALIGN=CENTER>'',A42,''</TD>'')')DETAILS(57:89)
      WRITE(6,'(''<TD ALIGN=CENTER>'',5A,''</A></TD>'')')         !1.2
     & '<A HREF="http://www-metdb-test/cgi-bin/',              !2
     & 'station_search/search_full_list.sh?', 
     &                          DETAILS(2:6),'">',                
     &                          DETAILS(2:6)                      
      WRITE(6,'(''<TD ALIGN=CENTER>'',A6,'' metres</TD>'')')
     &               DETAILS(28:33)
      WRITE(6,'(''<TD ALIGN=CENTER>'',A7,''</TD>'')')DETAILS(9:15)
      WRITE(6,'(''<TD ALIGN=CENTER>'',A8,''</TD>'')')DETAILS(17:24)

         WRITE(6,'("<TD ALIGN=CENTER>",13A,"</TD>")')   !1.4
     &           '<A HREF="',                           !1.4
     &   'http://www01/SysCGI/MetDB_webret.perl?',		!1.3
     &   'subtype=LNDSYN&stn01=',DETAILS(2:6),          !1.3
     &   '&stn02=&stn03=&stn04=&stn05=&stn06=&stn07=',  !1.3
     &   '&stn08=&stn09=&stn10=',                       !1.3
     &   '&submit=Retrieve+Latest+Report',              !1.3
     &   '&startdate=20000101&starthour=00',            !1.3
     &   '&startminute=00&enddate=20000101',            !1.3
     &   '&endhour=23&endminute=59&maxobs=0500&header=no',!1.3
     &   '&versions=no&increment=00&area_n=&area_s=',   !1.3
     &   '&area_w=&area_e=">',                          !1.3
     &   'Retrieve </A>'                                !1.3
      WRITE(6,'(''</TR>'')')

! Now complete so set return to 0
      WRITE_NAME_DETAILS=0

! Return and end
      RETURN
      END
