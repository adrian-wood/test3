      SUBROUTINE SRWINT(VALUES,CNAM,LAT,LON,YEAR,MONTH,DAY,HOUR,BULCOD, 
     &                  CCCC,STNID)                                     
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SRWINT                                               
!                                                                      
! PURPOSE       : TO INITIALISE ELEMENTS OF THE EXPANSION ARRAY WITH   
!                 DATA NOT AVAILABLE IN THE REPORT.                    
!                                                                      
! DESCRIPTION   : DATA FROM THE INDEX ENTRY AND TRAILER IS USED TO     
!                 FILL PART OF THE EXPANSION ARRAY TO ALLOW RETRIEVAL  
!                 OF ELEMENTS NOT CONTAINED IN THE REPORT WITHOUT      
!                 CALLING THE EXPANSION PROGRAM SRWEXP.                
!                                                                      
! DATA TYPE(S)  : SREW                                                 
!                                                                      
! CALLED BY     : TFMRET                                               
!                                                                      
! CALLS         : NONE                                                 
!                                                                      
! PARAMETERS    : (1) VALUES    EXPANSION ARRAY          (I/O)         
!                 (2) CNAM      CHARACTER ELEMENT DATA   (O)           
!                 (3) LAT       LATITUDE                 (I)           
!                 (4) LON       LONGITUDE                (I)           
!                 (5) YEAR      YEAR OF REPORT           (I)           
!                 (6) MONTH     MONTH OF REPORT          (I)           
!                 (7) DAY       DAY OF REPORT            (I)           
!                 (8) HOUR      HOUR OF REPORT           (I)           
!                 (9) BULCOD    BULLETIN IDENTIFIER      (I)           
!                (10) CCCC      COLLECTION CENTRE        (I)           
!                (11) STNID     STATION IDENTIFIER       (I)           
!                                                                      
!Y2K  26.06.1997  SRWINT is Year 2000 compliant.                        
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:25$
! $Source: /home/us0400/mdb/op/lib/source/RCS/srwint.F,v $
!                                                                      
! CHANGE RECORD :
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:25    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:12  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:33:49  13:33:49  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
! 
! Revision 1.1  1997/02/17 11:56:54  uspm
! Initial revision
!
! FEB 96 INTRODUCED TO ALLOW SREW RETRIEVAL.                  
!                                                                      
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        
                                                                        
      IMPLICIT NONE                                                     
                                                                                                                                                
! Declare variables                                                     
                                                                        
      CHARACTER*(*)     BULCOD     ! Bulletin identifier               
      CHARACTER*(*)     CCCC       ! Originating centre                
      CHARACTER*(*)     CNAM       ! String to hold text element data  
      CHARACTER*(*)     STNID      ! Station identifier                
                                                                        
      REAL              VALUES(11) ! Expanded data values              
      REAL              LAT        ! Station latitude                  
      REAL              LON        ! Station longitude                 
                                                                        
      INTEGER           YEAR       ! Year of observation               
      INTEGER           MONTH      ! Month of observation              
      INTEGER           DAY        ! Day of observation                
      INTEGER           HOUR       ! Hour of observation               
      INTEGER           IERROR     ! I/O error code                                                                        
                                                                                                                                              
      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/srwint.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:24:25$ '   

! Convert the station identifier into numeric format.                   
                                                                        
       READ (STNID,'(F5.0)',IOSTAT=IERROR) VALUES(1)                
                                                                        
! Initialise the rest of the array values                               
                                                                        
       VALUES(2)=LAT                ! Latitude                          
       VALUES(3)=LON                ! Longitude                         
       VALUES(4)=YEAR               ! Year of observation               
       VALUES(5)=MONTH              ! Month of observation              
       VALUES(6)=DAY                ! Day of observation                
       VALUES(7)=HOUR               ! Hour of observation               
       VALUES(8)=-9999999           ! Not used (minute of observation)  
       VALUES(9)=(65536*4)+1        ! Collection centre                 
       VALUES(10)=(65536*4)+5       ! Bulletin identifier               
       VALUES(11)=-9999999          ! Set mean precipitation to missing.
                                                                        
       CNAM(1:4)=CCCC                                                   
       CNAM(5:8)=BULCOD                                                 
                                                                                                                                                
       RETURN                                                           
       END                                                              
