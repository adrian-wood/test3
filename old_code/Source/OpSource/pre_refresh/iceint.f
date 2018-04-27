      SUBROUTINE ICEINT(ARRAY,CDATA,YEAR,MONTH,DAY,HOUR,MIN,TOR,REPLEN, 
     &                  TTAAII,CCCC,THISID,CORFLAG)                     
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : ICEINT                                               
!                                                                      
! PURPOSE       : TO INITIALISE THE ELEMENTS AVAILABLE FOR SEAICE      
!                 AND TROPADV RETRIEVAL.                               
!                                                                      
! DESCRIPTION   : A REAL ARRAY IS USED TO HOLD NUMERIC ELEMENTS AND    
!                 A CHARACTER STRING HOLDS CHARACTER ELEMENTS.         
!                                                                      
! DATA TYPE(S)  : SEAICE, TROPADV                                      
!                                                                      
! CALLED BY     : ICEREP                                               
!                                                                      
! CALLS         : NONE                                                 
!                                                                      
! FUNCTIONS     : NONE                                                 
!                                                                      
! PARAMETERS    : (1) ARRAY     EXPANSION ARRAY          (I/O)         
!                 (2) CDATA     CHARACTER ELEMENT DATA   (O)           
!                 (3) YEAR      YEAR OF REPORT           (I)           
!                 (4) MONTH     MONTH OF REPORT          (I)           
!                 (5) DAY       DAY OF REPORT            (I)           
!                 (6) HOUR      HOUR OF REPORT           (I)           
!                 (7) MIN       MINUTE OF REPORT         (I)           
!                 (8) TOR       TIME OF RECEIPT ARRAY    (I)           
!                 (9) REPLEN    REPORT LENGTH            (I)           
!                (10) TTAAII    BULLETIN IDENTIFIER      (I)           
!                (11) CCCC      ORIGINATING CENTRE       (I)           
!                (12) THISID    REPORT IDENTIFIER        (I)           
!                (13) CORFLAG   CORRECTION FLAG          (I)           
!                                                                      
!Y2K  26.06.1997  ICEINT is Year 2000 compliant.                        
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:51$
! $Source: /home/us0400/mdb/op/lib/source/RCS/iceint.F,v $
!                                                                     
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:51    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:45  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  16:07:32  16:07:32  uspm (Pat McCormack)
! Correct position of continuation character in definition of HEAD
! 
! Revision 1.1  1997/08/04 13:47:38  uspm
! Initial revision
!                                                                 
! 10 SEP 1996  : ADDITION OF CORRECTION FLAG TO ELEMENT LIST          
!                                                                      
! 29 JUL 1996  : INTRODUCED                                     
!                                                                      
!----------------------------------------------------------------------
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
                                                                        
       INTEGER           CORFLAG    ! Correction flag.                  
       INTEGER           DAY        ! Day of report                     
       INTEGER           HOUR       ! Hour of report                    
       INTEGER           MIN        ! Minute of report                  
       INTEGER           MONTH      ! Month of report                   
       INTEGER           REPLEN     ! Length of report                  
       INTEGER           TOR(*)     ! Array of time of receipt data.    
       INTEGER           YEAR       ! Year of report                    
                                                                        
       REAL              ARRAY(*)   ! Expanded numeric values.                                                                                  
                                                                        
       CHARACTER*(*)     CCCC       ! Originating centre                
       CHARACTER*(*)     CDATA      ! String to hold text element data  
       CHARACTER*(*)     THISID     ! Report identifier                 
       CHARACTER*(*)     TTAAII     ! Bulletin identifier               
       CHARACTER*132     HEAD       ! Revision information
                                                                        
! Initialise array.                                                     
                                                                        
       HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/iceint.F,v $
     &'//'$ $Date: 30/01/2006 20:22:51$ $Revision: 1$'
      
       ARRAY(1)=(65536*6)+1         ! Report identifier                 
       ARRAY(2)=YEAR                ! Year of report                    
       ARRAY(3)=MONTH               ! Month of report                   
       ARRAY(4)=DAY                 ! Day of report                     
       ARRAY(5)=HOUR                ! Hour of report                    
       ARRAY(6)=MIN                 ! Minute of report                  
       ARRAY(7)=TOR(1)              ! Year of receipt                   
       ARRAY(8)=TOR(2)              ! Month of receipt                  
       ARRAY(9)=TOR(3)              ! Day of receipt                    
       ARRAY(10)=TOR(4)             ! Hour of receipt                   
       ARRAY(11)=TOR(5)             ! Minute of receipt                 
       ARRAY(12)=(65536*6)+10       ! Bulletin identifier               
       ARRAY(13)=(65536*4)+16       ! Originating/collection centre     
       ARRAY(14)=REPLEN             ! Length of report                  
                                                                        
       IF (CORFLAG .GT. 0) THEN       ! Set correction flag             
         ARRAY(15)=CORFLAG-1                                            
       ELSE                                                             
         ARRAY(15)=CORFLAG                                              
       ENDIF                                                            
                                                                        
       CDATA(1:9)=THISID                                                
       CDATA(10:15)=TTAAII                                              
       CDATA(16:19)=CCCC                                                
                                                                                                                                                
       RETURN                                                           
       END                                                              
