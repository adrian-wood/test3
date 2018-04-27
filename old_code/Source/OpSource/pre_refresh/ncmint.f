      SUBROUTINE NCMINT(VALUES,CNAM,RLAT,RLON,YEAR,MONTH,DAY,HOUR,MNT,          
     &                  BULCOD,CCCC,STNID)                                      
                                                                                
!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : NCMINT  IN TFMTRT                                            
!                                                                              
! PURPOSE       : TO INITIALISE SOME ELEMENTS OF THE EXPANSION ARRAY           
!                 WITH DATA NOT AVAILABLE FROM THE REPORT.                     
!                                                                              
! DESCRIPTION   : THE INDEX AND TRAILER ARE USED TO INITIALISE PART            
!                 OF THE EXPANSION ARRAY WITH AVAILABLE DATA.                  
!                 ALL OTHER VALUES IN THE EXPANSION ARRAY ARE SET TO           
!                 THE DEFAULT VALUE FOR MISSING '-9999999'.                    
!                                                                              
! DATA TYPE(S)  : NCM                                                          
!  HANDLED                                                                     
!                                                                              
! CALLED BY     : TFMRET                                                       
!                                                                              
! CALLS         : NOTHING                                                      
!                                                                              
! PARAMETERS    : (1) VALUES     EXPANSION ARRAY          (I/O)                 
!                 (2) CNAM       CHARACTER ELEMENT DATA   (O)                  
!                 (3) RLAT       LATITUDE                 (I)                  
!                 (4) RLON       LONGITUDE                (I)                  
!                 (5) YEAR       YEAR                     (I)                  
!                 (6) MONTH      MONTH                    (I)                  
!                 (7) DAY        DAY                      (I)                  
!                 (8) HOUR       HOUR                     (I)                  
!                 (9) MNT        MINUTE                   (I)                  
!                (10) BULCOD     BULLETIN IDENTIFIER      (I)                  
!                (11) CCCC       COLLECTING CENTRE        (I)                  
!                (12) STNID      STATION IDENTIFIER       (I)                  
!                                                                              
!Y2K  26.06.1997  NCMINT is Year 2000 compliant.                                
!                                                                              
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:45$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ncmint.F,v $
!
! CHANGE RECORD :                                                              
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:45    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:02  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:16:46  13:16:46  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
! 
! Revision 1.1  1997/02/17 11:54:22  uspm
! Initial revision
!
! FEB 96         INTRODUCED TO ALLOW NCM RETRIEVAL.         
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
                                                                                
      REAL          RLAT          ! Station latitude                            
      REAL          RLON          ! Station longitude                           
      REAL          VALUES(79)    ! Expansion array                             
                                                                                
      CHARACTER*(*) BULCOD        ! Bulletin identifier                         
      CHARACTER*(*) CCCC          ! Originating centre                          
      CHARACTER*(*) CNAM          ! Character element data                      
      CHARACTER*(*) STNID         ! Station identifier                          
                                                                                
      INTEGER       DAY           ! Day of report validity                      
      INTEGER       HOUR          ! Hour of report validity                     
      INTEGER       LOOP          ! General loop variable                       
      INTEGER       MNT           ! Minute of report validity                   
      INTEGER       MONTH         ! Month of report validity                    
      INTEGER       YEAR          ! Year of report validity                     
                                                                                                                                                               
      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/ncmint.F,v $
     &'//'$ $Revision: 1$ $Date: 30/01/2006 20:23:45$ '
                                                                                
! Initialise variables                                                          
                                                                                
! Convert the station identifier into a numeric format.                         
                                                                                
      READ (STNID,'(F5.0)') VALUES(1)                                           
                                                                                
! Initialise other array values.                                                
                                                                                
      VALUES(2)=RLAT                                                            
      VALUES(3)=RLON                                                            
      VALUES(4)=YEAR                                                            
      VALUES(5)=MONTH                                                           
      VALUES(6)=DAY                                                             
      VALUES(7)=HOUR                                                            
      VALUES(8)=MNT                                                             
                                                                                
! Set the displacement for the CCCC and BULCOD data into the expansion          
! array and the character data into the character string CNAM for use           
! with the GETCHR function on retrieval.                                        
                                                                                
      VALUES(9)=(65536*4)+1                                                     
      VALUES(10)=(65536*4)+5                                                    
                                                                                
      CNAM(1:4)=CCCC                                                            
      CNAM(5:8)=BULCOD                                                          
                                                                                
! Set all the rest of the expansion array values to 'missing'.                  
                                                                                
      DO LOOP=11,79                                                             
        VALUES(LOOP)=-9999999.                                                  
      ENDDO                                                                                                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
