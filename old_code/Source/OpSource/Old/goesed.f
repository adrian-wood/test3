      SUBROUTINE GOESED(ISTART,IEND,BULL)                               
      IMPLICIT NONE                                                     
                                                                        
!-----------------------------------------------------------------------
!                                                                       
! PROGRAM       : GOESED                                                
!                                                                       
! PURPOSE       : TO TIDY UP BULLETIN AFTER HEADER INFORMATION FOUND.   
!                                                                       
! DESCRIPTION   : FIGURE, LETTER, SPACE, SLASH, PLUS, EQUALS, PLUS      
!                 AND MINUS ARE ACCEPTABLE CHARACTERS.                  
!                 IF TWO SUCCESSIVE CHARACTERS ARE SPACE OR LESS,       
!                 REDUCE THEM TO A SINGLE SPACE.  REPLACE ANY OTHER     
!                 UNEXPECTED CHARACTER BY A SLASH.                      
!                                                                       
! CALLED BY     :                                                       
!                                                                       
! CALLS         : NOTHING                                               
!                                                                       
! PARAMETERS    : (1) ISTART   STARTING POINT IN BULLETIN               
!                 (2) IEND     END POINT (RESET IF SPACES MERGED)       
!                 (3) BULL     REPORT DATA                              
!                                                                       
! REVISION INFO :
!
! $Revision: 2$
! $Date: 22/06/2007 14:43:09$
! $Source: /home/us0400/mdb/op/lib/source/RCS/goesed.F,v $
!                                                                      
! CHANGE RECORD :                                                       
!
! $Log:
!  2    Met_DB_Project 1.1         22/06/2007 14:43:09    Brian Barwell
!       Obsolete module used for storage of GOESAMW, GOESVIS and GOESWV data
!       which terminated in August 2006.
!  1    Met_DB_Project 1.0         30/01/2006 20:22:38    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:49  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  98/03/12  09:03:32  09:03:32  usmdb (Generic MetDB accou
! Initial revision
!                                                                       
! 02/03/98 : Made from BULLED but handles a character array rather than  
!            a character string.                                         
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        
!                                                                       
                                                                        
!Declare Character                                                      
      CHARACTER*(*)  BULL(*)              !Bulletin being processed     
      CHARACTER*1    CH                   !Char being checked           
      CHARACTER*132  HEAD                 !Revision information         
                                                                        
!Declare Integer                                                        
      INTEGER        N                    !Pointer within bulletin      
      INTEGER        ISTART               !Pointer to start of bulletin 
      INTEGER        IEND                 !Pointer to end of bulletin   
      INTEGER        IN                   !Pointer within bulletin      
                                                                        
      SAVE                                                              
      HEAD='                                                            
     &$Source:                                                          
     &'//'$ $Date: 22/06/2007 14:43:09$ $Revision: 2$'                
                                                                        
                                                                        
                                                                        
! Two pointers to the same string are used, IN & N (input & output).    
! N is not advanced when a character is skipped, so can be less than IN.
                                                                        
      N=ISTART                                                          
                                                                        
      DO 100 IN=ISTART,IEND                                             
      IF (BULL(IN).LE.' ' .AND. BULL(IN+1).LE.' ') THEN                 
        BULL(IN+1)=' '                                                  
      ELSE IF (BULL(IN).LE.' ' .AND. BULL(IN+1).EQ.'=') THEN            
        CONTINUE                                                        
      ELSE                                                              
        CH=BULL(IN)                                                     
        IF (CH.EQ.' ' .OR. CH.EQ.'/' .OR. CH.EQ.'='                     
     &            .OR. CH.EQ.'+' .OR. CH.EQ.'-'                         
     &            .OR. (CH.GE.'A'.AND.CH.LE.'I')                        
     &            .OR. (CH.GE.'J'.AND.CH.LE.'R')                        
     &            .OR. (CH.GE.'S'.AND.CH.LE.'Z')                        
     &            .OR. (CH.GE.'0'.AND.CH.LE.'9')) THEN                  
          BULL(N)=CH                                                    
        ELSE                                                            
          BULL(N)='/'                                                   
        ENDIF                                                           
        N=N+1                                                           
      ENDIF                                                             
 100  CONTINUE                                                          
*                                                                       
      IEND=N                                                            
      RETURN                                                            
      END                                                               
