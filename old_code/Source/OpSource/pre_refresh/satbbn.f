      SUBROUTINE SATBBN(POINT,CRUDELAT,CRUDELON,NORTH,EAST,NN,BULL)     
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SATBBN                                               
!                                                                      
! PURPOSE       : DECODES BBBNN GROUP IN A SATOB SECTION 2,3,4,5 OR 7  
!                                                                      
! CALLED BY     : SATOB2, SATOB3, SATOB4, SATOB5, SATOB7               
!                                                                     
! CALLS         : NIL                                                  
!                                                                      
! PARAMETERS    : (1) POINT    POINTER TO WHERE TO START IN BULLETIN   
!                 (2) CRUDELAT LATITUDE  OF A TEN DEGREE SQUARE        
!                 (3) CRUDELON LONGITUDE OF A TEN DEGREE SQUARE        
!                 (4) NORTH    SET TO TRUE IF NORTHERN HEMISPHERE      
!                 (5) EAST     SET TO TRUE IF EAST OF GREENWICH MERID  
!                 (6) NN       NUMBER OF REPORTS FROM 10 DEGREE SQUARE 
!                 (7) BULL     REPORT DATA                             
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:05$
! $Source: /home/us0400/mdb/op/lib/source/RCS/satbbn.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:05    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:47  usmdb
! Separated variable declaration and initialisation. Added copyright
! and modified header - S.Cox
!
! Revision 1.2  97/07/31  09:33:03  09:33:03  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:12:34  uspm
! Initial revision
!
! 05/06/96  REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY NEW PARAMETER 
!           'BULL'.                                                    
!
! OCT 93 : FIRST WRITTEN                                              
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE                                                     
                                                                        
      INTEGER POINT, CRUDELAT, CRUDELON, NN                             
      INTEGER B1, B2, B3                                                
      INTEGER IVALUE                                                    
      INTEGER MISSING                                               !2.0
*                                                                       
      LOGICAL NORTH, EAST                                               
*                                                                       
      CHARACTER*(*)  BULL                                               
      CHARACTER*132  HEAD
*                                                                       
      DATA MISSING /-9999999/                                       !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/satbbn.F,v $
     &'//'$ $Date: 30/01/2006 20:24:05$ $Revision: 1$'
      
      NN = IVALUE(BULL(POINT+3:POINT+4))                                
      CRUDELAT = 0                                                      
      CRUDELON = 0                                                      
      B1 = IVALUE(BULL(POINT:POINT))     ! OCTANT                       
      IF (B1.EQ.4 .OR. B1.EQ.9 .OR. B1.EQ.MISSING) THEN                 
         CRUDELAT = MISSING                                             
         CRUDELON = MISSING                                             
         RETURN                                                         
      END IF                                                            
      B2 = IVALUE(BULL(POINT+1:POINT+1))     ! TENS FIGURE OF LAT       
      IF (B2.EQ.9 .OR. B2.EQ.MISSING) CRUDELAT = MISSING                
      B3 = IVALUE(BULL(POINT+2:POINT+2))     ! TENS FIGURE OF LON       
      IF (             B3.EQ.MISSING) CRUDELON = MISSING                
      IF (CRUDELAT .NE. MISSING) THEN                                   
         CRUDELAT = B2*10                                               
         IF (B1.GE.5) THEN                                              
            CRUDELAT = 0-CRUDELAT                                       
            NORTH    = .FALSE.                                          
         ELSE                                                           
            NORTH    = .TRUE.                                           
         END IF                                                         
      END IF                                                            
*                                                                       
      IF (CRUDELON .NE. MISSING) THEN                                   
         IF ((B1.EQ.1).OR.(B1.EQ.2).OR.(B1.EQ.6).OR.(B1.EQ.7)) THEN     
            IF (B3.NE.9) B3 = B3+10                                     
         END IF                                                         
         CRUDELON = B3*10                                               
         IF ((B1.EQ.0).OR.(B1.EQ.1).OR.(B1.EQ.5).OR.(B1.EQ.6)) THEN     
            CRUDELON = 0-CRUDELON                                       
            EAST     = .FALSE.                                          
         ELSE                                                           
            EAST     = .TRUE.                                           
         END IF                                                         
      END IF                                                            
*                                                                       
      BULL(POINT:POINT+2) = 'BBB'  ! ##############                     
      RETURN                                                            
      END                                                               
