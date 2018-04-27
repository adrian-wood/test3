      SUBROUTINE SATOB3(POINT,KNOTS,DCVALS,NOBS,BULL)                   

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SATOB3                                               
!                                                                      
! PURPOSE       : TO DECODE A SATOB SECTION 3                          
!                                                                      
! CALLED BY     : SATOB1                                               
!                                                                      
! CALLS         : SATBBN                                               
!                                                                      
! PARAMETERS    : (1) POINT    POINTER TO WHERE TO START IN BULLETIN   
!                 (2) KNOTS    TRUE IF WIND SPEED IN KNOTS             
!                 (3) DCVALS   REAL ARRAY TO HOLD DECODED VALUES       
!                 (4) NOBS     NUMBER OF 'OBSERVATIONS' IN SECTION     
!                 (5) BULL     REPORT DATA                             
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:07$
! $Source: /home/us0400/mdb/op/lib/source/RCS/satob3.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:07    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:49  usmdb
! Separated variable declaration and initialisation. Removed
! unused variables, added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  11:33:21  11:33:21  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:20:31  uspm
! Initial revision
!
! 10/02/97  CORRECTLY DECODE WIND DIRECTION AND SPEED WHEN SPEED     !A 
!           REPORTED AS 500 UNITS OR MORE.                           !A 
!                                                                      
! 05/06/96  REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY NEW           
!           PARAMETER 'BULL'.                                          
!                                                                      
!  OCT 93 : FIRST WRITTEN                                              
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
*                                                                       
      INTEGER POINT, NN, NNTOT                                          
      INTEGER CRUDELAT, CRUDELON, LATUNIT, LONUNIT                      
      INTEGER PRESSURE, DDD, FFF                                        
      INTEGER I, J                                                  !2.0
      INTEGER IVALUE, NOBS                                              
      INTEGER MISSING                                               !2.0
*                                                                       
      REAL DCVALS(*)                                                    
*                                                                       
      CHARACTER*(*)  BULL                                               
      CHARACTER*132  HEAD
*                                                                       
      LOGICAL KNOTS, CALM, NORTH, EAST                                  
*                                                                       
      DATA MISSING /-9999999/                                       !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/satob3.F,v $
     &'//'$ $Date: 30/01/2006 20:24:07$ $Revision: 1$'
      
      I     = NOBS*9 + 1  ! NOBS*9 FROM SECTION ONE                     
      NNTOT = 0                                                         
*                                                                       
   10 CONTINUE                                                          
      CALL SATBBN(POINT,CRUDELAT,CRUDELON,NORTH,EAST,NN,BULL)           
      POINT = POINT+6                                                   
      NNTOT = NNTOT+NN                                                  
*                                                                       
      DO J = 0,NN-1                                                     
         LATUNIT = IVALUE(BULL(POINT + J*12:POINT + J*12))              
         IF (CRUDELAT.EQ.MISSING .OR. LATUNIT.EQ.MISSING) THEN          
            DCVALS(I + J) = MISSING                                     
         ELSE IF (NORTH) THEN                                           
            DCVALS(I + J) = CRUDELAT+LATUNIT                            
         ELSE                                                           
            DCVALS(I + J) = CRUDELAT-LATUNIT                            
         END IF                                                         
*                                                                       
         LONUNIT = IVALUE(BULL(POINT + J*12 + 1:POINT + J*12 +1))       
         IF (CRUDELON.EQ.MISSING .OR. LONUNIT.EQ.MISSING) THEN          
            DCVALS(I + NOBS + J) = MISSING                              
         ELSE IF (EAST) THEN                                            
            DCVALS(I + NOBS + J) = CRUDELON+LONUNIT                     
         ELSE                                                           
            DCVALS(I + NOBS + J) = CRUDELON-LONUNIT                     
         END IF                                                         
*                                                                       
         PRESSURE = IVALUE(BULL(POINT + J*12 + 2:POINT + J*12 + 3))     
         IF (PRESSURE .NE. MISSING) PRESSURE = PRESSURE*1000            
         DCVALS(I + NOBS*2 + J) = PRESSURE ! PRESSURE IN PASCALS!       
*                                                                       
         CALM = .FALSE.                                                 
         DDD = IVALUE(BULL(POINT + J*12 + 6:POINT + J*12 +  7))         
         FFF = IVALUE(BULL(POINT + J*12 + 8:POINT + J*12 + 10))         
         IF (DDD .NE. MISSING) DDD = DDD*10                             
         IF (FFF .GE. 500) THEN                                      !A 
            FFF = FFF-500                                               
            IF (DDD .NE. MISSING) DDD = DDD+5                           
         END IF                                                         
         IF (FFF .EQ. 0) CALM = .TRUE.                                  
         DCVALS(I + NOBS*5 + J) = FFF ! WIND SPEED M/S OR KNOTS         
         IF (KNOTS .AND. FFF.NE.MISSING)                                
     &      DCVALS(I + NOBS*5 + J) = DCVALS(I + NOBS*5 + J)*0.5144      
         IF (CALM) DCVALS(I + NOBS*5 + J) = 0.0                         
         FFF = DCVALS(I + NOBS*5 + J)                                   
         IF (FFF. GT. 200) THEN                                         
            FFF                    = MISSING                            
            DCVALS(I + NOBS*5 + J) = MISSING                            
         END IF                                                         
*                                                                       
         IF (FFF .EQ. MISSING) DDD = MISSING                            
         IF (DDD .EQ.       0) DDD = 360                                
         IF (DDD .GT.     360) DDD = MISSING                            
         IF (CALM)             DDD = 0                                  
         DCVALS(I + NOBS*4 + J) = DDD ! WIND DIRN DEGREES TRUE          
         IF (DDD .EQ. MISSING) THEN                                     
            FFF                    = MISSING                            
            DCVALS(I + NOBS*5 + J) = MISSING                            
         END IF                                                         
      END DO                                                            
*                                                                       
      POINT = POINT + NN*12                                             
      IF (NNTOT .LT. NOBS) THEN                                         
         I     = I + NN                                                 
         GOTO 10                                                        
      END IF                                                            
*                                                                       
      RETURN                                                            
      END                                                               
