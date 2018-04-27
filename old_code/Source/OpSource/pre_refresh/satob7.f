      SUBROUTINE SATOB7(POINT,DCVALS,NOBS,BULL)                         

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SATOB7                                               
!                                                                      
! PURPOSE       : TO DECODE A SATOB SECTION 7                          
!                                                                      
! CALLED BY     : SATOB1                                               
!                                                                      
! CALLS         : SATBBN                                               
!                                                                      
! PARAMETERS    : (1) POINT    POINTER TO WHERE TO START IN BULLETIN   
!                 (2) DCVALS   REAL ARRAY TO HOLD DECODED VALUES       
!                 (3) NOBS     NUMBER OF 'OBSERVATIONS' IN SECTION     
!                 (4) BULL     REPORT DATA                             
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:09$
! $Source: /home/us0400/mdb/op/lib/source/RCS/satob7.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:09    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:51  usmdb
! Separated variable declaration and initialisation. Added
! copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  11:33:49  11:33:49  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:22:58  uspm
! Initial revision
!
! 05/06/96  REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY A NEW         
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
      INTEGER POINT, NN, NNTOT, PENTTOT, LASTFEW                        
      INTEGER CRUDELAT, CRUDELON, LATUNIT, LONUNIT                      
      INTEGER PBASE, PTOP, RELHUM                                       
      INTEGER I, J, K, L, M                                             
      INTEGER IVALUE, NOBS                                              
      INTEGER MISSING                                               !2.0
*                                                                       
      REAL DCVALS(*)                                                    
*                                                                       
      CHARACTER*(*)  BULL                                               
      CHARACTER*15 LLHUMS                                               
      CHARACTER*132 HEAD
*                                                                       
      LOGICAL NORTH, EAST                                               
*                                                                       
      DATA MISSING /-9999999/                                       !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/satob7.F,v $
     &'//'$ $Date: 30/01/2006 20:24:09$ $Revision: 1$'
      
      I     = NOBS*6 + 1  ! NOBS*6 FROM SECTION ONE                     
      NNTOT = 0                                                         
*                                                                       
   10 CONTINUE                                                          
      IF (BULL(POINT+2:POINT+4) .EQ. '///') THEN                        
         PBASE = IVALUE(BULL(POINT:POINT+1))                            
         POINT = POINT+6                                                
         IF (PBASE .EQ. MISSING) THEN                                   
            PTOP  = MISSING                                             
         ELSE                                                           
            PBASE = PBASE*1000                                          
            PTOP  = 0  ! ASSUME TROPOPAUSE PRESSURE IS ZERO             
         END IF                                                         
      END IF                                                            
*                                                                       
      CALL SATBBN(POINT,CRUDELAT,CRUDELON,NORTH,EAST,NN,BULL)           
      POINT   = POINT+6                                                 
      NNTOT   = NNTOT+NN                                                
      PENTTOT = NN/5                                                    
      LASTFEW = MOD(NN,5)                                               
      L       = 0                                                       
*                                                                       
      DO J = 0,PENTTOT-1                                                
         LLHUMS( 1: 2) = BULL(POINT  :POINT+ 1)                         
         LLHUMS( 4: 5) = BULL(POINT+2:POINT+ 3)                         
         LLHUMS( 7: 7) = BULL(POINT+4:POINT+ 4)                         
         LLHUMS( 8: 8) = BULL(POINT+6:POINT+ 6)                         
         LLHUMS(10:11) = BULL(POINT+7:POINT+ 8)                         
         LLHUMS(13:14) = BULL(POINT+9:POINT+10)                         
         DO K = 1,5                                                     
            LLHUMS(K*3:K*3) = BULL(POINT+11+K:POINT+11+K)               
         END DO                                                         
         DO K = 0,14,3                                                  
            LATUNIT = IVALUE(LLHUMS(K+1:K+1))                           
            IF (CRUDELAT.EQ.MISSING .OR. LATUNIT.EQ.MISSING) THEN       
               DCVALS(I + J*5 + K/3) = MISSING                          
            ELSE IF (NORTH) THEN                                        
               DCVALS(I + J*5 + K/3) = CRUDELAT+LATUNIT                 
            ELSE                                                        
               DCVALS(I + J*5 + K/3) = CRUDELAT-LATUNIT                 
            END IF                                                      
            LONUNIT = IVALUE(LLHUMS(K+2:K+2))                           
            IF (CRUDELON.EQ.MISSING .OR. LONUNIT.EQ.MISSING) THEN       
               DCVALS(I + NOBS + J*5 + K/3) = MISSING                   
            ELSE IF (EAST) THEN                                         
               DCVALS(I + NOBS + J*5 + K/3) = CRUDELON+LONUNIT          
            ELSE                                                        
               DCVALS(I + NOBS + J*5 + K/3) = CRUDELON-LONUNIT          
            END IF                                                      
            DCVALS(I + NOBS*2 + J*5 + K/3) = PBASE                      
            DCVALS(I + NOBS*3 + J*5 + K/3) = PTOP                       
            RELHUM = IVALUE(LLHUMS(K+3:K+3))                            
            IF (RELHUM .NE. MISSING) RELHUM = RELHUM*10                 
            DCVALS(I + NOBS*4 + J*5 + K/3) = RELHUM                     
            L = J*5 + K/3 + 1                                           
         END DO                                                         
         POINT = POINT+18                                               
      END DO                                                            
*                                                                       
      LLHUMS( 1: 2) = BULL(POINT  :POINT+ 1)                            
      LLHUMS( 4: 5) = BULL(POINT+2:POINT+ 3)                            
      LLHUMS( 7: 7) = BULL(POINT+4:POINT+ 4)                            
      LLHUMS( 8: 8) = BULL(POINT+6:POINT+ 6)                            
      LLHUMS(10:11) = BULL(POINT+7:POINT+ 8)                            
      LLHUMS(13:14) = BULL(POINT+9:POINT+10)                            
      DO M = 1,5                                                        
         LLHUMS(M*3:M*3) = BULL(POINT+11+M:POINT+11+M)                  
      END DO                                                            
      DO M = 0, LASTFEW*3 - 1, 3                                        
         LATUNIT = IVALUE(LLHUMS(M+1:M+1))                              
         IF (CRUDELAT.EQ.MISSING .OR. LATUNIT.EQ.MISSING) THEN          
            DCVALS(I + L+ M/3) = MISSING                                
         ELSE IF (NORTH) THEN                                           
            DCVALS(I + L+ M/3) = CRUDELAT+LATUNIT                       
         ELSE                                                           
            DCVALS(I + L+ M/3) = CRUDELAT-LATUNIT                       
         END IF                                                         
         LONUNIT = IVALUE(LLHUMS(M+2:M+2))                              
         IF (CRUDELON.EQ.MISSING .OR. LONUNIT.EQ.MISSING) THEN          
            DCVALS(I + NOBS + L+ M/3) = MISSING                         
         ELSE IF (EAST) THEN                                            
            DCVALS(I + NOBS + L+ M/3) = CRUDELON+LONUNIT                
         ELSE                                                           
            DCVALS(I + NOBS + L+ M/3) = CRUDELON-LONUNIT                
         END IF                                                         
         DCVALS(I + NOBS*2 + L+ M/3) = PBASE                            
         DCVALS(I + NOBS*3 + L+ M/3) = PTOP                             
         RELHUM = IVALUE(LLHUMS(M+3:M+3))                               
         IF (RELHUM .NE. MISSING) RELHUM = RELHUM*10                    
         DCVALS(I + NOBS*4 + L+ M/3) = RELHUM                           
      END DO                                                            
      IF (LASTFEW .GT. 0) POINT = POINT+18                              
*                                                                       
      IF (NNTOT .LT. NOBS) THEN                                         
         IF (NN .GT. 0) I = I + NN                                      
         GOTO 10                                                        
      END IF                                                            
*                                                                       
      RETURN                                                            
      END                                                               
