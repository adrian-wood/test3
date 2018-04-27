      SUBROUTINE SATOB2(POINT,KNOTS,DCVALS,NOBS,BULL,SLASHES)        !B 

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SATOB2                                               
!                                                                      
! PURPOSE       : TO DECODE A SATOB SECTION 2                          
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
!                 (6) SLASHES  LOGICAL FOR '/////' PADDING           !B 
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:07$
! $Source: /home/us0400/mdb/op/lib/source/RCS/satob2.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:07    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:49  usmdb
! Separated variable declaration and initialisation. Added
! copyright and modified header - S.Cox
!
! Revision 1.4  99/09/09  09:58:11  09:58:11  usmdb (Generic MetDB accou
! 20 Sept 99  C Long
! 1.4 Change C/K conversion from 273 to 273.1 (temperatures in tenths)
! 
! Revision 1.3  97/08/28  10:19:22  10:19:22  usjl (Jon Lewthwaite)
! PASS IN LOGICAL AB PASS IN LOGICAL ABOUT PRESENCE OF '/////'S AT END O
! WHEN AN ODD NUMBER OF 'OBS' TO ENABLE A GOOD STEP THROUGH
!
! Revision 1.2  1997/07/31 11:33:10  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 13:19:45  uspm
! Initial revision
!
! 10/02/97  CORRECTLY DECODE WIND DIRECTION AND SPEED WHEN SPEED       
!           REPORTED AS 500 UNITS OR MORE                            !A 
!                                                                      
! 05/06/96  REMOVE COMMON BLOCK 'TBULL', REPLACED BY A NEW PARAMETER   
!           'BULL'.                                                    
!                                                                      
! JAN 94 : FIRST WRITTEN                                              
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
                                                                        
      INTEGER POINT, NN, NNTOT, PAIRTOT                                 
      INTEGER CRUDELAT, CRUDELON, LATUNIT, LONUNIT                      
      INTEGER PRESSURE, TEMPRCH, TEMPSIGN, DDD, FFF                     
      INTEGER I, J, K, L                                                
      INTEGER IVALUE, NOBS                                              
      INTEGER MISSING                                               !2.0
*                                                                       
      REAL DCVALS(*)                                                    
*                                                                       
      CHARACTER*(*) BULL                                                
      CHARACTER*132 HEAD
*                                                                       
      LOGICAL KNOTS, CALM, NORTH, EAST, ODDTOT, SLASHES              !B 
*                                                                       
      DATA MISSING /-9999999/                                       !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/satob2.F,v $
     &'//'$ $Date: 30/01/2006 20:24:07$ $Revision: 1$'

      I     = NOBS*9 + 1  ! NOBS*9 FROM SECTION ONE                     
      NNTOT = 0                                                         
*                                                                       
   10 CONTINUE                                                          
      CALL SATBBN(POINT,CRUDELAT,CRUDELON,NORTH,EAST,NN,BULL)           
      IF (MOD(NN,2) .EQ. 0) THEN                                        
         PAIRTOT = NN/2                                                 
         ODDTOT  = .FALSE.                                              
      ELSE                                                              
         PAIRTOT = (NN+1) / 2                                           
         ODDTOT  = .TRUE.                                               
      END IF                                                            
      POINT = POINT+6                                                   
      NNTOT = NNTOT+NN                                                  
      J     = -2                                                        
*                                                                       
      DO K = 1,PAIRTOT                                                  
         J = J+2                                                        
         DO L = 0,1                                                     
            IF (K.EQ.PAIRTOT .AND. L.EQ.1 .AND. ODDTOT) GOTO 20         
* NO POINT IN DECODING AND STORING SOLIDI (IF PRESENT)                  
            IF (L .EQ. 0) THEN                                          
              LATUNIT = IVALUE(BULL(POINT + J*15    :POINT + J*15    )) 
            ELSE                                                        
              LATUNIT = IVALUE(BULL(POINT + J*15 + 2:POINT + J*15 + 2)) 
            END IF                                                      
            IF (CRUDELAT.EQ.MISSING .OR. LATUNIT.EQ.MISSING) THEN       
               DCVALS(I + J + L) = MISSING                              
            ELSE IF (NORTH) THEN                                        
               DCVALS(I + J + L) = CRUDELAT+LATUNIT                     
            ELSE                                                        
               DCVALS(I + J + L) = CRUDELAT-LATUNIT                     
            END IF                                                      
*                                                                       
            IF (L .EQ. 0) THEN                                          
              LONUNIT = IVALUE(BULL(POINT + J*15 + 1:POINT + J*15 + 1)) 
            ELSE                                                        
              LONUNIT = IVALUE(BULL(POINT + J*15 + 3:POINT + J*15 + 3)) 
            END IF                                                      
            IF (CRUDELON.EQ.MISSING .OR. LONUNIT.EQ.MISSING) THEN       
               DCVALS(I + NOBS + J + L) = MISSING                       
            ELSE IF (EAST) THEN                                         
               DCVALS(I + NOBS + J + L) = CRUDELON+LONUNIT              
            ELSE                                                        
               DCVALS(I + NOBS + J + L) = CRUDELON-LONUNIT              
            END IF                                                      
*                                                                       
            IF (L .EQ. 0) THEN                                          
           PRESSURE = IVALUE(BULL(POINT + J*15 +  6:POINT + J*15 +  7)) 
            ELSE                                                        
           PRESSURE = IVALUE(BULL(POINT + J*15 + 18:POINT + J*15 + 19)) 
            END IF                                                      
            IF (PRESSURE .NE. MISSING) PRESSURE = PRESSURE*1000         
           DCVALS(I + NOBS*2 + J + L) = PRESSURE ! PRESSURE IN PASCALS! 
*                                                                       
            IF (L .EQ. 0) THEN                                          
            TEMPRCH = IVALUE(BULL(POINT + J*15 +  8:POINT + J*15 + 10)) 
            ELSE                                                        
            TEMPRCH = IVALUE(BULL(POINT + J*15 + 20:POINT + J*15 + 22)) 
            END IF                                                      
            IF (TEMPRCH .NE. MISSING) THEN                              
               IF (L .EQ. 0) THEN                                       
              TEMPSIGN = IVALUE(BULL(POINT +J*15 +10:POINT +J*15 + 10)) 
               ELSE                                                     
              TEMPSIGN = IVALUE(BULL(POINT +J*15 +22:POINT +J*15 + 22)) 
               END IF                                                   
               IF (MOD(TEMPSIGN, 2) .EQ. 1) TEMPRCH = 0-TEMPRCH         
               TEMPRCH = TEMPRCH + 2731 ! temperature in tenths    !1.4 
            END IF                                                      
            DCVALS(I + NOBS*3 + J + L) = TEMPRCH ! SURFACE TEMPERATURE  
            IF (TEMPRCH .NE. MISSING)                                   
     &     DCVALS(I + NOBS*3 + J + L) = DCVALS(I + NOBS*3 + J + L)/10.0 
*                                                                       
            CALM = .FALSE.                                              
            IF (L .EQ. 0) THEN                                          
               DDD = IVALUE(BULL(POINT + J*15 + 12:POINT + J*15 + 13))  
               FFF = IVALUE(BULL(POINT + J*15 + 14:POINT + J*15 + 16))  
            ELSE                                                        
               DDD = IVALUE(BULL(POINT + J*15 + 24:POINT + J*15 + 25))  
               FFF = IVALUE(BULL(POINT + J*15 + 26:POINT + J*15 + 28))  
            END IF                                                      
            IF (DDD .NE. MISSING) DDD = DDD*10                          
            IF (FFF .GE. 500) THEN                                   !A 
               FFF = FFF-500                                            
               IF (DDD .NE. MISSING) DDD = DDD+5                        
            END IF                                                      
            IF (FFF .EQ. 0) CALM = .TRUE.                               
            DCVALS(I + NOBS*5 + J + L) = FFF ! WIND SPEED M/S OR KNOTS  
            IF (KNOTS .AND. FFF.NE.MISSING)                             
     &   DCVALS(I + NOBS*5 + J + L) = DCVALS(I + NOBS*5 + J + L)*0.5144 
            IF (CALM) DCVALS(I + NOBS*5 + J + L) = 0.0                  
            FFF = DCVALS(I + NOBS*5 + J + L)                            
            IF (FFF. GT. 200) THEN                                      
               FFF                        = MISSING                     
               DCVALS(I + NOBS*5 + J + L) = MISSING                     
            END IF                                                      
*                                                                       
            IF (FFF .EQ. MISSING) DDD = MISSING                         
            IF (DDD .EQ.       0) DDD = 360                             
            IF (DDD .GT.     360) DDD = MISSING                         
            IF (CALM)             DDD = 0                               
            DCVALS(I + NOBS*4 + J + L) = DDD ! WIND DIRN DEGREES TRUE   
            IF (DDD .EQ. MISSING) THEN                                  
               FFF                        = MISSING                     
               DCVALS(I + NOBS*5 + J + L) = MISSING                     
            END IF                                                      
         END DO                                                         
      END DO                                                            
*                                                                       
   20 CONTINUE                                                          
      IF (ODDTOT .AND. SLASHES) THEN                                 !B 
         POINT = POINT + PAIRTOT*6 + (NN+1)*12                       !B 
      ELSE                                                              
         POINT = POINT + PAIRTOT*6 + NN*12                           !B 
      END IF                                                            
      IF (NNTOT .LT. NOBS) THEN                                         
         I     = I + NN                                                 
         GOTO 10                                                        
      END IF                                                            
*                                                                       
      RETURN                                                            
      END                                                               
