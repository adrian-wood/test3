      SUBROUTINE SATOB4(POINT,DCVALS,NOBS,BULL)                         

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SATOB4                                               
!                                                                     
! PURPOSE       : TO DECODE A SATOB SECTION 4                          
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
! $Date: 30/01/2006 20:24:08$
! $Source: /home/us0400/mdb/op/lib/source/RCS/satob4.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:08    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:50  usmdb
! Separated variable declaration and initialisation. Removed
! unused variables, added copyright and modified header - S.Cox
!
! Revision 1.3  99/09/09  09:58:31  09:58:31  usmdb (Generic MetDB accou
! 20 Sept 99  C Long
! 1.3 Change C/K conversion from 273 to 273.1 (temperatures in tenths)
! 
! Revision 1.2  97/07/31  11:33:31  11:33:31  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 13:21:14  uspm
! Initial revision
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

      INTEGER POINT, NN, NNTOT                                          
      INTEGER CRUDELAT, CRUDELON, LATUNIT, LONUNIT                      
      INTEGER TEMPRCH, TEMPSIGN                                         
      INTEGER I, J                                                  !2.0
      INTEGER IVALUE, NOBS                                              
      INTEGER MISSING                                               !2.0
*                                                                       
      REAL DCVALS(*)                                                    
*                                                                       
      CHARACTER*(*)  BULL                                               
      CHARACTER*132  HEAD
*                                                                       
      LOGICAL NORTH, EAST                                               
*                                                                       
      DATA MISSING /-9999999/                                       !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/satob4.F,v $
     &'//'$ $Date: 30/01/2006 20:24:08$ $Revision: 1$'

      I     = NOBS*7 + 1  ! NOBS*7 FROM SECTION ONE                     
      NNTOT = 0                                                         
*                                                                       
   10 CONTINUE                                                          
      CALL SATBBN(POINT,CRUDELAT,CRUDELON,NORTH,EAST,NN,BULL)           
      POINT = POINT+6                                                   
      NNTOT = NNTOT+NN                                                  
                                                                        
      DO J = 0,NN-1                                                     
         LATUNIT = IVALUE(BULL(POINT + J*6:POINT + J*6))                
         IF (CRUDELAT.EQ.MISSING .OR. LATUNIT.EQ.MISSING) THEN          
            DCVALS(I + J) = MISSING                                     
         ELSE IF (NORTH) THEN                                           
            DCVALS(I + J) = CRUDELAT+LATUNIT                            
         ELSE                                                           
            DCVALS(I + J) = CRUDELAT-LATUNIT                            
         END IF                                                         
         LONUNIT = IVALUE(BULL(POINT + J*6 +1:POINT + J*6 +1))          
         IF (CRUDELON.EQ.MISSING .OR. LONUNIT.EQ.MISSING) THEN          
            DCVALS(I + NOBS + J) = MISSING                              
         ELSE IF (EAST) THEN                                            
            DCVALS(I + NOBS + J) = CRUDELON+LONUNIT                     
         ELSE                                                           
            DCVALS(I + NOBS + J) = CRUDELON-LONUNIT                     
         END IF                                                         
         TEMPRCH = IVALUE(BULL(POINT + J*6 +2:POINT + J*6 +4))          
         IF (TEMPRCH .NE. MISSING) THEN                                 
            TEMPSIGN = IVALUE(BULL(POINT +J*6 +4:POINT +J*6 +4))        
            IF (MOD(TEMPSIGN, 2) .EQ. 1) TEMPRCH = 0-TEMPRCH            
            TEMPRCH = TEMPRCH + 2731  ! temperature in tenths      !1.3 
         END IF                                                         
         DCVALS(I + NOBS*2 + J) = TEMPRCH  ! TEMPERATURE OF SURFACE     
         IF (TEMPRCH .NE. MISSING)                                      
     -   DCVALS(I + NOBS*2 + J) = DCVALS(I + NOBS*2 + J)/10.0 ! DEG K   
      END DO                                                            
*                                                                       
      POINT = POINT + NN*6                                              
      IF (NNTOT .LT. NOBS) THEN                                         
         I     = I + NN                                                 
         GOTO 10                                                        
      END IF                                                            
*                                                                       
      RETURN                                                            
      END                                                               
