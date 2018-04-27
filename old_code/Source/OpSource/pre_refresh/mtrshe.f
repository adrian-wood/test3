      SUBROUTINE MTRSHE(REPLEN,REPORT,POINT,GRPLEN,LREPFL,BADGRP,RWYUSE,
     &                  RWYDIR,RWYPRL)                                  
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : MTRSHE                                               
!                                                                      
! PURPOSE       : THIS IS A SPECIAL SUBROUTINE TO LOCATE THE           
!                 SUBGROUPS WHICH MAKE UP A WINDSHEAR GROUP            
!                 AND RETURN THE TOTAL LENGTH TO ALLOW THE             
!                 WINDSHEAR GROUP TO BE TREATED AS A WHOLE BY THE      
!                 MAIN EXPANSION ROUTINE, LIKE THE REST OF THE GROUPS. 
!                 ALL OTHER CODE GROUPS ARE SEPARATE, WINDSHEAR IS     
!                 UNIQUE.                                              
!                                                                      
! CALLED BY     : MTREXP                                               
!                                                                      
! PARAMETERS    : (1)  REPLEN  REPORT LENGTH (INP)                     
!                 (2)  REPORT  REPORT (INP)                            
!                 (3)  POINT   START POSITION OF GROUP (INP)           
!                 (4)  GRPLEN  GROUP LENGTH (RET)                      
!                 (5)  LREPFL  END OF REPORT FLAG (RET)                
!                 (6)  BADGRP  BAD WINDSHEAR GROUP FLAG (RET)          
!                                                                      
!Y2K  16.06.1997  MTREXP is Year 2000 compliant.                        
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:36$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrshe.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:36    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:58  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/03/10  10:17:32  10:17:32  usmdb (Generic MDB accou
! 20/03/2000 Addition of error checking, to check that elements
!            stored, have expected numerical values.
! 
! Revision 1.1  97/07/31  08:49:09  08:49:09  uspm (Pat McCormack)
! Initial revision
!
! NOV/DEC 95       UPDATE TO '95 TEAM STANDARDS AND INCLUDE CHANGES    
!                  TO ACCOMODATE METAR CODE CHANGES VALID FROM JAN 96. 
!                  ADD CHECK TO SET LREPFL IF NECESSARY. NOT DONE      
!                  BEFORE.                                         !A  
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
                                                                        
      INTEGER GRPLEN              ! Exact length of wind shear group.   
      INTEGER OFFSET              ! Position after last character in    
                                  ! wind shear group.                   
      INTEGER POINT               ! Current 'point' within report.      
      INTEGER REPLEN              ! Overall Metar report length.        
      INTEGER RWYUSE              ! Expansion value to define runway(s) 
                                  ! affected.                           
      INTEGER RWYDIR              ! Expansion value to define runway    
                                  ! direction.                          
      INTEGER RWYPRL              ! Expansion value to define parallel  
                                  ! runways.                            
                                                                        
      CHARACTER REPORT*(*)        ! Complete Metar report               
      CHARACTER HEAD*132          ! Revision information
                                                                        
      LOGICAL BADGRP              ! Flag to bypass sections of code if  
                                  ! wind shear group contains syntax    
                                  ! errors.                             
      LOGICAL LREPFL              ! Set if POINT is beyond the report   
                                  ! length.                             
      LOGICAL PRL                 !                                     
                                                                        
! Initialise variables                                                  
                                                                        
      BADGRP=.FALSE.                                                    
      LREPFL=.FALSE.                                                    
      GRPLEN=0                                                          
      OFFSET=0                                                          
      PRL=.FALSE.                                                       
      RWYUSE=-9999999                                                   
      RWYDIR=-9999999                                                   
      RWYPRL=-9999999                                                   
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/mtrshe.F,v $
     &'//'$ $Date: 30/01/2006 20:23:36$ $Revision: 1$'
                                                                                                                                                
! If group is a take-off runway report, then increment OFFSET value,    
! set runway use value, expand runway direction data (if available) and 
! set flag to check for parallel runways.                               
                                                                        
      IF (REPORT(POINT+3:POINT+10) .EQ. 'TKOF RWY') THEN                
        RWYUSE=1                                                        
        IF ((POINT+12) .LE. REPLEN .AND.                                
     &      REPORT(POINT+11:POINT+11) .GE. '0' .AND.                    
     &      REPORT(POINT+11:POINT+11) .LE. '9' .AND.                    
     &      REPORT(POINT+12:POINT+12) .GE. '0' .AND.                    
     &      REPORT(POINT+12:POINT+12) .LE. '9') THEN                    
          READ(REPORT(POINT+11:POINT+12),'(I2)') RWYDIR                 
          OFFSET=13                                                     
          PRL=.TRUE.                                                    
        ELSE                                                            
          OFFSET=10                                                     
        ENDIF                                                           
                                                                        
! If group is a landing runway report, then increment OFFSET value,     
! set runway use value, expand runway direction data (if available) and 
! set flag to check for parallel runways.                               
                                                                        
      ELSEIF (REPORT(POINT+3:POINT+9) .EQ. 'LDG RWY') THEN              
        RWYUSE=2                                                        
        IF ((POINT+11) .LE. REPLEN .AND.                                
     &      REPORT(POINT+10:POINT+10) .GE. '0' .AND.                    
     &      REPORT(POINT+10:POINT+10) .LE. '9' .AND.                    
     &      REPORT(POINT+11:POINT+11) .GE. '0' .AND.                    
     &      REPORT(POINT+11:POINT+11) .LE. '9') THEN                    
          READ(REPORT(POINT+10:POINT+11),'(I2)') RWYDIR                 
          OFFSET=12                                                     
          PRL=.TRUE.                                                    
        ELSE                                                            
          OFFSET=9                                                      
        ENDIF                                                           
                                                                        
! If group is an indesignated runway use group, then increment the      
! OFFSET value, expand the runway direction data (if available) and     
! set the flag to check for parallel runways.                           
                                                                        
      ELSEIF (REPORT(POINT+3:POINT+5) .EQ. 'RWY') THEN                  
        IF ((POINT+7) .LE. REPLEN .AND.                                 
     &      REPORT(POINT+6:POINT+6) .GE. '0' .AND.                      
     &      REPORT(POINT+6:POINT+6) .LE. '9' .AND.                      
     &      REPORT(POINT+7:POINT+7) .GE. '0' .AND.                      
     &      REPORT(POINT+7:POINT+7) .LE. '9') THEN                      
          READ(REPORT(POINT+6:POINT+7),'(I2)') RWYDIR        !1.2
          OFFSET=8                                                      
          PRL=.TRUE.                                                    
        ELSE                                                            
          OFFSET=5                                                      
        ENDIF                                                           
                                                                        
! If the group is an 'all runways' report, then set the OFFSET value    
! and the parallel runways value.                                       
                                                                        
      ELSEIF (REPORT(POINT+3:POINT+9) .EQ. 'ALL RWY') THEN              
        OFFSET=10                                                       
        RWYPRL=5                                                        
                                                                        
! If the group content did not satisfy any of the above conditions,     
! it is deemed to be incorrectly formatted and no expansion is done.    
! Set the offset value to skip the 'WS' in the report, preventing an    
! infinite loop.                                                        
                                                                        
      ELSE                                                              
        BADGRP=.TRUE.                                                   
        OFFSET=2                                                        
      ENDIF                                                             
                                                                        
! Set the OFFSET value to the correct position within the report        
! and the overall group length. Set POINT to the end of the group.      
                                                                        
      OFFSET=POINT+OFFSET                                               
      POINT=POINT+OFFSET                                                
      GRPLEN=OFFSET-1                                                   
                                                                        
! Check for parallel runways (not all aerodromes will have one runway)  
! provided the flag was set.                                            
! This section will not be processed should the wind shear report be    
! corrupt because the flag PRL will never be true.                      
                                                                        
      IF (PRL) THEN                                                     
        IF (REPORT(OFFSET:OFFSET+1) .EQ. 'RR') THEN                     
          OFFSET=OFFSET+3                                               
          RWYPRL=4                                                      
        ELSEIF (REPORT(OFFSET:OFFSET+1) .EQ. 'LL') THEN                 
          OFFSET=OFFSET+3                                               
          RWYPRL=0                                                      
        ELSEIF (REPORT(OFFSET:OFFSET) .EQ. 'R') THEN                    
          OFFSET=OFFSET+2                                               
          RWYPRL=3                                                      
        ELSEIF (REPORT(OFFSET:OFFSET) .EQ. 'L') THEN                    
          OFFSET=OFFSET+2                                               
          RWYPRL=1                                                      
        ELSEIF (REPORT(OFFSET:OFFSET) .EQ. 'C') THEN                    
          OFFSET=OFFSET+2                                               
          RWYPRL=2                                                      
        ELSE                                                            
          OFFSET=OFFSET+1                                               
        ENDIF                                                           
      ENDIF                                                                                                                                     
                                                                        
      RETURN                                                            
      END                                                               
