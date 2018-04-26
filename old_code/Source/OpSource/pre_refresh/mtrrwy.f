      SUBROUTINE MTRRWY(POINT,REPORT,GRPLEN,CHARR,RVRDIR,VISTEND,   !2.0
     &                  RVRPRL,MINQUAL,MINVIS,MAXQUAL,MAXVIS)           
                                                                        
!-----------------------------------------------------------------------
!                                                                       
! PROGRAM       : MTRRWY                                                
!                                                                       
! PURPOSE       : TO DECODE THE RUNWAY VISIBILITY GROUPS.               
!                 IT ALLOWS FOR A MAXIMUM OF 2 GROUPS.                  
!                                                                       
! CALLED BY     : MTREXP                                                
!                                                                       
! PARAMETERS    : (1)  POINT   CURRENT POSITION WITHIN REPORT(IN)       
!                 (2)  REPORT  METAR REPORT                     (IN)    
!                 (3)  GRPLEN  GROUP LENGTH(IN)                         
!                 (4)  CHARR   CHARACTER DESCRIPTION OF GROUP   (IN)    
!                 (5)  RVRDIR  RUNWAY DIRECTION                 (RET)   
!                 (6)  VISTEND VISIBILITY TENDENCY              (RET)   
!                 (7)  RVRPRL  PARALLEL RUNWAY IDENTFIER        (RET)   
!                 (8)  MINQUAL MINIMUM VISIBILITY QUALIFIER     (RET)   
!                 (9)  MINVIS  MINIMUM VISIBILITY               (RET)   
!                (10)  MAXQUAL MAXIMUM VISIBILITY QUALIFIER     (RET)   
!                (11)  MAXVIS  MAXIMUM VISIBILITY               (RET)   
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:36$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrrwy.F,v $
!                                                                       
! CHANGE RECORD :                                                       
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:36    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:57  usmdb
! Removed unused dummy argument NCHAR. Added copyright
! and modified header - S.Cox
!
! Revision 1.2  2000/03/10  10:13:10  10:13:10  usmdb (Generic MDB accou
! 20/03/2000 Addition of error checking, to check that elements
!            stored, have expected numerical values.
! 
! Revision 1.1  97/09/22  13:05:38  13:05:38  uspm (Pat McCormack)
! Initial revision
!
!   NOV/DEC 95   MAJOR OVERHAUL OF WHOLE ROUTINE. UPDATED TO CURRENT    
!                STANDARDS AND INCLUSION OF IMPLICIT NONE.              
!                REMOVED TWO UNNECESSARY PARAMETERS FROM CALL AND       
!                ADDED CORRECTIONS TO ENSURE THAT RUNWAY VISIBILITY     
!                QUALIFIERS AND RUNWAY VISIBILITY TENDENCY VALUES WERE  
!                EXPANDED CORRECTLY.                                    
!                CORRECTED VALUES RETURNED FOR ABOVE (PREVIOUS VALUES   
!                RETURNED SEEMED TO HAVE NO RELATION TO ANY TABLES)     
!                STREAMLINED CODE, REMOVING DUPLICATED CODE.            
!
!   07/05/94     EXTRA CONDITIONS ADDED EVERYWHERE A CHECK ON CHARR     
!                IS USED TO ENSURE THAT THE SUBSTRING BEING EXAMINED    
!                ACTUALLY LIES WITHIN THE BOUNDS OF THE GROUP BEING     
!                CONSIDERED.                                            
!
!   31/01/94     EXTRA CONDITION ADDED TO IF STATEMENTS IN 'OLD' CODE   
!                SECTION BEFORE THE INTERNAL READ IS DONE.              
!                                                                       
!   15/10/93     CORRECT CODE VALUE FOR LIMITS ON VISIBILITY 'P'=1      
!                                                                       
!   24/06/93     ONLY SET PARALLEL RUNWAY INDICATOR TO CENTRE IF        
!                'C' LOCATED IN THE GROUP, DO NOT USE AS DEFAULT.       
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
                                                                        
      INTEGER GRPLEN           ! Length of group.                       
      INTEGER LOOP             ! General loop variable                  
      INTEGER POINT            ! Point within report and group.         
      INTEGER OFFSET           ! Ensures correct position within group  
                               ! is checked for variable visibilities.  
      INTEGER SOLIDI           ! Position within group of '/' character 
      INTEGER TIMES            ! Number of times to loop visibility     
                               ! expansion.                             
      INTEGER VALUE            ! Visibility value (MAXVIS or MINVIS)    
      INTEGER VRB              ! Position within group of 'V' character 
      INTEGER IVALUE           !1.2 Func to get integer from a string
                                                                        
      REAL MAXVIS              ! Maximum visibility, when variable.     
      REAL MINVIS              ! Minimum visibility.                    
      REAL MAXQUAL             ! Maximum visibility qualifier.          
      REAL MINQUAL             ! Minimum visibility qualifier.          
      REAL RVRDIR              ! Runway direction.                      
      REAL RVRPRL              ! Parallel runway identifier.            
      REAL VISTEND             ! Visibility tendency.                   
                                                                        
      CHARACTER REPORT*(*)     ! Complete METAR report.                 
      CHARACTER CHARR*(*)      ! Character content of group.            
      CHARACTER HEAD*132       ! Revision information
                                                                        
! Initialise variables                                                  

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/mtrrwy.F,v $
     &'//'$ $Date: 30/01/2006 20:23:36$ $Revision: 1$'
      MAXVIS=-9999999                                                   
      MAXQUAL=-9999999                                                  
      MINQUAL=-9999999                                                  
      MINVIS=-9999999                                                   
      RVRDIR=-9999999                                                   
      RVRPRL=-9999999                                                   
      SOLIDI=0                                                          
      TIMES=0                                                           
      VRB=0                                                             
      VALUE=-9999999                                                    
      VISTEND=-9999999                                                  
                                                                                                                                                
! Find position of SOLIDI, if any.                                      
                                                                        
      SOLIDI=INDEX(REPORT(POINT:GRPLEN+POINT-1),'/')                    
                                                                        
! Expand runway direction.                                              
                                                                        
      IF (CHARR(2:3) .EQ. 'NN') THEN                                    
        RVRDIR = FLOAT(IVALUE(REPORT(POINT+1:POINT+2)))        !1.2
      ENDIF                                                             
                                                                        
! RVR's in post July 1993 code format.                                  
                                                                        
      IF (CHARR(2:4) .EQ. 'NNY' .OR. CHARR(2:4) .EQ. 'NN/'              
     &    .AND. SOLIDI .GT. 0) THEN                                     
                                                                        
! Check for and expand parallel runway identifier.                      
                                                                        
        IF (CHARR(SOLIDI-2:SOLIDI-1) .EQ. 'YY') THEN                    
          IF (REPORT(POINT+3:POINT+4) .EQ. 'RR') THEN                   
            RVRPRL=4                                                    
          ELSEIF (REPORT(POINT+3:POINT+4) .EQ. 'LL') THEN               
            RVRPRL=0                                                    
          ENDIF                                                         
        ELSEIF (CHARR(SOLIDI-2:SOLIDI-1) .EQ. 'NY') THEN                
          IF (REPORT(POINT+3:POINT+3) .EQ. 'R') THEN                    
            RVRPRL=3                                                    
          ELSEIF (REPORT(POINT+3:POINT+3) .EQ. 'C') THEN                
            RVRPRL=2                                                    
          ELSEIF (REPORT(POINT+3:POINT+3) .EQ. 'L') THEN                
            RVRPRL=1                                                    
          ENDIF                                                         
        ENDIF                                                           
                                                                        
! Check whether variations in visibility are reported.                  
! If there are then the check for qualifiers, visibilities and tendency 
! needs done twice.                                                     
! Set the value of TIMES to ensure the correct number of loops.         
                                                                        
        VRB=INDEX(REPORT(POINT:POINT+GRPLEN-1),'V')                     
        IF (VRB .EQ. 0) THEN                                            
          TIMES=1                                                       
        ELSE                                                            
          TIMES=2                                                       
        ENDIF                                                           
                                                                        
! The value of OFFSET will ensure the correct position within the       
! group is checked.                                                     
                                                                        
        DO LOOP=1,TIMES                                                 
          OFFSET=SOLIDI+(LOOP-1)*(VRB-SOLIDI)                           
                                                                        
! Check for visibility qualifiers.                                      
! Set value for 'less than or equal to' if 'M' found, or value for      
! 'greater than or equal to' if 'P' found.                              
                                                                        
          IF (CHARR(OFFSET+1:OFFSET+1) .EQ. 'Y') THEN                   
            IF (REPORT(POINT+OFFSET:POINT+OFFSET) .EQ. 'M') THEN        
              IF (LOOP .EQ. 1) THEN                                     
                MINQUAL=4                                               
              ELSE                                                      
                MAXQUAL=4                                               
              ENDIF                                                     
            ELSEIF (REPORT(POINT+OFFSET:POINT+OFFSET) .EQ. 'P') THEN    
              IF (LOOP .EQ. 1) THEN                                     
                MINQUAL=2                                               
              ELSE                                                      
                MAXQUAL=2                                               
              ENDIF                                                     
            ENDIF                                                       
          ENDIF                                                         
                                                                        
! Check for runway visibility and any tendency.                         
! The position of the tendency indicator is different if a visibility   
! qualifier is included in the report.                                  
! Two seperate checks cater for the possibilities.                      
                                                                        
! Check for visibility and tendency in groups with qualifiers.          
                                                                        
          IF (MAXQUAL .GT. 0 .OR. MINQUAL .GT. 0) THEN                  
            IF (CHARR(OFFSET+2:OFFSET+5) .EQ. 'NNNN') THEN              
              VALUE = IVALUE(REPORT(POINT+OFFSET+1:POINT+OFFSET+4)) !1.2
              IF (LOOP .EQ. 1) THEN                                     
                MINVIS=VALUE                                            
              ELSE                                                      
                MAXVIS=VALUE                                            
              ENDIF                                                     
            ENDIF                                                       
            IF (REPORT(POINT+OFFSET+5:POINT+OFFSET+5) .EQ. 'D') THEN    
              VISTEND=2.                                                
            ELSEIF (REPORT(POINT+OFFSET+5:POINT+OFFSET+5) .EQ. 'U') THEN
              VISTEND=1.                                                
            ELSEIF (REPORT(POINT+OFFSET+5:POINT+OFFSET+5) .EQ. 'N') THEN
              VISTEND=0.                                                
            ENDIF                                                       
                                                                        
! Check for visibility and tendency in groups without qualifiers.       
                                                                        
          ELSE                                                          
            IF (CHARR(OFFSET+1:OFFSET+4) .EQ. 'NNNN') THEN              
              VALUE=IVALUE(REPORT(POINT+OFFSET:POINT+OFFSET+3))    !1.2
              IF (LOOP .EQ. 1) THEN                                     
                MINVIS=VALUE                                            
              ELSE                                                      
                MAXVIS=VALUE                                            
              ENDIF                                                     
            ENDIF                                                       
            IF (REPORT(POINT+OFFSET+4:POINT+OFFSET+4) .EQ. 'D') THEN    
              VISTEND=2.                                                
            ELSEIF (REPORT(POINT+OFFSET+4:POINT+OFFSET+4) .EQ. 'U') THEN
              VISTEND=1.                                                
            ELSEIF (REPORT(POINT+OFFSET+4:POINT+OFFSET+4) .EQ. 'N') THEN
              VISTEND=0.                                                
            ENDIF                                                       
          ENDIF                                                         
        ENDDO                                                           
                                                                        
! RVR's in pre July 1993 code format or RVR's with no variable          
! visibility or no runway identifier.                                   
                                                                        
      ELSE                                                              
                                                                        
! Search for parallel runway identifiers and set appropriate expansion  
! value if found.                                                       
                                                                        
        IF (SOLIDI .GT. 0) THEN                                         
          IF (CHARR(SOLIDI+3:GRPLEN) .EQ. 'YY') THEN                    
            IF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+3) .EQ. 'RR') THEN   
              RVRPRL=4.                                                 
            ELSEIF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+3) .EQ. 'LL')THEN
              RVRPRL=0.                                                 
            ENDIF                                                       
          ELSEIF (CHARR(SOLIDI+3:GRPLEN) .EQ. 'Y') THEN                 
            IF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+2) .EQ. 'R') THEN    
              RVRPRL=3.                                                 
            ELSEIF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+2) .EQ. 'C')THEN 
              RVRPRL=2.                                                 
            ELSEIF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+2) .EQ. 'L')THEN 
              RVRPRL=1.                                                 
            ENDIF                                                       
          ENDIF                                                         
        ENDIF                                                           
                                                                        
! Check for visibility qualifiers.                                      
! Set value for 'less than or equal to' if 'MM' found, or value for     
! 'greater than or equal to' if 'P' found.                              
! Expand visibility value, if available.                                
! Note that only MINVIS and MINQUAL are used.                           
                                                                        
        IF (CHARR(2:3) .EQ. 'YY') THEN                                  
          IF (REPORT(POINT+1:POINT+2) .EQ. 'MM') THEN                   
            MINQUAL=4                                                   
          ENDIF                                                         
          IF (CHARR(4:7) .EQ. 'NNNN') THEN                              
            MINVIS = FLOAT(IVALUE(REPORT(POINT+3:POINT+6)))        !1.2
          ENDIF                                                         
        ELSEIF (CHARR(2:3) .EQ. 'YN') THEN                              
          IF (REPORT(POINT+1:POINT+1) .EQ. 'P') THEN                    
            MINQUAL=2                                                   
          ENDIF                                                         
          IF (CHARR(3:6) .EQ. 'NNNN') THEN                              
            MINVIS = FLOAT(IVALUE(REPORT(POINT+2:POINT+5)))       !1.2
          ENDIF                                                         
        ELSEIF (CHARR(2:5) .EQ. 'NNNN') THEN                            
          MINVIS = FLOAT(IVALUE(REPORT(POINT+1:POINT+4)))         !1.2
        ENDIF                                                           
      ENDIF                                                             
                                                                                                                                                
      RETURN                                                            
      END
