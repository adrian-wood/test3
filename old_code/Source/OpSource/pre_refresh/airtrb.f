      SUBROUTINE AIRTRB(REPORT,POINT,OPT_TRB)                       !2.0

      IMPLICIT NONE                                                             

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : AIRTRB                                                         
!                                                                              
! PURPOSE       : TO DECODE THE TURBULENCE GROUP IN AN AIREP REPORT              
!                                                                              
! DESCRIPTION   : THE SEARCH USES 'KEYWORDS' TO IDENTIFY THE VARIOUS             
!                 METHODS OF REPORTING TURBULENCE. ONCE A GROUP HAS              
!                 BEEN IDENTIFIED AIRLOC IS CALLED TO MOVE THE POINTER           
!                 ONTO THE NEXT GROUP AS WE DO NOT ALWAYS KNOW THE               
!                 LENGTH OF THE GROUP DECODED AND THERE ARE MANY                 
!                 VARIATIONS IN THE LENGTHS                                      
!                                                                              
! CALLED BY     : AIROPT                                            !2.0                                                        
!                                                                              
! CALLS TO      : AIRLOC                                                         
!                                                                              
! PARAMETERS    : (1) REPORT - Passed to routine. Airep report                   
!                 (2) POINT  - Passed to routine. Positional pointer             
!                 (3) OPT_TURB - Returned from routine. Decode Turb grp          
!                 (4) LREPFL - Passed/Returned from routine.Detects end          
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:49$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airtrb.F,v $
!                                                                              
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:49    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:26  usmdb
! Removed unused dummy argument REPLEN. Added copyright and
! modified header - S.Cox
!
! Revision 1.2  97/07/31  09:08:50  09:08:50  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 10:37:44  uspm
! Initial revision
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
                                                                                
!declare integers                                                               
      INTEGER TURB              !Indicates which turb group lgt/mod/sev         
      INTEGER POINT             !Position within report pointer                 
                                                                                
!declare characters                                                             
      CHARACTER*(*) REPORT      !character form of report                       
      CHARACTER*132 HEAD        !revision information
                                                                                
!declare real                                                                   
      REAL OPT_TRB              !Decoded Turb. Value                            
                                                                                
!declare logical                                                                
      LOGICAL DEBUG                                                             
      LOGICAL LREPFL                                                            

!initialize variables                                                           
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airtrb.F,v $
     &'//'$ $Date: 30/01/2006 20:20:49$ $Revision: 1$'
      
      LREPFL=.FALSE.                                                            
      DEBUG=.FALSE.                                                             
      IF (DEBUG) THEN                                                           
        WRITE(6,*)'AIRTRB REPORT',REPORT(POINT:POINT+3)                         
      ENDIF                                                                     
                                                                                
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
!If the report starts 'TURB' then look st the next word for the start           
!of 'L'gt, 'M'od or 'S'ev. The pointer is moved to the start of the             
!group following TURB. AIRLOC is called to move the pointer to the              
!next group after this. As we do not know what size it will be we               
!cannot specify the position the pointer should move to.                        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
      IF(REPORT(POINT:POINT+3) .EQ. 'TURB') THEN                                
        IF(REPORT(POINT+5:POINT+5) .EQ. 'L') THEN                               
          TURB=1                                                                
          POINT=POINT+5                                                         
        ELSEIF(REPORT(POINT+5:POINT+5) .EQ. 'M') THEN                           
          TURB=2                                                                
          POINT=POINT+5                                                         
        ELSEIF(REPORT(POINT+5:POINT+5) .EQ. 'S') THEN                           
          TURB=3                                                                
          POINT=POINT+5                                                         
        ENDIF                                                                   
                                                                                
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
!This search looks at groups that start 'TB'. This is followed by               
!'CODE' and then followed by a number . The following list shows what           
!the TURB codes stand for;                                                      
! 0 - smooth                           !coded as *nil*                          
! 1 - occasional light turbulence      !coded as *light*                        
! 2 - light turbulence                 !coded as *light*                        
! 3 - light to moderate turbulence     !coded as *light*                        
! 4 - moderate turbulence              !coded as *moderate*                     
! 5 - moderate to severe turbulence    !coded as *moderate*                     
! 6 - severe turbulence                !coded as *severe*                       
! 7 - extreme turbulence               !coded as *severe*                       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
      ELSEIF (REPORT(POINT:POINT+1) .EQ. 'TB') THEN                             
        IF(REPORT(POINT+3:POINT+6) .EQ. 'CODE') THEN                            
          IF((REPORT(POINT+8:POINT+8) .EQ. '1') .OR.                            
     &    (REPORT(POINT+8:POINT+10) .EQ. 'ONE')) THEN                           
            TURB=1                                                              
            POINT=POINT+9                                                       
          ELSEIF((REPORT(POINT+8:POINT+8) .EQ. '2') .OR.                        
     &    (REPORT(POINT+8:POINT+10) .EQ. 'TWO')) THEN                           
            TURB=1                                                              
            POINT=POINT+9                                                       
          ELSEIF((REPORT(POINT+8:POINT+8) .EQ. '3') .OR.                        
     &    (REPORT(POINT+8:POINT+12) .EQ. 'THREE')) THEN                         
            TURB=1                                                              
            POINT=POINT+9                                                       
          ELSEIF((REPORT(POINT+8:POINT+8) .EQ. '4') .OR.                        
     &    (REPORT(POINT+8:POINT+11) .EQ. 'FOUR')) THEN                          
            TURB=2                                                              
            POINT=POINT+9                                                       
          ELSEIF((REPORT(POINT+8:POINT+8) .EQ. '5') .OR.                        
     &    (REPORT(POINT+8:POINT+11) .EQ. 'FIVE')) THEN                          
            TURB=2                                                              
            POINT=POINT+9                                                       
          ELSEIF((REPORT(POINT+8:POINT+8) .EQ. '6') .OR.                        
     &    (REPORT(POINT+8:POINT+10) .EQ. 'SIX')) THEN                           
            TURB=3                                                              
            POINT=POINT+9                                                       
          ELSEIF((REPORT(POINT+8:POINT+8) .EQ. '7') .OR.                        
     &    (REPORT(POINT+8:POINT+12) .EQ. 'SEVEN')) THEN                         
            TURB=3                                                              
            POINT=POINT+9                                                       
          ELSEIF((REPORT(POINT+8:POINT+8) .EQ. '0') .OR.                        
     &    (REPORT(POINT+8:POINT+11) .EQ. 'ZERO')) THEN                          
            TURB=0                                                              
            POINT=POINT+9                                                       
          ELSE                                                                  
            TURB=-1                                                             
          ENDIF                                                                 
                                                                                
        ELSEIF ((REPORT(POINT+2:POINT+2) .EQ. '0') .OR.                         
     &         (REPORT(POINT+3:POINT+3) .EQ. '0')) THEN                         
          TURB=0                                                                
          POINT=POINT+4                                                         
        ELSEIF ((REPORT(POINT+2:POINT+2) .EQ. '1') .OR.                         
     &         (REPORT(POINT+3:POINT+3) .EQ. '1')) THEN                         
          TURB=1                                                                
          POINT=POINT+4                                                         
        ELSEIF ((REPORT(POINT+2:POINT+2) .EQ. '2') .OR.                         
     &         (REPORT(POINT+3:POINT+3) .EQ. '2')) THEN                         
          TURB=1                                                                
          POINT=POINT+4                                                         
        ELSEIF((REPORT(POINT+2:POINT+2) .EQ. '3') .OR.                          
     &         (REPORT(POINT+3:POINT+3) .EQ. '3')) THEN                         
          TURB=1                                                                
          POINT=POINT+4                                                         
        ELSEIF ((REPORT(POINT+2:POINT+2) .EQ. '4') .OR.                         
     &         (REPORT(POINT+3:POINT+3) .EQ. '4')) THEN                         
          TURB=2                                                                
          POINT=POINT+4                                                         
        ELSEIF ((REPORT(POINT+2:POINT+2) .EQ. '5') .OR.                         
     &         (REPORT(POINT+3:POINT+3) .EQ. '5')) THEN                         
          TURB=2                                                                
          POINT=POINT+4                                                         
        ELSEIF ((REPORT(POINT+2:POINT+2) .EQ. '6') .OR.                         
     &         (REPORT(POINT+3:POINT+3) .EQ. '6')) THEN                         
          TURB=3                                                                
          POINT=POINT+4                                                         
        ELSEIF ((REPORT(POINT+2:POINT+2) .EQ. '7') .OR.                         
     &         (REPORT(POINT+3:POINT+3) .EQ. '7')) THEN                         
          TURB=3                                                                
          POINT=POINT+4                                                         
        ELSE                                                                    
          TURB=-1                                                               
        ENDIF                                                                   
                                                                                
                                                                                
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
!This code is similar to above. Except that instead of 'TB' the                 
!reported turbulence starts 'CODE'                                              
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
      ELSEIF(REPORT(POINT:POINT+3) .EQ. 'CODE') THEN                            
        IF (DEBUG) THEN                                                         
          WRITE(6,*)'CODE',REPORT(POINT:POINT+3),REPORT(POINT+5:POINT+5)        
        ENDIF                                                                   
        IF((REPORT(POINT+5:POINT+5) .EQ. '0') .OR.                              
     &  (REPORT(POINT+5:POINT+8) .EQ. 'ZERO')) THEN                             
          TURB=0                                                                
          POINT=POINT+6                                                         
        ELSEIF((REPORT(POINT+5:POINT+5) .EQ. '1') .OR.                          
     &  (REPORT(POINT+5:POINT+7) .EQ. 'ONE')) THEN                              
        IF (DEBUG) THEN                                                         
         WRITE(6,*)'DECODED TURB'                                               
        ENDIF                                                                   
          TURB=1                                                                
          POINT=POINT+6                                                         
        ELSEIF((REPORT(POINT+5:POINT+5) .EQ. '2') .OR.                          
     &  (REPORT(POINT+5:POINT+7) .EQ. 'TWO')) THEN                              
          TURB=1                                                                
          POINT=POINT+6                                                         
        ELSEIF((REPORT(POINT+5:POINT+5) .EQ. '3') .OR.                          
     &  (REPORT(POINT+5:POINT+9) .EQ. 'THREE')) THEN                            
          TURB=1                                                                
          POINT=POINT+6                                                         
        ELSEIF((REPORT(POINT+5:POINT+5) .EQ. '4') .OR.                          
     &  (REPORT(POINT+5:POINT+8) .EQ. 'FOUR')) THEN                             
          TURB=2                                                                
          POINT=POINT+6                                                         
        ELSEIF((REPORT(POINT+5:POINT+5) .EQ. '5') .OR.                          
     &  (REPORT(POINT+5:POINT+8) .EQ. 'FIVE')) THEN                             
          TURB=2                                                                
          POINT=POINT+6                                                         
        ELSEIF((REPORT(POINT+5:POINT+5) .EQ. '6') .OR.                          
     &  (REPORT(POINT+5:POINT+7) .EQ. 'SIX')) THEN                              
          TURB=3                                                                
          POINT=POINT+6                                                         
        ELSEIF((REPORT(POINT+5:POINT+5) .EQ. '7') .OR.                          
     &  (REPORT(POINT+5:POINT+9) .EQ. 'SEVEN')) THEN                            
          TURB=3                                                                
          POINT=POINT+6                                                         
        ELSE                                                                    
          TURB=-1                                                               
        ENDIF                                                                   
      ENDIF                                                                     
                                                                                
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
!The following code sets the value of OPT_TRB to the value in the BUFR          
!code table for turbulence, where missing = 15                                  
!                                 nil     =  8                                  
!                                 light   =  9                                  
!                                 moderate= 10                                  
!                                 severe  = 11                                  
!The flag TURB is set to -1 when this routine has been called by                
!AIROPT but no turbulence group could be decode.                                
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
!     WRITE(6,*)'VALUE OF TURB= ',TURB                                          
      IF (TURB .EQ. -1) THEN                                                    
        OPT_TRB=-9999999.                                                       
      ELSEIF(TURB .EQ. 0) THEN                                                  
        OPT_TRB=8.0                                                             
      ELSEIF(TURB .EQ. 1) THEN                                                  
        OPT_TRB=9.0                                                             
      ELSEIF(TURB .EQ. 2) THEN                                                  
        OPT_TRB=10.0                                                            
      ELSEIF(TURB .EQ. 3) THEN                                                  
        OPT_TRB=11.0                                                            
      ENDIF                                                                     
                                                                                
!Returns to AIROPT routine                                                      
      RETURN                                                                    
      END                                                                       
