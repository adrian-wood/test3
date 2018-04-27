      SUBROUTINE AIRLEV(REPLEN,REPORT,POINT,LEVEL,DECERR)                       
                                                                               
!-----------------------------------------------------------------------
!                                                                               
! PROGRAM       : AIRLEV                                                          
!                                                                               
! PURPOSE       : TO DECODE THE FLIGHT LEVEL IN AN AIREP REPORT                   
!                                                                               
! DESCRIPTION   : THE PROGRAM LOOKS FOR A PARTICULAT PATTERN WHICH                
!                 IS EXPECTED FOR THE FLIGHT LEVEL. WHEN A MATCH IS               
!                 MADE WITH ONE OF THE PATTERNS THE DECODE ROUTINE                
!                 EXTRACTS THE FLIGHT LEVEL AND CONVERTS IT FROM FEET             
!                 TO METERS.                                                      
!                                                                               
! CALLED BY     : AIRARP                                                          
!                                                                               
! CALL TO       : AIRLOC                                                          
!                 AIRGRP                                                          
!                                                                               
! PARAMETERS    : 1. REPLEN - LENGTH OF REPORT - I                                
!                 2. REPORT -REPORT BEING EXPANDED -I                             
!                 3. POINT - POINTER WITHIN REPORT - I/O                          
!                 4. LEVEL - AIRCRAFT FLIGH LEVEL IN METERS - O                   
!                 5. DECERR - DECODE STATUS FLAG - O                              
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:41$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airlev.F,v $
!                                                                               
! CHANGE RECORD :                                                                 
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:41    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:22  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.6  98/02/04  15:02:10  15:02:10  usmdb (Generic MetDB account)
! Changed acceptable flight level range to 001 - 600
!                                                             
! Revision 1.5  1997/09/22 13:32:14  uspm
! Change final positions in string REPORT to avoid out of bounds errors
!
! Revision 1.4  1997/07/31 09:07:19  uspm                                       
! First revision for COSMOS
!                                                                               
! Revision 1.3  1997/07/04 10:33:23  uspm                                       
! Latest version from COSMOS - Y2K check
!                                                                               
! Revision 1.2  1997/07/03 14:01:40  uspm                                       
! Latest version from COSMOS with Y2k check
!                                                                               
! Revision 1.1  1997/06/05 13:08:44  uspm                                       
! Initial revision                                                              
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

 !declare characters                                                             
      CHARACTER REPORT*(*)     !size passed on from airarp                      
      CHARACTER CHARR*20       !dummy array initialise as blank                 
      CHARACTER HEAD*132       !revision information                            
                                                                                
!declare integers                                                               
      INTEGER REPLEN           !report length                                   
      INTEGER GRPLEN           !length of group within report                   
      INTEGER POINT            !position within group in report                 
      INTEGER NCHAR            !no. of non-numerics in group                    
      INTEGER FLEVEL           !flight level raw non-converted                  
      INTEGER DECERR           !decode error flag                               
                                                                                
!declare real                                                                   
      REAL LEVEL               !expanded flight level (feet>meters)             
                                                                                
!declare logical                                                                
      LOGICAL LREPFL         !indicates end of report if = .true.               
                                                                                
      SAVE                                                                      
!declare revision information                                                   
      HEAD='                                                                    
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airlev.F,v $                 
     &'//' $ $Date: 30/01/2006 20:20:41$ $Revision: 1$'
!initialise variables                                                           
      GRPLEN=0                 !set initial grouplength =0                      
      LEVEL=-9999999.                                                           
      FLEVEL=-9999999.                                                          
      CHARR=' '                                                                 
      LREPFL=.FALSE.                                                            
                                                                                
!----------------------------------------------------------------------         
!This has three main formats; F xxx, Fxxx and xxx. Where F is an                
!indicator and xxx represents the actual height. The flight level               
!is reported by aircraft in Feet and for our purposes this is converted         
!into meters.                                                                   
!----------------------------------------------------------------------         
                                                                                
      CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)                            
      CALL AIRGRP(REPORT(POINT-GRPLEN-1:),                         !r1.5
     &GRPLEN,NCHAR,CHARR)                                                       
                                                                                
!----------------------------------------------------------------------         
!Check to see if this is the last group                                         
!----------------------------------------------------------------------         
                                                                                
      IF (LREPFL) THEN                                                          
        DECERR=1                                                                
      ENDIF                                                                     
                                                                                
!----------------------------------------------------------------------         
!Format is F_xxx. So 'F' found as a group on its own. We need to                
!look to the next group to get the flight level.It will then be                 
!a group of three numbers with no non-numeric characters.                       
!----------------------------------------------------------------------         
                                                                                
        IF ((GRPLEN .EQ. 1) .AND. (NCHAR .EQ. 1) .AND. (DECERR .EQ. 0))         
     &  THEN                                                                    
          CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)                        
          CALL AIRGRP(REPORT(POINT-GRPLEN-1:1),                                 
     &    GRPLEN,NCHAR,CHARR)                                                   
                                                                                
          IF (NCHAR .EQ. 0) THEN                                                
            READ(REPORT(POINT-GRPLEN-1:POINT-GRPLEN+1),'(I3)')FLEVEL !r1.5
            LEVEL=FLEVEL*0.3048*100                                             
          ENDIF                                                                 
                                                                                
!---------------------------------------------------------------------          
!Format of group is 'no F'.Check group starts with numbers                      
!---------------------------------------------------------------------          
                                                                                
        ELSEIF ((GRPLEN .EQ. 3) .AND. (NCHAR .EQ. 0)) THEN                      
          READ(REPORT(POINT-GRPLEN-1:POINT-2),'(I3)')FLEVEL                     
            LEVEL=FLEVEL*0.3048*100                                             
                                                                                
!---------------------------------------------------------------------          
!Format of group is Fxxx. Check first character is non-numeric                  
!---------------------------------------------------------------------          
                                                                                
        ELSEIF ((GRPLEN .EQ. 4) .AND. (CHARR(1:1) .EQ. 'Y') .AND.               
     &  (NCHAR .EQ. 1)) THEN                                                    
          READ(REPORT(POINT-GRPLEN:POINT-2),'(I3)')FLEVEL                       
            LEVEL=FLEVEL*0.3048*100                                             
        ENDIF                                                                   
                                                                                
!---------------------------------------------------------------------          
!This section checks the flight level is within limits                          
!---------------------------------------------------------------------          
                                                                                
      IF ((FLEVEL .GT. 600) .OR. (FLEVEL .LT. 001)) THEN            !A          
        LEVEL=-9999999.                                                         
        DECERR=1                                                                
      ENDIF                                                                     
                                                                                
      RETURN                  !return to main program                           
      END                                                                       
