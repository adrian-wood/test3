      SUBROUTINE AIRTMP(REPLEN,REPORT,POINT,GRPLEN,CHARR,TEMP)          

      IMPLICIT NONE                                                     

!---------------------------------------------------------------------  
!                                                                       
! PROGRAM       : AIRTMP                                                  
!                                                                       
! PURPOSE       : TO DECODE THE TEMPERATURE GROUP IN AN AIREP REPORT      
!                                                                       
! DESCRIPTION   : THE TEMPERATURE GROUP IS DECODED BY USING A FIXED       
!                 'PATTERN' TO CHECK AGAINST.                             
!                                                                       
! CALLED BY     : AIRELM                                                  
!                                                                       
! CALLS TO      : NONE                                                    
!                                                                       
! PARAMETERS    : 1. REPLEN -LENGTH OF REPORT -I                          
!                 2. REPORT- REPORT BEING EXPANDED - I                    
!                 3. POINT -POINTER TO POSITION WITHIN REPORT -I/O        
!                 4. GRPLEN - LENGTH OF GROUP BEING DECODED - I
!                 5. CHARR - 'PATTERN' OF Y'S AND N'S FOR GROUP-I
!                 4. TEMP -AIR TEMPERATURE DECODE -O                      
!                                                                       
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:48$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airtmp.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:48    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:26  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.7  99/04/12  10:57:40  10:57:40  usmdb (Generic MetDB accou
! 19 April 1999     C Long
! 1.7 Remove write
! 
! Revision 1.6  98/05/15  10:24:50  10:24:50  usmdb (Generic MDB account
! corrected incorrect handling of final group                         !d
!
! Revision 1.5  98/04/20  07:03:52  07:03:52  usmdb (Generic MDB account
! corrected pattern detection                                         !c
!
! Revision 1.4  98/02/04  14:58:59  14:58:59  usmdb (Generic MDB account
! Correct substring definition. Set temp as 9999999 to force airelm to
! attempt to decode this group as a wind group.                       !B
!
! 02/01/98 - correct substring definition, print bad group if error   !a
!
! Revision 1.3  1997/11/06 09:52:11  uspm
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
!
!declare integers                                                       
      INTEGER REPLEN           !Length of airep report being decoded    
      INTEGER POINT            !Position within report                  
      INTEGER NCHAR            !Number of non-numerics within group     
      INTEGER GRPLEN           !Length of group being decoded           
      INTEGER TEMP1            !Raw temperature value                   
                                                                        
!declare characters                                                     
      CHARACTER REPORT*(*)     !Airep report                            
      CHARACTER CHARR*20       !Dummy string for group                  
      CHARACTER HEAD*132       !Revision information                    
      CHARACTER TEMP_SIGN*2 !Sign of AIR temperature
                                                                        
!declare real                                                           
      REAL TEMP                !Decoded temperature                     
                                                                        
!declare logical                                                        
      LOGICAL LREPFL           !Indicates end of report if true         
      LOGICAL END_REP          !Indicates '=' found                     
      LOGICAL TEMP_OK

!initialise variables                                                   

      TEMP=-9999999.                                                    
      END_REP=.FALSE.
      TEMP_OK=.TRUE.                                                    
      TEMP1=-9999999
      TEMP_SIGN(:)=' '
      HEAD='
     &$Source:
     &'//'$ $Date: 30/01/2006 20:20:48$ $Revision: 1$'

!---------------------------------------------------------------------- 
!the next section looks at the temperature group. Using known pattern   
!to detect the group                                                    
!---------------------------------------------------------------------- 
                                                                        
!
!   if temperature group is the final group then the pointer points  !d
!   to the end final '=' sign and not the beginning of the next      !d
!   report as usual                                                  !d
!                                                                    !d
      IF (REPORT(POINT:POINT).EQ.'=') POINT=POINT+2                  !d
!                                                                    !d
      IF ((GRPLEN .EQ. 3) .AND. (CHARR(1:3) .EQ. 'YNN')) THEN           
        READ(REPORT(POINT-3:POINT-2),'(I2)')TEMP1
        TEMP_SIGN(1:1)=REPORT(POINT-4:POINT-4)
      ELSEIF ((GRPLEN .EQ. 4) .AND. (CHARR(1:4) .EQ. 'YYNN')) THEN
        READ(REPORT(POINT-3:POINT-2),'(I2)')TEMP1                       
        TEMP_SIGN(1:2)=REPORT(POINT-5:POINT-4)
      ELSEIF ((GRPLEN .EQ. 2) .AND. (CHARR(1:2) .EQ. 'YN')) THEN
        READ(REPORT(POINT-2:POINT-2),'(I1)')TEMP1
        TEMP_SIGN(1:1)=REPORT(POINT-3:POINT-3)
      ELSEIF ((GRPLEN .EQ. 3) .AND. (CHARR(1:3) .EQ. 'YYN'))THEN
        READ(REPORT(POINT-2:POINT-2),'(I1)')TEMP1
        TEMP_SIGN(1:2)=REPORT(POINT-4:POINT-3)
      ELSEIF ((GRPLEN .EQ. 3) .AND. (CHARR(1:3) .EQ. 'YNY'))THEN
        READ(REPORT(POINT-3:POINT-3),'(I1)')TEMP1                 !c
        TEMP_SIGN(1:1)=REPORT(POINT-4:POINT-4)                    !c
      ELSEIF ((GRPLEN .EQ. 4) .AND. (CHARR(1:4) .EQ. 'YYNY'))THEN
        READ(REPORT(POINT-3:POINT-3),'(I1)')TEMP1                 !c
        TEMP_SIGN(1:2)=REPORT(POINT-5:POINT-4)                    !c
      ELSEIF ((GRPLEN .EQ. 5) .AND. (CHARR(1:5) .EQ. 'YYNNY'))THEN
        READ(REPORT(POINT-4:POINT-3),'(I2)')TEMP1                 !c !a
        TEMP_SIGN(1:2)=REPORT(POINT-6:POINT-5)                    !c
      ELSEIF ((GRPLEN .EQ. 4) .AND. (CHARR(1:4) .EQ. 'YNNY'))THEN
        READ(REPORT(POINT-4:POINT-3),'(I2)')TEMP1                 !c
        TEMP_SIGN(1:1)=REPORT(POINT-5:POINT-5)                    !c
      ELSEIF ((GRPLEN .EQ. 2) .AND. (CHARR(1:2) .EQ. 'YY')) THEN      !B
        TEMP_SIGN(1:2)=REPORT(POINT-3:POINT-2)                     !c !B
        CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)                !B
        CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-2),GRPLEN,NCHAR,CHARR)!B
        IF ((GRPLEN .EQ. 2) .AND. (CHARR(1:2) .EQ. 'NN')) THEN        !B
          READ(REPORT(POINT-3:POINT-2),'(I2)')TEMP1                !c !B
        ENDIF                                                         !B
      ELSEIF ((GRPLEN .EQ. 1) .AND. (CHARR(1:1) .EQ. 'Y')) THEN       !B
        TEMP_SIGN(1:1)=REPORT(POINT-2:POINT-2)                        !B
        CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)                !B
        CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-2),GRPLEN,NCHAR,CHARR)!B
        IF ((GRPLEN .EQ. 2) .AND. (CHARR(1:2) .EQ. 'NN')) THEN       !B
          READ(REPORT(POINT-3:POINT-2),'(I2)')TEMP1                !c!B
        ENDIF                                                        !B
!---------------------------------------------------------------------- 
!a missing temperature group is usually reported as '///'.              
!---------------------------------------------------------------------- 
                                                                        
      ELSEIF (((GRPLEN .EQ. 3) .AND.                                    
     &(CHARR(:GRPLEN) .EQ. 'YYY')) .OR. ((CHARR(:GRPLEN)                
     &.EQ. 'YYYY') .AND. (GRPLEN .EQ. 4))) THEN                         
        TEMP=-9999999.                                                  
        TEMP_OK=.FALSE.
      ENDIF                                                             
!
! if have just processed final group then reset the pointer          !d
!

      IF (REPORT(POINT-2:POINT-2).EQ.'=') POINT=POINT-2              !d
!
!---------------------------------------------------------------------- 
!now that the temperature group has been found it needs to be           
!converted from degrees C to Kelvin. A check for the sign of the        
!temperature is done first and then the temperature is converted by     
!adding 273.                                                            
!---------------------------------------------------------------------- 

      IF (TEMP_OK) THEN                                                 
        IF ((TEMP_SIGN(1:1) .EQ. 'P') .OR. (TEMP_SIGN(1:2) .EQ. 'PS'))
     &  THEN
          TEMP=FLOAT(TEMP1)+273.                                        
       ELSEIF ((TEMP_SIGN(1:1) .EQ. 'M').OR.(TEMP_SIGN(1:2) .EQ. 'MS')) 
     &  THEN
          TEMP=(FLOAT(TEMP1)*(-1))+273.                                 
        ELSE                                                            
          TEMP=9999999.                                              !B
        ENDIF
      ENDIF
      RETURN                                                            
      END                                                               
