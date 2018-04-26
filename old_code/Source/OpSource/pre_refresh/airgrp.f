      SUBROUTINE AIRGRP(GROUP,LENGTH,NCHAR,CHARR)                               

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : AIRGRP                                                         
!                                                                              
! PURPOSE       : TO LOCATE NON-FIGURES IN A GROUP LOCATED BY AIRLOC &           
!                 TO RETURN THEIR POSITION AND TOTAL NUMBER                      
!                                                                              
! DESCRIPTION   : EACH BYTE OF THE GROUP IS CHECKED IN TURN. IF THE              
!                 BYTE CONTAINS A FIGURE THEN CHARR='N'. IF THE BYTE             
!                 CONTAINS ANYTHING ELSE, THEN CHARR='Y'. NCHAR GIVES            
!                 THE TOTAL NUMBER OF CHARACTERS                                 
!                                                                              
! CALLED BY     : AIRELM (& others?)                                             
!                                                                              
! CALLS TO      : NONE                                                           
!                                                                              
! PARAMETERS    : (1) group to be analysed                         (i)          
!                 (2) length of group                              (i)          
!                 (3) number of non-figures found                  (o)          
!                 (4) string set to N for figure & Y otherwise     (o)          
!                                                                              
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:39$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airgrp.F,v $
!
! CHANGE RECORD :                                                               
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:39    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:21  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  97/08/28  10:16:00  10:16:00  usjl (Jon Lewthwaite)
! don't Overwrite what follows CHARR if LENGTH>LEN(CHARR)
!                                                             
! Revision 1.2  1997/07/31 09:06:37  uspm                                       
! First revision for MVS
!                                                                               
! Revision 1.1  1997/07/04 10:24:23  uspm                                       
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

! Declare variables                                                             
      INTEGER LENGTH      !length of group being investigated                   
      INTEGER CHARCHK     !number of byte being checked                         
      INTEGER NCHAR       !total number of non-digit characters                 
      INTEGER I           !loop counter                                         
                                                                                
      CHARACTER CH*1                                                            
      CHARACTER GROUP*(*) !group being checked                                  
      CHARACTER CHARR*(*) !group represented as 'N' or 'Y' s.                   
      CHARACTER HEAD*132  !Revision information                                 
                                                                                
! Initialize variables                                                          
      HEAD='                                                                    
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airgrp.F,v $                 
     &'//'$ $Date: 30/01/2006 20:20:39$ $Revision: 1$'                      
                                                                                
      NCHAR=0                                                                   
      CHARCHK=0                                                                 
                                                                                
! Set default value of CHARR to 'N' (figure)                                    
      DO I=1,MIN(LENGTH,LEN(CHARR))                                  !a         
        CHARR(I:I)='N'                                                          
      ENDDO                                                                     
                                                                                
! Check each byte and if it contains a non-figure change CHARR to 'Y'           
! and increment NCHAR. Otherwise leave CHARR and NCHAR as they are.             
      DO CHARCHK=1,MIN(LENGTH,LEN(CHARR))                            !a         
        CH=GROUP(CHARCHK:CHARCHK)                                               
        IF (.NOT.(CH .GE. '0' .AND. CH .LE. '9')) THEN                          
          CHARR(CHARCHK:CHARCHK)='Y'                                 !a         
          NCHAR=NCHAR+1                                              !a         
        ENDIF                                                                   
      ENDDO                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
