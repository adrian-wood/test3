      SUBROUTINE AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)                      
                                                                                
!-----------------------------------------------------------------------
!                                                                              
! SUBROUTINE    : AIRLOC                                                          
!                                                                              
! PURPOSE       : TO LOCATE A GROUP IN A REPORT BY SEARCHING FOR SPACES           
!                                                                              
! DESCRIPTION   : PROGRAM PRODUCES A POINTER INDICATING WHERE THE NEXT            
!                 GROUP STARTS IN THE REPORT AND CALCULATES THE GROUP             
!                 LENGTH FOR EACH GROUP                                           
!                                                                              
! CALLED BY     : ROUTINES
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:42$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airloc.F,v $
!
! CHANGE RECORD :                                                       
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:42    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:23  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  09:07:28  09:07:28  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 10:34:08  uspm
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
                                                                                
!declare variables                                                              
      INTEGER POINT                                                             
      INTEGER GRPLEN                                                            
      INTEGER REPLEN                                                            
      INTEGER END_GRP                                                           
      CHARACTER REPORT*(*)                                                      
      CHARACTER*132 HEAD
      LOGICAL LREPFL                                                            
                                                                                
!initialize variables                                                           
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airloc.F,v $
     &'//'$ $Date: 30/01/2006 20:20:42$ $Revision: 1$'
      
      GRPLEN=0                                                                  
      LREPFL=.FALSE.                                                            
      END_GRP=0                                                                 
                                                                                
                                                                                
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
!IF THE BYTE EXAMINED IS SOME CHARACTER OTHER THAN A SPACE THEN THE             
!GROUP LENGTH IS INCREMENTED  AND THE POINTER IS MOVED TO THE NEXT BYTE         
!IF, HOWEVER A SPACE IS DETECTED THEN THE END_GRP GLAG IS SET TO 1 TO           
!STOP THE LOOPING. POINT IS STILL INCREMENTED TO THAT THE POSITION              
!REPORTED BY POINT IS THE BEGINNING OF THE NEXT GROUP AND NOT THE SPACE         
!INBETWEEN. IF WE HAVE COME TO THE END OF THE REPORT BEING TESTED THE           
!POINTER IS DECREMENTED BY ONE SO THAT IT POINTS TO THE LAST POSITION           
!OF A CHARACTER IN A REPORT                                                     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
      DO WHILE (END_GRP .EQ. 0)                                                 
        IF (POINT .LE. REPLEN) THEN                                             
          IF (REPORT(POINT:POINT) .NE. ' ') THEN                                
            GRPLEN=GRPLEN+1                                                     
            POINT=POINT+1                                                       
          ELSEIF (REPORT(POINT:POINT) .EQ. ' ') THEN                            
            END_GRP=1                                                           
            POINT=POINT+1                                                       
          ENDIF                                                                 
        ELSEIF (POINT .GT. REPLEN) THEN                                         
          END_GRP=1                                                             
          POINT=POINT-1                                                         
          LREPFL=.TRUE.                                                         
        ENDIF                                                                   
      ENDDO                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
