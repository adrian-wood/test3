      SUBROUTINE AIRICE(REPORT,POINT,OPT_ICE)                                   

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : AIRICE                                                         
!                                                                              
! PURPOSE       : DECODE ICE GROUP IN AIREPS                                     
!                                                                              
! DESCRIPTION   : SIMPLE SEARCH FOR SPECIFIED 'KEYWORDS' TO IDENTIFY             
!                 THE ICING GROUP. IF THE GROUP IS NOT IDENTIFIED                
!                 THEN TE GROUP IS SKIPPED AND THE MISSING FLAG SET.             
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:39$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airice.F,v $
!                                                                              
! CHANGE RECORD :
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:39    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:21  usmdb
! Added copyright, modified header and comments - S.Cox
!
! Revision 1.2  97/07/31  09:06:47  09:06:47  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 10:25:48  uspm
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
                                                                                
!declare integers                                                               
      INTEGER POINT              !Position within airep report                  
                                                                                
!declare real                                                                   
      REAL    OPT_ICE            !Decoded value for icing                       
                                                                                
!declare characters                                                             
      CHARACTER *(*) REPORT      !airep report                                  
      CHARACTER*132   HEAD       !Revision information
                                                                                
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airice.F,v $
     &'//'$ $Date: 30/01/2006 20:20:39$ $Revision: 1$'
                                                                                
!-----------------------------------------------------------------------
! The search looks for the word 'ICE' followed by another word such as           
! LGT. When found the OPT_ICE variable is set to the appropiate BUFR             
! code table number for icing.Since we know the size of each group here          
! we are able to specify the amount which pointer should move                    
!-----------------------------------------------------------------------
                                                                                
      IF (REPORT(POINT:POINT+2) .EQ. 'ICE') THEN                                
        IF (REPORT(POINT+4:POINT+6) .EQ. 'LGT') THEN                            
          OPT_ICE=1                                                             
          POINT=POINT+7                                                         
        ELSEIF (REPORT(POINT+4:POINT+6) .EQ. 'MOD') THEN                        
          OPT_ICE=4                                                             
          POINT=POINT+7                                                         
        ELSEIF (REPORT(POINT+4:POINT+6) .EQ. 'SEV') THEN                        
          OPT_ICE=7                                                             
          POINT=POINT+7                                                         
        ENDIF                                                                   
      ELSEIF (REPORT(POINT:POINT+3) .EQ. 'LITE') THEN                           
        OPT_ICE=1                                                               
        POINT=POINT+10                                                          
      ELSEIF (REPORT(POINT:POINT+2) .EQ. 'NIL') THEN                            
        OPT_ICE=15                                                              
        POINT=POINT+4                                                           
      ENDIF                                                                     
                                                                                
!-----------------------------------------------------------------------
! Return to AIROPT                                                               
!-----------------------------------------------------------------------

      RETURN                                                                    
      END                                                                       
