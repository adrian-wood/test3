      SUBROUTINE SYNREG(BLOCK,STNNUM,WMOREG)                            

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SYNREG                                                
!                                                                      
! PURPOSE       : TO RETURN WMO REGION GIVEN WMO BLOCK AND STATION     
!                 NUMBER.                                              
!                                                                      
! CALLED BY     : SYNEXP                                                
!                                                                      
! CALLS         : NONE                                                 
!                                                                      
! PARAMETERS    : (1)  WMO BLOCK                                       
!                 (2)  WMO STATION NUMBER                              
!                 (3)  WMO REGION                                      
!                           ANTARCTICA = REGION 0                      
!                           TOO HIGH   = -32768                        
!                           TOO LOW    = -32768                        
! 
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:56$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synreg.F,v $
!                                                                     
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:56    Sheila Needham  
! $
! Revision 2.0  2001/07/03 11:08:39  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  11:39:16  11:39:16  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:47:32  uspm
! Initial revision
!
! SEP 93 REVISED TO REAL FROM INTEGER*2                        
!                                                                      
! OCT 89 WRITTEN BY JOHN NORTON                                
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

      INTEGER BLOCK,STNNUM,WMOREG                                       
      CHARACTER*132 HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synreg.F,v $
     &'//'$ $Date: 30/01/2006 20:24:56$ $Revision: 1$'
      
      ISTN=BLOCK*1000+STNNUM                                            
*                                                                       
      IF(ISTN.LT.00000)THEN                                             
         WMOREG = -9999999.                                             
      ELSEIF(ISTN.LT.20000)THEN                                         
         WMOREG = 6                                                     
      ELSEIF(ISTN.LT.33000)THEN                                         
        IF(ISTN.LT.20100)THEN                                           
           WMOREG = 2                                                   
        ELSEIF(ISTN.LT.20200)THEN                                       
           WMOREG = 6                                                   
        ELSEIF(ISTN.LT.22000)THEN                                       
           WMOREG = 2                                                   
        ELSEIF(ISTN.LT.23000)THEN                                       
           WMOREG = 6                                                   
        ELSEIF(ISTN.LT.26000)THEN                                       
           WMOREG = 2                                                   
        ELSEIF(ISTN.LT.28000)THEN                                       
           WMOREG = 6                                                   
        ELSE                                                            
           WMOREG = 2                                                   
        ENDIF                                                           
      ELSEIF(ISTN.LT.60000)THEN                                         
        IF(ISTN.LT.35000)THEN                                           
           WMOREG = 6                                                   
        ELSEIF(ISTN.LT.37000)THEN                                       
           WMOREG = 2                                                   
        ELSEIF(ISTN.LT.38000)THEN                                       
           WMOREG = 6                                                   
        ELSEIF(ISTN.LT.40000)THEN                                       
           WMOREG = 2                                                   
        ELSEIF(ISTN.LT.40350)THEN                                       
           WMOREG = 6                                                   
        ELSEIF(ISTN.LT.48600)THEN                                       
           WMOREG = 2                                                   
        ELSEIF(ISTN.LT.48800)THEN                                       
           WMOREG = 5                                                   
        ELSE                                                            
           WMOREG = 2                                                   
        ENDIF                                                           
      ELSEIF(ISTN.LT.70000)THEN                                         
         WMOREG = 1                                                     
      ELSEIF(ISTN.LT.80000)THEN                                         
         WMOREG = 4                                                     
      ELSEIF(ISTN.LT.89000)THEN                                         
        IF(ISTN.EQ.88963.OR.ISTN.EQ.88968)THEN                          
           WMOREG = 0                                                   
        ELSE                                                            
           WMOREG = 3                                                   
        ENDIF                                                           
      ELSEIF(ISTN.LT.90000)THEN                                         
         WMOREG = 0                                                     
      ELSEIF(ISTN.LT.99000)THEN                                         
         WMOREG = 5                                                     
      ELSE                                                              
         WMOREG = -9999999.                                             
      ENDIF                                                             
*                                                                       
      RETURN                                                            
      END                                                               
