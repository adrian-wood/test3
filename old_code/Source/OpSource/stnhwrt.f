      PROGRAM STNHWRT                                                   

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : STNHWRT                                              
!                                                                      
! PURPOSE       : TO POPULATE STNMAS.MAIN AND INDEX IN HIPERBATCH      
!                 FROM MAIN AND INDEX ON DISK                          
!                                                                      
! DESCRIPTION   : SEQUENTIAL READS/WRITES FOR MAIN AND THEN INDEX      
!                                                                      
! CALLED BY     : NONE                                                 
!                                                                      
! CALLS         : NONE                                                 
!                                                                      
! PARAMETERS    : NIL                                                  
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:35$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stnhwrt.F,v $
!                                                                       
! CHANGE RECORD :
!                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:35    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:00  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  11:36:07  11:36:07  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:38:20  uspm
! Initial revision
!
! 17OCT94 CHANGE OPENING OF OUTPUT DATASETS FROM ACCESS SEQUENTIAL TO
!         ACCESS DIRECT   
!      
! 16AUG93 FIRST VERSION OF SOFTWARE                  
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

      INTEGER PHYSREC                                                   
      CHARACTER*692 RECORD                                              
      CHARACTER*132 HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/stnhwrt.F,v $
     &'//'$ $Date: 30/01/2006 20:24:35$ $Revision: 1$'
      
      OPEN(20,ACCESS='DIRECT',FORM='FORMATTED',ACTION='READ',RECL=692)  
      OPEN(25,ACCESS='DIRECT',FORM='FORMATTED',ACTION='WRITE',RECL=692) 
      OPEN(30,ACCESS='DIRECT',FORM='FORMATTED',ACTION='READ',RECL=692)  
      OPEN(35,ACCESS='DIRECT',FORM='FORMATTED',ACTION='WRITE',RECL=692) 
*                                                                       
*     POPULATE STNMAS.MAIN                                              
*                                                                       
      DO PHYSREC = 1,11859                                              
         READ  (20, 100, REC=PHYSREC) RECORD                            
         WRITE (25, 100, REC=PHYSREC) RECORD                            
      END DO                                                            
      PRINT*, 'COPYING MAIN  COMPLETE AT RECORD ', PHYSREC-1            
*                                                                       
*     POPULATE STNMAS.INDEX                                             
*                                                                       
      DO PHYSREC = 1,600                                                
         READ  (30, 100, REC=PHYSREC) RECORD                            
         WRITE (35, 100, REC=PHYSREC) RECORD                            
      END DO                                                            
      PRINT*, 'COPYING INDEX COMPLETE AT RECORD ', PHYSREC-1            
*                                                                       
  100 FORMAT (A692)                                                     
      STOP                                                              
      END                                                               
