      SUBROUTINE DESFXY(DESCR, F,X,Y)     
      
      IMPLICIT NONE                              
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! ROUTINE       : DESFXY
!                                                                      
! PUTPOSE       : to split up a 16-bit BUFR descriptor into            
!                 F (2 bits), X (6 bits) & Y (8 bits).                 
!                                                                      
! CALLED BY     : any                                                  
!                                                                      
! ARGUMENTS     : (1) descriptor (passed in a fullword)                
!                 (2) F - kind of descriptor (element or operator)     
!                 (3) X - class, number of elements                    
!                 (4) Y - element in class, operation                  
!                                                                      
! REVISION INFO :
!
! $Revision: 2$
! $Date: 02/04/2008 11:25:33$
! $Source: /home/us0400/mdb/op/lib/source/RCS/desfxy.F,v $
!
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  2    Met_DB_Project 1.1         02/04/2008 11:25:33    Stan Kellett    added
!        comments to this uncmmented code. Added implicit none and made sure
!       all variables are declared with a description.
!  1    Met_DB_Project 1.0         30/01/2006 20:22:05    Sheila Needham  
! $
! Revision 2.0  2001/03/07 10:19:13  usmdb
! Added copyright. Modified header - S.Cox
!
! Revision 1.1  97/06/19  13:39:11  13:39:11  uspm (Pat McCormack)
! Initial revision
! 
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------

      INTEGER      DESCR        ! BUFR descriptor passed IN
      INTEGER      F            ! Type of descriptor OUT
      INTEGER      X            ! Class if F=0 or 3, 
                                ! number of desctors to replicate if F=1
                                ! or number of elements OUT
      INTEGER      Y            ! element in class F=0 or 3,
                                ! delayed replication factor if F=1
                                ! or operation if F=2  
      INTEGER      IFX          ! used to calculate X
      CHARACTER    HEAD*132     ! Revision header
      
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/desfxy.F,v $
     &'//'$ $Date: 02/04/2008 11:25:33$ $Revision: 2$'
     
      IFX=DESCR/256             ! Chop off the least significant 
                                ! octet from DESCR 
                                ! as FX are in the 2nd least 
                                ! significant octet 
      F=DESCR/16384             ! Chop off the bits which hold the XY 
                                ! and Y values         
      IF (F.GT.3) F=F-(F/4)*4   ! F is a table D sequence          
      X=IFX-(IFX/64)*64         ! Extract value of X which is the 
                                ! 6 least significant bits of IFX                                        
      Y=DESCR-(DESCR/256)*256   ! calculate Y which is the least 
                                ! significant octet of DESCR.                                      
                                                                        
      RETURN                                                            
      END                                                               
