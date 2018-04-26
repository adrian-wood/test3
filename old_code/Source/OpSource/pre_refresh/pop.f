      SUBROUTINE POP(SP, ELEM, STACK, IERR)                             
                                                                        
!-----------------------------------------------------------------------
!  SUBROUTINE    : POP (LIFTED OUT FROM MEMBER LIFO 15MAY96)           
!                                                                      
!  PURPOSE       : OPERATION ON ARRAY IMPLEMENTATION OF STACK          
!                                                                      
!  DESCRIPTION   : POPS A CHARACTER OFF THE STACK AND REDUCES THE      
!                  STACK POINTER BY ONE.                               
!                                                                      
!  CALLED BY     :                                                     
!                                                                      
!  CALLS         : NONE                                                
!                                                                      
!Y2K  26.06.1997  POP IS YEAR 2000 COMPLIANT.                           
!                                                                      
! ARGUMENTS     : IN/OUT : INTEGER        SP                            
!                 OUT    : CHARACTER*1    ELEM                          
!                 IN/OUT : CHARACTER*1    STACK(*)                      
!                 OUTPUT : INTEGER        IERR                          
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:54$
! $Source: /home/us0400/mdb/op/lib/source/RCS/pop.F,v $
!
! CHANGE RECORD :
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:54    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:04  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:18:22  13:18:22  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
! 
! Revision 1.1  1997/02/11 16:49:03  uspm
! Initial revision
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

      INTEGER        SP                                                 
      INTEGER        IERR                                               
      CHARACTER*1    STACK(*)                                           
      CHARACTER*1    ELEM                                               
      CHARACTER*132  HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/pop.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:23:54$ '
                                                                                                                                                
      IF (SP.GE.1) THEN                                                 
        ELEM = STACK(SP)                                                
        SP   = SP - 1                                                   
      ELSE                                                              
                                                                        
**********************************************************************  
* ERROR - STACK UNDERFLOW                                               
**********************************************************************  
                                                                        
        IERR = 6                                                        
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
