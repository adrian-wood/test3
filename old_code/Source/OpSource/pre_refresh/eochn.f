      LOGICAL FUNCTION EOCHN(IPT,IMAX,INC)                                      

!-----------------------------------------------------------------------
!Y2K  26.06.1997  EOCHN IS YEAR 2000 COMPLIANT.                                 
!
! revision info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:23$
! $Source: /home/us0400/mdb/op/lib/source/RCS/eochn.F,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:23    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:37  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  97/08/04  13:08:23  13:08:23  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
! 
! Revision 1.2  1997/02/12 12:29:01  uspm
! Update dir name in variable head
!
! Revision 1.1  1997/02/12 11:28:51  uspm
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
      
      INTEGER INC
      INTEGER IPT
      INTEGER IMAX

      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/eochn.F,v $
     &'//'$ $Revision: 1$ $Date: 30/01/2006 20:22:23$ '
                                                                                
      IF (INC.GT.0) THEN                                                        
        EOCHN = IPT+1.GT.IMAX                                                   
      ELSE                                                                      
        EOCHN = IPT - 1.LT.IMAX                                                 
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
