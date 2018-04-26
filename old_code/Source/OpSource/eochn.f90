LOGICAL FUNCTION EOCHN(IPT,IMAX,INC)                                      

!-----------------------------------------------------------------------
!Y2K  26.06.1997  EOCHN IS YEAR 2000 COMPLIANT.                                 
!
! revision info :
!
! $Revision: 2$
! $Date: 23/11/2010 09:43:17$
! $Source:$
!
! change record :
!
! $Log:
!  2    MetDB_Refresh 1.1         23/11/2010 09:43:17    Stan Kellett    ported
!        to f95
!  1    MetDB_Refresh 1.0         23/11/2010 09:17:07    Stan Kellett
!       initial f77 version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        
                                                                                 
IMPLICIT NONE
      
INTEGER, INTENT(IN) :: INC
INTEGER, INTENT(IN) :: IPT
INTEGER, INTENT(IN) :: IMAX  
                                                                              
IF (INC.GT.0) THEN                                                        
  EOCHN = IPT+1.GT.IMAX                                                   
ELSE                                                                      
  EOCHN = IPT - 1.LT.IMAX                                                 
ENDIF 
                                                                    
RETURN                                                                    
END FUNCTION EOCHN                                                               
