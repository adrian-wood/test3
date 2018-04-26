      INTEGER FUNCTION CENTURY (TWOFIG_YEAR)                          
                                                                        
      IMPLICIT NONE                                                     
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : CENTURY                                              
!                                                                      
! PURPOSE       : To return "1900" or "2000" for 20th- and 21st-       
!                 century dates respectively.                         
!                                                                      
! DESCRIPTION   : Uses arbitrary figure of "80" to decide to which     
!                 century a 2-figure year belongs.  This was chosen   
!                 in 1997 to allow for MDB-archive retrieval up to    
!                 5 years before.                                     
!                                                                      
! CALLED BY     : Many date-handling modules.                          
!                                                                      
! CALLS         : None                                                 
!                                                                      
! FUNCTIONS     : None                                                 
!                                                                      
! PARAMETERS    : 2-figure year number.                                
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:39$
! $Source: /home/us0400/mdb/op/lib/source/RCS/century.F,v $
!                                                                 
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:39    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:41  usmdb
! Added copyright and modified header - S.Cox
!
!
! Revision 1.1  97/08/15  09:39:46  09:39:46  uspm (Pat McCormack)
! Initial revision
! 
! INTRODUCED  : July 1997 by Jim Arnott (CC3)                        
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
                                                                        
      INTEGER      TWOFIG_YEAR       ! As supplied by user routine..    
      CHARACTER    HEAD*132

!     Declare revision information
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/century.F,v $
     &'//'$ $Date: 30/01/2006 20:21:39$ $Revision: 1$'
                                                                        
!     Check for year 2000.                                              
                                                                        
      IF (TWOFIG_YEAR .GT. 80) THEN                                     
                                                                        
        CENTURY = 1900                                                  
      ELSE                                                              
        CENTURY = 2000                                                  
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
