LOGICAL FUNCTION BUFRQOP(DESCR,ND)                                              
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! ROUTINE       : BUFRQOP                                                       
!                                                                               
! CALLED BY     : DECODE, BUFDATA                                               
!                                                                               
! PURPOSE       : To see if there are any quality operations in the             
!  (informal)     descriptor sequence                                           
!                                                                               
!    (formal)     Loop round the descriptors, adding any expanded               
!                 sequence on the end.  (Putting an expansion after             
!                 the original sequence or after any sequence already           
!                 added saves array movements and leaves the original           
!                 descriptors as they were on input.)                           
!                 As soon as an operator has an XX of 22-25 or 32,              
!                 return with QUALOPS true.  If none found, it's false.         
!                                                                               
! CALLS         : DESFXY                                                        
!                 LOCALD                                                        
!                 TABLED                                                        
!                                                                               
! PARAMETERS    :                                                               
!                                                                               
!  (1) DESCR    descriptor array                                                
!                (The ND descriptors are left as they are;                      
!                 the rest of the array is used as work space)                  
!  (2) ND       total number of expanded descriptors                            
!                (not changed)                                                  
!                                                                               
! ERROR RETURNS : none                                                          
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Revision: 1$                                                                 
! $Date: 12/02/2010 14:41:52$                                                   
! $Source: /home/us0400/mdb/op/lib/source/RCS/bufrqop.F,v $                     
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         12/02/2010 14:41:52    Richard Weedon
!       further files ported
!       
! $                                                                             
! Revision 2.0  2001/09/05 08:31:24  usmdb                                      
! Initial version                                                               
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
                                                                                
IMPLICIT NONE                                                                   
                                                                                
INTEGER DESCR(*)   ! descriptor array                                           
INTEGER F,X,Y      ! components of current descriptor                           
INTEGER ND         ! input number of descriptors                                
INTEGER NDS        ! ND incremented for any sequence expansion                  
INTEGER N          ! current descriptor number (in loop)                        
INTEGER NSEQ       ! number of descriptors in sequence                          
                                                                                
NDS=ND                                                                          
N=1                                                                             
BUFRQOP=.FALSE.                                                                 
                                                                                
!-----------------------------------------------------------------------        
! Loop round the input descriptors (expanding any sequence), seeing             
! if any F=2 descriptor is a quality operation.  Set flag if one is             
! found - no need to loop any further.                                          
! If a sequence descriptor is found, just put the expansion on the end          
! for speed - the order of the descriptors is irrelevant for this check.        
!-----------------------------------------------------------------------        
                                                                                
DO WHILE (N.LE.NDS .AND. .NOT.BUFRQOP)                                          
  CALL DESFXY(DESCR(N),F,X,Y)                                                   
  IF (F.EQ.2) THEN                                                              
    IF ((X.GE.22 .AND. X.LE.25) .OR. X.EQ.32) BUFRQOP=.TRUE.                    
  ELSE IF (F.EQ.3) THEN                                                         
    CALL LOCALD(X,Y,DESCR(NDS+1),NSEQ,' ',' ')                                  
    IF (NSEQ.EQ.0) CALL TABLED(X,Y,DESCR(NDS+1),NSEQ)                           
    NDS=NDS+NSEQ                                                                
  ENDIF                                                                         
  N=N+1                                                                         
ENDDO                                                                           
                                                                                
RETURN                                                                          
END FUNCTION BUFRQOP                                                            
