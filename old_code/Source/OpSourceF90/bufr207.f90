SUBROUTINE BUFR207(Y,SCALE,WIDTH,REFVAL)                                        
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! ROUTINE       : BUFR207                                                       
!                                                                               
! CALLED BY     : BUFDATA                                                       
!                                                                               
! PURPOSE       : Change scale, width & reference value together.               
!  (informal)     Y gives the scale change, as in 202YYY,                       
!                 the reference value is given that scale change,               
!                 and a new width is calculated from the range implied          
!                 by the old width (maybe wasting the odd bit).                 
!                                                                               
!    (formal)     Add Y-128 to SCALE.                                           
!                 Multiply REFVAL by 10**(Y-128).                               
!                 From range corresponding to old width & from new              
!                 SCALE & REFVAL derive a width which will cover at             
!                 least the same range (but may use a bit too many).            
!                                                                               
! CALLS         : nothing                                                       
!                                                                               
! PARAMETERS    :                                                               
!                                                                               
!  (1) Y        from 207YYY                                                     
!                (not changed)                                                  
!  (2) SCALE    scale (power of ten)                                            
!                (changed by Y-128)                                             
!  (3) WIDTH    number of bits for values                                       
!                (changed to give at least the same range)                      
!  (4) REFVAL   reference value                                                 
!                (changed by 10**(Y-128))                                       
!                                                                               
! ERROR RETURNS : none                                                          
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Revision: 1$                                                                 
! $Date: 12/02/2010 14:41:52$                                                   
! $Source: /data/us0400/mdb/op/lib/source/RCS/bufr207.F,v $                     
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         12/02/2010 14:41:52    Richard Weedon
!       further files ported
!       
! $                                                                             
! Revision 2.0  2002/03/07  16:18:19  16:18:19  usmdb (Generic MetDB account)   
! Initial version                                                               
!                                                                               
!-----------------------------------------------------------------------        
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.                   
!                                                                               
! Met Office, United Kingdom                                                    
!                                                                               
! The use, duplication and disclosure of this code is strictly                  
! prohibited without the permission of The Meteorological Database              
! Team at the above address.                                                    
!-----------------------------------------------------------------------        
                                                                                
IMPLICIT      NONE                                                              
                                                                                
INTEGER       Y                                                                 
INTEGER       SCALE                                                             
INTEGER       WIDTH                                                             
INTEGER       REFVAL                                                            
INTEGER       NEW_SCALE                                                         
INTEGER       NEW_REFVAL                                                        
                                                                                
!-----------------------------------------------------------------------        
! The current range of values is (2**WIDTH-1+REFVAL)/10**SCALE.                 
! (2**N possible values in N bits, one of them missing, hence -1.)              
! Write the same formula for the new width, refval & scale;                     
! say that the new range must be at least as great as the old;                  
! so put the ranges equal & solve for the new width, rounding up.               
!    But there's a risk that, if these calculations give slightly               
! different results on different machines, the final integer could              
! be fatally different!  To avoid this use the formula derived in               
! the proposal, which might occasionally waste an extra bit but                 
! should give the same result on any machine.                                   
!                                                                               
!    Although the proposal says the operation should not be used when           
! the division below gives a non-integer result, there should be no             
! problem if encode & decode are consistent.  If the reference value            
! is negative, subtracting 1 should make the new range include the              
! old, whereas truncation (e.g. of a negative value to zero) could              
! lead to values at one end of the range being encoded as missing.              
!    But this is not in the proposal, so we can't assume that other             
! centres will code the operation the same way - hence the warning.             
!-----------------------------------------------------------------------        
                                                                                
NEW_SCALE=SCALE+(Y-128)                                                         
IF (Y.GE.128) THEN                                                              
  NEW_REFVAL=REFVAL*10**(Y-128)                                                 
ELSE                                                                            
  NEW_REFVAL=REFVAL/10**(128-Y)                                                 
  IF (NEW_REFVAL*10**(128-Y).NE.REFVAL) THEN                                    
    PRINT *,'BUFR207: REFVAL calculation gives non-integer result'              
    IF (REFVAL.LT.0) NEW_REFVAL=NEW_REFVAL-1                                    
  ENDIF                                                                         
ENDIF                                                                           
                                                                                
!-----------------------------------------------------------------------        
!     W=(2**WIDTH-1+REFVAL)/10.0**SCALE   ! range of old values                 
!     W=W*10.0**NEW_SCALE+1-NEW_REFVAL                                          
!     W=LOG(W)/LOG(2.0)                   ! log to the base 2 of above          
!     WIDTH=INT(W)+1                      ! round up integer width              
!-----------------------------------------------------------------------        
                                                                                
WIDTH=WIDTH+(20*(Y-128)+ISIGN(1,Y-128)+3)/6                                     
SCALE=NEW_SCALE                                                                 
REFVAL=NEW_REFVAL                                                               
                                                                                
RETURN                                                                          
END SUBROUTINE BUFR207                                                          
