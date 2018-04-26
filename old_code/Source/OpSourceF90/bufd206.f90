SUBROUTINE BUFD206(STRING,IBEFOR,DESCR,NOBS,CMPRES,Y,N,IRC)                     
! ---------------------------------------------------------------------         
!                                                                               
! Program       : BUFD206                                                       
!                                                                               
! Called by     : BUFDATA                                                       
!                                                                               
! Purpose       : Skip the value corresponding to a hidden descriptor.          
!  (informal)     (The regulations say this operation only applies to           
!                 a local element descriptor, but here the operation            
!                 is carried out with no check for legitimacy.)                 
!                                                                               
!    (formal)     Skip a Y-bit field after IBEFOR bits in STRING (plus          
!                 an increment width & NOBS increments if the data is           
!                 CMPRESsed).  Skip means move N past the descriptor            
!                 (flagged for deletion) & IBEFOR past the value.               
!                    (The regulations talk about a descriptor, which            
!                 could be an element or sequence; it has been argued           
!                 that only an element can be hidden by 206YYY, but             
!                 there seems no reason why the YYY bits shouldn't              
!                 cover a whole sequence in uncompressed data.)                 
!                                                                               
! Calls         : VALUE (function) to get number from bit string                
!                                                                               
! Parameters    :                                                               
!  (1) STRING   bit string from BUFR message                       (i)          
!                (not changed)                                                  
!  (2) IBEFOR   number of bits in STRING before value concerned    (i)          
!                (updated to skip value & any increments                        
!  (3) DESCR    descriptor array                                   (i)          
!                (not changed)                                                  
!  (4) NOBS     number of reports in message, i.e. number of       (i)          
!               values for each field if data is compressed                     
!                (not changed)                                                  
!  (5) CMPRES   true if data in message compressed                 (i)          
!                (not changed)                                                  
!  (6) Y        number of bits to skip (from 206YYY)               (i)          
!                (not changed)                                                  
!  (7) N        subscript of current descriptor                   (i/o)         
!                (returned incremented by 1)                                    
!  (8) IRC      return code:                                       (o)          
!         IRC=206 descriptor is sequence, not element, & data compressed        
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Revision: 1$                                                                 
! $Date: 12/02/2010 14:41:52$                                                   
! $Source: /home/us0400/mdb/op/lib/source/RCS/bufd206.f,v $                     
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         12/02/2010 14:41:52    Richard Weedon
!       further files ported
!       
! $                                                                             
! Revision 2.2  2003/05/06 15:34:24  usmdb                                      
! 19 May 2003    C Long                                                         
! 2.2 Remove calls to LOCALB & TABLEB, skipping whenever posssible              
!      rather than doing checks which should be done by encode.                 
!                                                                               
! Revision 2.2  2003/05/02 14:01:13  usmdb                                      
! 19 May 2003    C Long                                                         
! 2.1  Remove calls to LOCALB & TABLEB, skipping whenever posssible             
!      rather than doing checks which should be done by encode.                 
!                                                                               
!-----------------------------------------------------------------------        
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.                   
!                                                                               
! Met Office, United Kingdom                                                    
!                                                                               
! The use, duplication and disclosure of this code is strictly                  
! prohibited without the permission of The Meteorological Database              
! Team at the above address.                                                    
!-----------------------------------------------------------------------        
                                                                                
IMPLICIT NONE                                                                   
                                                                                
LOGICAL       CMPRES     ! argument (5)                                         
INTEGER       DESCR(*)   ! argument (3)                                         
INTEGER       FLDEL      ! to flag descriptor for deletion                      
CHARACTER     HEAD*132   ! revision info                                        
LOGICAL       HEADSET    ! TRUE if HEAD set.                                    
INTEGER       IBEFOR     ! argument (2)                                         
INTEGER       IRC        ! argument (8)                                         
INTEGER       N          ! argument (7)                                         
INTEGER       NCREM      ! increment width                                      
INTEGER       NEXTF      ! F from next descriptor                               
INTEGER       NEXTX      ! X from next descriptor                               
INTEGER       NEXTY      ! Y from next descriptor                               
INTEGER       NOBS       ! argument (4)                                         
CHARACTER*(*) STRING     ! argument (1)                                         
INTEGER       VALUE      ! function to get value from data section              
INTEGER       Y          ! argument (6)                                         
                                                                                
SAVE                                                                            
                                                                                
DATA FLDEL/1073741824/   ! 2**30 (descriptor deletion flag)                     
DATA HEADSET/.FALSE./                                                           
                                                                                
IF (.NOT.HEADSET) THEN                                                          
  HEAD='$RCSfile: bufd206.f,v $ ' //&                                           
      &'$Revision: 1$ $Date: 12/02/2010 14:41:52$'                              
  HEADSET=.TRUE.                                                                
ENDIF                                                                           
                                                                                
! If a descriptor is hidden by 206YYY, where Y is the field width,              
! flag descriptor (element or sequence) for deletion & skip Y bits.             
                                                                                
IF (MOD(DESCR(N)/FLDEL,2).NE.1) DESCR(N)=DESCR(N)+FLDEL                         
IBEFOR=IBEFOR+Y                        ! skip Y bits                            
                                                                                
! If the data is compressed, then skip the increments too if it's an            
! element, give up if it's a sequence (impossible to complete skip).            
                                                                                
IF (CMPRES) THEN                                                                
  CALL DESFXY(DESCR(N),NEXTF,NEXTX,NEXTY)                                       
                                                                                
  IF (NEXTF.EQ.0) THEN                                                          
    NCREM=VALUE(STRING,IBEFOR,6)       ! get increment width,                   
    IBEFOR=IBEFOR+NCREM*NOBS           ! skip the increments                    
                                                                                
  ELSE IF (NEXTF.EQ.3) THEN                                                     
    PRINT *,'Sequence descriptor hidden by 206...,'                             
    PRINT *,'but data is compressed, so cannot skip.'                           
    IRC=206                                                                     
  ENDIF                                                                         
ENDIF                                                                           
                                                                                
N=N+1                                  ! finally skip descriptor                
RETURN                                                                          
END SUBROUTINE BUFD206                                                          
