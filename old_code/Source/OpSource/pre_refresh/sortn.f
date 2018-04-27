      SUBROUTINE SORTN(A,L,N,MASK)                                              
                                                                                
!-----------------------------------------------------------------------
!                                                                              
! ROUTINE       : SORTN                                                        
!                                                                              
! PURPOSE       : to sort a 2-dimensional array on fields defined in           
!               : mask into ascending order                                    
!                                                                              
! DESCRIPTION   : this is a fortran version of various sorts based             
!               : on the kdf9 program l82.  it uses a "diminishing             
!               : increment" method based on frank & lazarus (comm             
!               : a.c.m.,jan 1960).  for a general discussion see              
!               : knuth: "art of computer programming: sorting and             
!               : searching" (1973) -  shelf mark ec5b(43).                    
!                                                                              
! CALLED BY     : anything
!                                                                              
! ARGUMENTS     : (1) array(l,n): n sets of l numbers                          
!               : (2) number of numbers in each item                           
!               : (3) number of items to be sorted                             
!               : (4) mask with zeros for numbers to be ignored                
!                                                                              
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:23$
! $Source: /home/us0400/mdb/op/lib/source/RCS/sortn.F,v $
!
! CHANGE RECORD :
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:23    Sheila Needham  
! $
! Revision 2.0  2001/03/07 10:19:19  usmdb
! Put a check around HEAD= so it is only set on first call to the
! routine. Added a SAVE. Added copyright and modified header and
! comments - S.Cox
!
! Revision 1.1  1997/06/19 13:42:35  uspm
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
                                                                                
      INTEGER A(L,N),MASK(L), X   
      CHARACTER HEAD*132
      LOGICAL HEADSET         !- FALSE on 1st call only             !2.0

      SAVE                                                          !2.0

      DATA HEADSET/.FALSE./                                         !2.0

!-----------------------------------------------------------------------
! revision information
!-----------------------------------------------------------------------
      
      IF (.NOT.HEADSET) THEN                                        !2.0
        HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/sortn.F,v $
     &'//'$ $Date: 30/01/2006 20:24:23$ $Revision: 1$'                                              
        HEADSET=.TRUE.                                              !2.0
      ENDIF                                                         !2.0
                                                                                
!-----------------------------------------------------------------------        
! first set the increment x.  this will be quartered or halved as the           
! sort goes on, being rounded up to an odd number to give a decreasing          
! sequence ending with 1.  items with subscripts x apart will be                
! compared and swapped if out of order.                                         
!-----------------------------------------------------------------------        
                                                                                
      X=N                                                                       
   10 X=X/2                                                                     
      IF (X.GE.8) X=X/2                                                         
      IF (MOD(X,2).EQ.0) X=X+1                                                  
                                                                                
!-----------------------------------------------------------------------        
! do n minus x comparisons for each increment.  if a swap is done,              
! repeat the comparison involving the item just moved up.                       
!-----------------------------------------------------------------------        
                                                                                
      DO 50 K=1,N-X                                                             
       I=K                                                                      
   20  J=I+X                                                                    
                                                                                
!-----------------------------------------------------------------------        
! where the mask is set, compare the numbers in the two items in turn,          
! working from left to right (i.e. the order of importance of fields            
! in the mask is simply their order in the items to be sorted), and if          
! necessary swap them using a single number as work area.                       
!-----------------------------------------------------------------------        
                                                                                
       DO 30 M=1,L                                                              
        IF (MASK(M).NE.0) THEN                                                  
          IF (A(M,I).GT.A(M,J)) THEN                                            
            DO 40 MM=1,L                                                        
             INT=A(MM,I)                                                        
             A(MM,I)=A(MM,J)                                                    
             A(MM,J)=INT                                                        
   40       CONTINUE                                                            
                                                                                
!-----------------------------------------------------------------------        
! if a comparison x back can be repeated, do so. in that way                    
! each subset of items x apart will be put into order.                          
!-----------------------------------------------------------------------        
                                                                                
            IF (I.GT.X) THEN                                                    
              I=I-X                                                             
              GO TO 20                                                          
            ELSE                                                                
              GO TO 50                                                          
            ENDIF                                                               
          ELSE IF (A(M,I).LT.A(M,J)) THEN                                       
            GO TO 50                                                            
          ENDIF                                                                 
        ENDIF                                                                   
   30  CONTINUE                                                                 
   50 CONTINUE                                                                  
                                                                                
!-----------------------------------------------------------------------        
! repeat the process with a smaller increment unless the increment              
! has reached 1, in which case the sort has been completed.                     
!-----------------------------------------------------------------------        
                                                                                
      IF (X.GT.1) GO TO 10                                                      
                                                                                
      RETURN                                                                    
      END                                                                       
