SUBROUTINE ENBUFR(DESCR,VALUES,ND,NELEM,NOBS,NAMES,DATIME,MESAGE,&              
     &CMPRES,L)                                                                 
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! ROUTINE       : ENBUFR                                                        
!                                                                               
! PURPOSE       : to make a complete bufr message, given a descriptor           
!               : sequence (which will be expanded if necessary) and            
!               : an array of values corresponding to the expanded              
!               : sequence (i.e. the user must know the expansion               
!               : beforehand!)                                                  
!                                                                               
! CALLED BY     : user...                                                       
!                                                                               
! CALLS         : DESFXY, ENCODE, VALUE                             !2.2        
!                                                                               
! ARGUMENTS     : (1) descriptor sequence (may need expanding)                  
!                 (2) nobs*nelem array of values                                
!                 (3) number of descriptors (input, then output;                
!                      returned as zero if there's an error.)                   
!                 (4) number of elements in array (nelem)                       
!                 (5) number of observations in array (nobs)                    
!                 (6) any character values (with pointers in array)             
!                 (7) five integers: year, month, day, hour, minute             
!                 (8) character string for bufr message                         
!                 (9) flag set if compression required                          
!                 (10) total length of bufr message                             
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Workfile: enbufr.f90$ $Folder: OpSourceF90$                                       
! $Revision: 1$ $Date: 12/02/2010 14:41:52$                                     
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         12/02/2010 14:41:52    Richard Weedon
!       further files ported
!       
! $                                                                             
! Revision 2.2  2002/04/09  11:44:13  11:44:13  usmdb (Generic MetDB account)   
! After each call to ICHAR, the value returned by ICHAR is checked.             
! If < 0, 256 is added - needed for Sun OS.                                     
! Removed ENCODI/NBUFRI. Initialised BUFR, SEVENS in ASCII &                    
! therefore removed calls to EB2ASC. Added IMPLICIT NONE and                    
! declared variables. S.Cox                                                     
!                                                                               
! Revision 2.1  2001/08/09 09:25:00  usmdb                                      
! Changed declaration of VALUES from INTEGER to REAL to                         
! be consistent with calling program - S.Cox                                    
!                                                                               
! Revision 2.0  2001/03/07  10:19:14  usmdb (Generic MetDB account)             
! Added copyright and modified header - S.Cox                                   
!                                                                               
! Revision 1.3  1998/10/07 15:00:18  usmdb                                      
! If year is multiple of 100, set 100 in section 1, not 0                       
! (and don't change the input year!                                             
!                                                                               
! Revision 1.2  97/09/22  09:49:34  09:49:34  uspm (Pat McCormack)              
! Change all labelled statements to be CONTINUE                                 
!                                                                               
! Revision 1.1  1997/06/19 13:39:22  uspm                                       
! Initial revision                                                              
!                                                                               
! Jun 95: allow for length at start getting descriptors from input              
!                                                                               
! Feb 91: different entry points for integer & real - S.Cox                     
!                                                                               
! Aug 90: get descriptors from message (arg.8) if count (arg.3) is 0            
!                                                                               
!-----------------------------------------------------------------------        
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.                   
!                                                                               
! Met Office, United Kingdom                                                    
!                                                                               
! The use, duplication and disclosure of this code is strictly                  
! prohibited without the permission of The Meteorological Database              
! Team at the above address.                                                    
!-----------------------------------------------------------------------        
                                                                                
IMPLICIT NONE                                                 !2.2              
                                                                                
INTEGER DATIME(5)                                                               
INTEGER DESCR(*)                                                                
INTEGER F                                                                       
INTEGER FLOPT       ! flag for optional section               !2.2              
INTEGER I                                                                       
INTEGER IBEFOR      ! number of bits before value in string   !2.2              
INTEGER ID                                                                      
INTEGER L                                                                       
INTEGER L1                                                                      
INTEGER L2                                                                      
INTEGER L3                                                                      
INTEGER L4                                                                      
INTEGER N                                                                       
INTEGER ND                                                                      
INTEGER NED         ! BUFR edtion number                       !2               
INTEGER NELEM                                                                   
INTEGER NOBS                                                                    
INTEGER NYEAR                                                                   
INTEGER VALUE     ! function to get integer from bits         !2.2              
INTEGER X                                                                       
INTEGER Y                                                                       
                                                                                
LOGICAL CMPRES                                                !2.2              
LOGICAL FIRST                                                 !2.2              
                                                                                
REAL VALUES(NOBS,NELEM)                                       !2.1              
                                                                                
CHARACTER BUFR*4                                                                
CHARACTER CCCCNO*2                                                              
CHARACTER HEAD*80                                              !2               
CHARACTER MESAGE*(*)                                                            
CHARACTER NAMES*(*)                                                             
CHARACTER SEVENS*4                                                              
                                                                                
!-----------------------------------------------------------------------        
! Save all variables.                                                           
!-----------------------------------------------------------------------        
                                                                                
SAVE                                                          !2.2              
                                                                                
!-----------------------------------------------------------------------        
! Data statements                                                               
!-----------------------------------------------------------------------        
                                                                                
DATA FIRST/.TRUE./                                            !2.2              
                                                                                
!-----------------------------------------------------------------------        
! Revision information.                                                         
! Initialise variables BUFR and SEVENS (in ASCII).                              
!-----------------------------------------------------------------------        
                                                                                
IF (FIRST) THEN                                               !2.2              
  FIRST=.FALSE.                                               !2.2              
  BUFR= CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)                !2.2              
  SEVENS=CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55)               !2.2              
  HEAD = '$Workfile: enbufr.f90$ ' //&                                            
        &'$Revision: 1$ $Date: 12/02/2010 14:41:52$'                            
ENDIF                                                         !2.2              
                                                                                
!-----------------------------------------------------------------------        
!  if number of descriptors passed as zero, use the descriptors in the          
!  message passed: look for 'bufr', then use the section lengths (skip-         
!  ping any section 2) to reach the descriptors in section 3.                   
!-----------------------------------------------------------------------        
                                                                                
IF (ND.EQ.0) THEN                                                               
  N=1                                                                           
   10   IF (MESAGE(N:N+3).NE.BUFR) THEN                                         
    IF (N.LT.100) THEN                                                          
      N=N+1                                                                     
      GO TO 10                                                                  
    ELSE                                                                        
      PRINT *,' NO DESCRIPTORS PASSED, BUT NO BUFR MESSAGE EITHER'              
      RETURN                                                                    
    ENDIF                                                                       
  ENDIF                                                                         
  N=N+4                                                                         
                                                                                
!-----------------------------------------------------------------------        
! skip section 1.  the length after 'bufr' is (from edition 2 on) the           
! length of the whole message, so skip it first if it points to '7777'.         
! the minimum total length is (section by section) 8+18+0+10+4+4=44.            
!-----------------------------------------------------------------------        
                                                                                
  IBEFOR = 0                                                   !2               
  NED = VALUE(MESAGE(N+3:N+3),IBEFOR,8)  ! BUFR edition no.    !2               
                                                                                
  IBEFOR=0                                                    !2.2              
  L=VALUE(MESAGE(N:N+2),IBEFOR,24)                            !2.2              
  IF (L.GE.44 .AND. MESAGE(N+L-8:N+L-5).EQ.SEVENS) THEN                         
    N=N+4                                                                       
    IBEFOR=0                                                  !2.2              
    L1=VALUE(MESAGE(N:N+2),IBEFOR,24)                         !2.2              
  ELSE                                                                          
    L1=L                                                                        
  ENDIF                                                                         
  IF (NED.EQ.2 .OR. NED.EQ.3) THEN                             !2               
    CCCCNO = CHAR(0) // MESAGE(N+5:N+5)  ! Ignore sub-centre   !2               
  ELSE                                                         !2               
    CCCCNO = MESAGE(N+4:N+5)                                                    
  ENDIF                                                        !2               
!                           Check flag for presence of optional section         
  IF (NED.LT.4) THEN                                           !2               
    IBEFOR = 56        ! Edtns. 0-3: flag in byte 8            !2               
  ELSE                                                         !2               
    IBEFOR = 72        ! Edtn. 4: flag in byte 10              !2               
  END IF                                                       !2               
  FLOPT = VALUE(MESAGE(N:N+9),IBEFOR,1)                        !2               
  N=N+L1                                                                        
                                                                                
!-----------------------------------------------------------------------        
! if there's a section 2 (optional), skip it.                                   
!-----------------------------------------------------------------------        
                                                                                
  IF (FLOPT.EQ.1) THEN                                        !2.2              
    IBEFOR=0                                                  !2.2              
    L2=VALUE(MESAGE(N:N+2),IBEFOR,24)                         !2.2              
    N=N+L2                                                                      
  ENDIF                                                                         
                                                                                
ENDIF !- nd.eq.0                                                                
                                                                                
!-----------------------------------------------------------------------        
!  set up section 1 of message (section 2 missing) with 'BUFR' at start         
!-----------------------------------------------------------------------        
                                                                                
MESAGE(1:4)=BUFR                                                                
                                                                                
! length                                                                        
                                                                                
MESAGE(5:7)=CHAR(0)//CHAR(0)//CHAR(18)                                          
                                                                                
! bufr revision number                                                          
                                                                                
MESAGE(8:8)=CHAR(0)                                                             
                                                                                
! originating centre (country/centre)                                           
! if nd=0, cccc as in input message.                                            
! if nd>0, set met office cccc                                                  
                                                                                
IF (ND.EQ.0) MESAGE(9:10)=CCCCNO                                                
IF (ND.GT.0) MESAGE(9:10)=CHAR(0)//CHAR(74)                                     
                                                                                
! cor sequence number                                                           
                                                                                
MESAGE(11:11)=CHAR(0)                                                           
                                                                                
! optional section flag in first bit                                            
                                                                                
MESAGE(12:12)=CHAR(0)                                                           
                                                                                
! bufr message type (from table a)                                              
                                                                                
MESAGE(13:13)=CHAR(255)                                                         
                                                                                
! message subtype (local number)                                                
                                                                                
MESAGE(14:14)=CHAR(0)                                                           
                                                                                
! number of non-standard table set                                              
                                                                                
MESAGE(15:16)=CHAR(0)//CHAR(1)                                                  
                                                                                
! year, month, day, hour, minute                                                
! (century-year is 100 (not 0!) when year is a multiple of 100)     !1.3        
                                                                                
NYEAR=MOD(DATIME(1),100)                                      !1.3              
IF (NYEAR.EQ.0) NYEAR=100                                     !1.3              
MESAGE(17:17)=CHAR(NYEAR)                                     !1.3              
                                                                                
DO I=2,5                                                      !2.2              
  MESAGE(16+I:16+I)=CHAR(DATIME(I))                                             
ENDDO                                                         !2.2              
                                                                                
! finally one byte of padding                                                   
                                                                                
MESAGE(22:22)=CHAR(0)                                                           
                                                                                
!----------------------------------------------section 3----------------        
! if number of descriptors was zero, copy descriptors from message              
!-----------------------------------------------------------------------        
                                                                                
IF (ND.EQ.0) THEN                                                               
  IBEFOR=0                                                    !2.2              
  L3=VALUE(MESAGE(N:N+2),IBEFOR,24)                           !2.2              
  ND=(L3-7)/2                                                                   
  DO I=1,ND                                                   !2.2              
    ID=N+7+(I-1)*2                                                              
    IBEFOR=0                                                  !2.2              
    DESCR(I)=VALUE(MESAGE(ID:ID+1),IBEFOR,16)                 !2.2              
  ENDDO                                                       !2.2              
ENDIF                                                                           
                                                                                
!----------------------------------------------section 3----------------        
! put descriptors (plus number of obs & compression flag) in section 3          
!                                                                               
! L is length of section 3 (7 bytes before descriptors, padding at end)         
!-----------------------------------------------------------------------        
                                                                                
L=7+ND*2+1                                                                      
MESAGE(23:25)=CHAR(0)//CHAR(L/256)//CHAR(MOD(L,256))                            
MESAGE(26:26)=CHAR(0)                                                           
                                                                                
! number of observations                                                        
                                                                                
MESAGE(27:28)=CHAR(NOBS/256)//CHAR(MOD(NOBS,256))                               
                                                                                
! observed data, may be compressed                                              
                                                                                
IF (CMPRES) THEN                                                                
  MESAGE(29:29)=CHAR(128+64)                                                    
ELSE                                                                            
  MESAGE(29:29)=CHAR(128)                                                       
ENDIF                                                                           
                                                                                
! split descriptor into fxx & yyy                                               
                                                                                
DO I=0,ND-1                                                   !2.2              
  CALL DESFXY(DESCR(I+1),F,X,Y)                                                 
                                                                                
!-----------------------------------------------------------------------        
! the category of a class d descriptor for a whole observation corres-          
! ponds to the kind of data in class a, so set that byte in section 1.          
!-----------------------------------------------------------------------        
                                                                                
  IF (I.EQ.0 .AND. F.EQ.3 .AND. X.GE.7 .AND. X.LE.12) THEN                      
    MESAGE(13:13)=CHAR(X-7)                                                     
  ENDIF                                                                         
                                                                                
! put fxx in one byte & yyy in other                                            
                                                                                
  MESAGE(30+I*2:30+I*2)=CHAR(F*64+X)                                            
  MESAGE(31+I*2:31+I*2)=CHAR(Y)                                                 
ENDDO                                                         !2.2              
                                                                                
!-----------------------------------------------------------------------        
! now encode the bit string in section 4 and put 7777 at the end.               
! (arguments for bufr4 as for this program, but l4 & no datime)                 
!                                                                               
! L=length of sections 0,1 & 3  (no section 2)                                  
!-----------------------------------------------------------------------        
                                                                                
L=4+18+L                                                                        
                                                                                
CALL ENCODE(DESCR,VALUES,ND,NELEM,NOBS,NAMES,MESAGE(L+1:),&                     
           &CMPRES,L4)                                                          
                                                                                
!-----------------------------------------------------------------------        
! l4 is returned as zero if there's an error in bufr4.  if so, return           
! nd=0 to warn the user to stop; if not, put 7777 at the end.                   
!-----------------------------------------------------------------------        
                                                                                
IF (L4.EQ.0) THEN                                                               
  ND=0                                                                          
ELSE                                                                            
  L=L+L4                                                                        
  MESAGE(L+1:L+4)=SEVENS                                                        
  L=L+4                                                                         
ENDIF                                                                           
                                                                                
RETURN                                                                          
END SUBROUTINE ENBUFR                                                           
