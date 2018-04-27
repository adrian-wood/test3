SUBROUTINE BUFDRIP(CMPRES,NOBS,DESCR,N,MAXVAL,&                                 
     &VALUES,IVAL,NINCREM,IRC)                                                  
                                                                                
! ---------------------------------------------------------------------         
!                                                                               
! Program       : BUFDRIP                                                       
!                                                                               
! Called by     : BUFDATA, BUFDRPL                                              
!                                                                               
! Purpose       : (a) When called by BUFDRPL: to list any increments            
!  (informal)     before the replication operator & include the                 
!                 increment descriptors in the replication.                     
!                 (b) When called by BUFDATA: to put the increment              
!                 corresponding to an increment descriptor in the               
!                 values array.                                                 
!  (formal)       (a) If the previous element is in a coordinate class          
!                 and has 'increment' in its name, then list it with a          
!                 pointer (IVAL) to the corresponding value.  Do this           
!                 until an element that is not an increment is found.           
!                 (b) Loop round the listed increments to match the             
!                 element descriptor & put the corresponding value              
!                 in the values array (NOBS times if data compressed).          
!                                                                               
! Calls         : DESFXY to split 16-bit descriptor into F, X & Y               
!                 TABLEB to find standard element details                       
!                                                                               
! Parameters    :                                                               
!  (1) CMPRES   true if data in BUFR message is compressed          (i)         
!                (not changed)                                                  
!  (2) NOBS     total number of obs in message                      (i)         
!                (not changed)                                                  
!  (3) DESCR    descriptor array                                    (i)         
!                (not changed)                                                  
!  (4) N        number of current descriptor (operator or increment)(i)         
!                (not changed)                                                  
!  (5) MAXVAL   dimension of VALUES (to check for space left)       (i)         
!                (not changed)                                                  
!  (6) VALUES   value array                                        (i/o)        
!                (returned with increment set - in (b) calls)                   
!  (7) IVAL     current subscript in VALUES                        (i/o)        
!                (incremented)                                                  
!  (8) NINCREM  number of increments found (elements, not counting  (o)         
!               any operators associated with them)                             
!                (returned for use in replication operation)                    
!  (9) IRC      return code:                                        (o)         
!                IRC=111: not enough room left in value array                   
!                IRC=112: increment not found in list                           
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Revision: 1$                                                                 
! $Date:                                                                        
! $Source:                                                                      
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         12/02/2010 14:41:52    Richard Weedon
!       further files ported
!       
! $                                                                             
! Revision 2.1  2003/03/27 13:46:26  usmdb                                      
! 7 April 2003     C Long                                                       
! 2.1  Remove LOCALB call                                                       
!                                                                               
! Revision 2.0  2003/03/10  14:15:57  14:15:57  usmdb (MetDB account c/o usjh)  
! Initial revision                                                              
!                                                                               
!-----------------------------------------------------------------------        
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.                   
!                                                                               
! Met Office, United Kingdom                                                    
!                                                                               
! The use, duplication and disclosure of this code is strictly                  
! prohibited without the permission of The Meteorological Database              
! Team at the above address.                                                    
! ---------------------------------------------------------------------         
                                                                                
IMPLICIT  NONE                                                                  
                                                                                
!  STEP & STACK collect increments for a whole message,                         
! with no reference to replication structure (nested                            
! or not).  This should work for any structure except                           
! one which increments the same element in two nested                           
! replications, which seems nonsensical.                                        
!  But this means that a message with many non-nested                           
! replications incrementing different elements could                            
! fill the arrays.  If so, just empty them & carry on,                          
! with a warning: there's a good chance that the lost                           
! increments are no longer needed.  (If they are, the                           
! decode will fail 112.)                                                        
                                                                                
INTEGER   NARRAY                                                                
PARAMETER (NARRAY=20)                                                           
INTEGER   NSTEPS         ! number of elements in STEP/STACK                     
INTEGER   STEP(NARRAY)   ! pointers to VALUES of elements in STACK              
INTEGER   STACK(NARRAY)  ! descriptors for incremented elements                 
                                                                                
INTEGER   NOBS           ! argument (2)                                         
INTEGER   DESCR(*)       ! argument (3)                                         
INTEGER   N              ! argument (4)                                         
INTEGER   IVAL           ! argument (7)                                         
INTEGER   IRC            ! argument (9)                                         
INTEGER   MAXVAL         ! argument (5)                                         
INTEGER   I,IS           ! loop variables                                       
INTEGER   NINCREM        ! number of increments found                           
INTEGER   LASTL          ! subscript of previous descriptor                     
INTEGER   F,X,Y          ! components of descriptor FXXYYY                      
INTEGER   WIDTH          ! dummy argument for TABLEB call  !2.1                 
INTEGER   SCALE          ! dummy argument for TABLEB call  !2.1                 
INTEGER   REFVAL         ! dummy argument for TABLEB call  !2.1                 
INTEGER   FLINCR         ! descriptor flag to mark increments                   
                                                                                
LOGICAL   CMPRES         ! argument (1)                                         
LOGICAL   LINK           ! set if last descriptor was an increment              
LOGICAL   LISTED         ! set if stack has entry for this element              
CHARACTER NAME*60        ! element name from TABLEB     !2.1                    
CHARACTER UNITS*24       ! dummy argument for TABLEB call  !2.1                 
CHARACTER FORMAT*1       ! dummy argument for TABLEB call  !2.1                 
REAL      VALUES(*)                                                             
! DOUBLE PRECISION VALUES(*)                                                    
                                                                                
DATA FLINCR/262144/      ! 2**18 to set flag                                    
                                                                                
LOGICAL   HEADSET                                                               
DATA      HEADSET/.FALSE./                                                      
CHARACTER HEAD*132                                                              
                                                                                
SAVE                                                                            
                                                                                
IF (.NOT.HEADSET) THEN                                                          
  HEAD='$RCSfile: bufdrip.f,v $ ' //&                                           
      &'$Revision: 1$ $Date: 12/02/2010 14:41:52$'                              
  HEADSET=.TRUE.                                                                
  NSTEPS=0                                                                      
ENDIF                                                                           
                                                                                
! See if current descriptor is a replication operator or an increment.          
                                                                                
CALL DESFXY(DESCR(N),F,X,Y)                                                     
                                                                                
! ---------------------------------------------------------------------         
! If it's a replication operator, look for increment descriptors before         
! it.  List the descriptors and the corresponding value subscripts.             
! ---------------------------------------------------------------------         
                                                                                
! If there are increments immediately before the replication, copy them         
! at the end of the string to be repeated and flag them so that they're         
! interpreted as having no more corresponding fields in the bit string.         
! First find how many increments there are (NINCREM) by checking for the        
! coordinate classes (4-7) and the word 'increment' in the element name.        
! Allow for operators (F=2) among the coordinate descriptors.                   
! N.B. NINCREM is used to set value subscript, so ignores operators!            
! (TABLEB is called only to get the name.)                    !2.1              
! First empty the list of increments (NSTEPS=0) if it's a new message.          
                                                                                
IF (F.EQ.1) THEN                                                                
  IF (NSTEPS.GT.0 .AND. IVAL.LE.STEP(1)) NSTEPS=0                               
  NINCREM=0              ! zero count of increment descriptors                  
  LASTL=N-1                                                                     
  LINK=.TRUE.            ! to start loop...                                     
                                                                                
! Loop while consecutive increments are being found                             
                                                                                
  DO WHILE (LASTL.GT.0 .AND. LINK)                                              
    LINK=.FALSE.                                                                
    CALL DESFXY(DESCR(LASTL),F,X,Y)                                             
                                                                                
! If it's an element descriptor from a coordinate class                         
! and not flagged as an increment in a previous replication,                    
! get the name from Table B & check for increment.                              
                                                                                
    IF (F.EQ.0 .AND. X.GE.4.AND.X.LE.7 .AND.&                                   
       &MOD(DESCR(LASTL)/FLINCR,2).NE.1) THEN                                   
      CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)                     
                                                                                
      IF (WIDTH.GT.0.AND.INDEX(NAME,' INCREMENT ').GT.0) THEN !2.1              
        NINCREM=NINCREM+1                                                       
        LINK=.TRUE.                                                             
                                                                                
! Loop round the stack: is there already an increment for this element?         
                                                                                
        LISTED=.FALSE.                                                          
        I=0                                                                     
        DO WHILE (.NOT.LISTED .AND. I.LT.NSTEPS)                                
          I=I+1                                                                 
          IF (STACK(I).EQ.DESCR(LASTL)+FLINCR) LISTED=.TRUE.                    
        ENDDO                                                                   
                                                                                
! If so, reset the pointer; if not, add a new entry to the stack                
! (If the stack is full, empty it - there's a good chance that only             
!  increments from previous replications will be lost - & continue.)            
! So NSTEPS may be less than NINCREM (but NINCREM must be returned).            
! (Keep a pointer rather than the value itself to allow different               
! increments in data compressed together)                                       
                                                                                
        IF (LISTED) THEN                                                        
          STEP(I)=IVAL-NOBS*NINCREM                                             
        ELSE                                                                    
          NSTEPS=NSTEPS+1                                                       
          IF (NSTEPS.GT.NARRAY) THEN                                            
            PRINT *,'BUFDRIP: too many replicated increments.'                  
            PRINT *,'Some values may be lost: make NARRAY bigger.'              
            NSTEPS=1                                                            
          ENDIF                                                                 
          STACK(NSTEPS)=DESCR(LASTL)+FLINCR                                     
          STEP(NSTEPS)=IVAL-NOBS*NINCREM                                        
        ENDIF                                                                   
      ENDIF              ! end of IF for 'increment' in name                    
      LASTL=LASTL-1                                                             
    ELSE IF (F.EQ.2) THEN! If it's an operator,                                 
      LASTL=LASTL-1      ! go back past it                                      
      LINK=.TRUE.        ! & carry on round loop.                               
    ENDIF                ! end of IF for coord class (& F=0 or 2)               
  ENDDO                  ! end of loop round consecutive incremnts              
                                                                                
! ---------------------------------------------------------------------         
! If it's an increment descriptor (F=0), look it up in the value list.          
! ---------------------------------------------------------------------         
                                                                                
! Look up the increment descriptor in the list of those with values             
! currently set.  Look back from end of list.                                   
                                                                                
ELSE IF (F.NE.1) THEN                                                           
  IS=NSTEPS                                                                     
  DO WHILE (IS.GT.0 .AND. DESCR(N).NE.STACK(IS))                                
    IS=IS-1                                                                     
  ENDDO                                                                         
                                                                                
! If the descriptor is in the list, and there's room for NOBS values            
! of this element, set the increment (NOBS times if data compressed).           
                                                                                
  IF (IS.GT.0) THEN                                                             
    IF (IVAL+NOBS.GT.MAXVAL) THEN                                               
      PRINT *,' not enough room left in value array'                            
      IRC=111                                                                   
      RETURN                                                                    
    ENDIF                                                                       
                                                                                
! Put increment in value array NOBS times if data compressed, once only         
! if not.  (N.B. if NOBS were always 1 without compression there would          
! be no need for the IF, but NOBS>1 is possible without compression!)           
                                                                                
    IF (CMPRES) THEN                                                            
      DO I=0,NOBS-1                                                             
        VALUES(IVAL+I)=VALUES(STEP(IS)+I)                                       
      ENDDO                                                                     
    ELSE                                                                        
      VALUES(IVAL)=VALUES(STEP(IS))                                             
    ENDIF                                                                       
    IVAL=IVAL+NOBS                                                              
  ELSE                                                                          
    PRINT *,' Replicated increment: value not listed in BUFDRIP'                
    IRC=112                                                                     
  ENDIF                                                                         
ENDIF                                                                           
RETURN                                                                          
END SUBROUTINE BUFDRIP                                                          
