SUBROUTINE BUFDRPL(STRING,NOBS,CMPRES,MAXDES,MAXVAL,IBEFOR,&                    
     &VALUES,IVAL,DESCR,N,ND,NTIMES,IRC)                                        
!----------------------------------------------------------------------         
!                                                                               
! Program       : BUFDRPL                                                       
!                                                                               
! Called by     : BUFDATA                                                       
!                                                                               
! Purpose       : to perform a replication operation, expanding the             
!  (informal)     descriptor array.  Coordinate increments before the           
!                 operator are repeated too; values will be copied to           
!                 the corresponding slots later.                                
!                                                                               
!    (formal)     Copy the XX descriptors after 1XXYYY either YYY times         
!                 or the number of times given by the following count           
!                 if YYY=0. Include any increments before 1XXYYY after          
!                 the XX descriptors.                                           
!                 If the count is zero, flag all the descriptors which          
!                 would have been replicated for deletion (because no           
!                 values correspond to them), plus any increments               
!                 before the operator & any associated F=2 operators.           
!                                                                               
! Calls         : VALUE (function) to get number from bit string                
!                 DESFXY to split descriptor into F, X & Y                      
!                 TABLEB to find element encoding details                       
!                 LOCALB to find details for local elements                     
!                 BUFDRIP to find any increments before the operator            
!                                                                               
! Parameters    :                                                               
!  (1) STRING   bit string from BUFR message (to get count)        (i)          
!                (not changed)                                                  
!  (2) NOBS     number of reports in message, i.e. number of       (i)          
!               values for each field if data is compressed                     
!                (not changed)                                                  
!  (3) CMPRES   flag set if data compressed                        (i)          
!                (not changed)                                                  
!  (4) MAXDES   size of descriptor array (to see if room left)     (i)          
!                (not changed)                                                  
!  (5) MAXVAL   size of input value array (to see if room left)    (i)          
!                (not changed)                                                  
!  (6) IBEFOR   number of bits before value concerned             (i/o)         
!                (updated by VALUE to get past count if there is one)           
!  (7) VALUES   value array, only used if BUFDRIP is called       (i/o)         
!                (may have increment set by BUFDRIP)                            
!  (8) IVAL     subscript for value in VALUES (used to set STEP)  (i/o)         
!                (may be incremented by BUFDRIP)                                
!  (9) DESCR    descriptor array                                  (i/o)         
!                (returned with descriptors replicated,                         
!                 operator & count (if any) flagged for deletion)               
! (10) N        subscript of current descriptor                   (i/o)         
!                (moved past replication & any count on return)                 
! (11) ND       total number of expanded descriptors              (i/o)         
!                (returned adjusted for replication)                            
! (12) NTIMES   number of times replicated (Y or count in data)    (o)          
!                (returned to let bit map be recognised)                        
! (13) IRC      return code:                                       (o)          
!         IRC=101   delayed replication, but no count descriptor                
!         IRC=102   delayed replication, but compressed & counts vary           
!         IRC=103   no room to replicate descriptors                            
!         IRC=104   no room for repeated values                                 
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Workfile: bufdrpl.f90$ $Folder: OpSourceF90$                                      
! $Revision: 1$ $Date: 12/02/2010 14:41:52$                                     
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         12/02/2010 14:41:52    Richard Weedon
!       further files ported
!       
! $                                                                             
!                                                                               
!-----------------------------------------------------------------------        
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.                   
!                                                                               
! Met Office, United Kingdom                                                    
!                                                                               
! The use, duplication and disclosure of this code is strictly                  
! prohibited without the permission of The Meteorological Database              
! Team at the above address.                                                    
!----------------------------------------------------------------------         
                                                                                
IMPLICIT  NONE                                                                  
                                                                                
LOGICAL   CMPRES        ! argument (3)                                          
INTEGER   COUNT_IBEFOR  ! value of IBEFOR for replication count                 
                        ! (used to set IBEFOR back for later obs)               
INTEGER   DESCR(*)      ! argument (9)                                          
INTEGER   F             ! F from descriptor (FXXYYY)                            
INTEGER   FLDEL         ! flag to mark descriptor for deletion                  
INTEGER   FLINCR        ! flag to mark descriptor as replicated                 
                        !  increment with no value in string                    
CHARACTER FORMAT*1      ! from Table B                                          
INTEGER   I             ! short-term loop variable                              
INTEGER   IBEFOR        ! argument (6)                                          
INTEGER   IMBED         ! =1 if count is in data, =0 if it's YYY                
                        ! (used in working out subscripts: one                  
                        !  more descriptor & value if IMBED=1)                  
INTEGER   NINCREM       ! number of increments before operator                  
INTEGER   INSERT        ! loop variable to copy increments after                
                        !  descriptors to be replicated                         
INTEGER   IRC           ! argument (13)                                         
INTEGER   IVAL          ! argument (8)                                          
INTEGER   MAXDES        ! argument (4)                                          
INTEGER   MAXVAL        ! argument (5)                                          
INTEGER   N             ! argument (10)                                         
CHARACTER NAME*60       ! from Table B                                          
INTEGER   NBUNCH        ! number of descriptors to be replicated                
                        !  (not including any increments)                       
INTEGER   ND            ! argument (11)                                         
INTEGER   NEXTRA        ! total number of descriptors to insert                 
INTEGER   NOBS          ! argument (2)                                          
INTEGER   NTIMES        ! argument (12)                                         
INTEGER   REFVAL        ! from Table B                                          
INTEGER   SCALE         ! from Table B                                          
                                                                                
CHARACTER STRING*(*)    ! argument (1)                                          
CHARACTER UNITS*24      ! from Table B                                          
INTEGER   VALUE         !                                                       
REAL      VALUES(*)     ! argument (7)                                          
! DOUBLE PRECISION VALUES(*)                                                    
INTEGER   WIDTH         ! from Table B                                          
INTEGER   X             ! X from descriptor (FXXYYY)                            
INTEGER   Y             ! Y from descriptor (FXXYYY)                            
                                                                                
! Descriptors use only 16 bits, so the top half of a full word can be           
! used for flags to indicate action needed later: the replication               
! descriptor(s) can be deleted once the replication is done, and                
! increments replicated at the end of one replication must not be               
! confused with increments before the next replication operator (if             
! one follows immediately).                                                     
                                                                                
DATA FLDEL/1073741824/  ! to flag descriptor for deletion (2**30)               
DATA FLINCR/262144/     ! to flag replicated increments (2**18)                 
                                                                                
LOGICAL   HEADSET                                                               
DATA      HEADSET/.FALSE./                                                      
CHARACTER HEAD*80                                              !2               
                                                                                
IF (.NOT.HEADSET) THEN                                                          
  HEAD='$Workfile: bufdrpl.f90$ ' //&                                             
      &'$Revision: 1$ $Date: 12/02/2010 14:41:52$'                              
  HEADSET=.TRUE.                                                                
ENDIF                                                                           
                                                                                
! ---------------------------------------------------------------------         
! First deal with the number of times descriptors are to be replicated          
! ---------------------------------------------------------------------         
                                                                                
! If the replication count is not in the descriptor, look in the data.          
! (If the data is compressed, all the counts must be the same)                  
                                                                                
CALL DESFXY(DESCR(N),F,X,Y)                                                     
NBUNCH=X                                                                        
                                                                                
IF (Y.NE.0) THEN                                                                
  NTIMES=Y                                                                      
  IMBED=0                                                                       
ELSE                                                                            
  IMBED=1                                                                       
                                                                                
! Next descriptor must be for embedded count: assume it is if class 31.         
                                                                                
  CALL DESFXY(DESCR(N+1),F,X,Y)                                                 
  IF (F.NE.0 .OR. X.NE.31) THEN                                                 
    PRINT *,' Delayed replication, but no count descriptor'                     
    IRC=101                                                                     
    RETURN                                                                      
  ENDIF                                                                         
                                                                                
! If descriptor is 031yyy, get width (number of bits) from Table B              
                                                                                
  COUNT_IBEFOR=IBEFOR                                                           
  CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)                         
  NTIMES=VALUE(STRING,IBEFOR,WIDTH)                                             
                                                                                
! If compressed, must be zero increment for count, i.e. all counts same         
                                                                                
  IF (CMPRES) THEN                                                              
    IF (VALUE(STRING,IBEFOR,6).NE.0) THEN                                       
      PRINT *,' Delayed replication with compression,'                          
      PRINT *,' but counts vary from report to report'                          
      IRC=102                                                                   
      RETURN                                                                    
    ENDIF                                                                       
  ENDIF                                                                         
                                                                                
! A count could conceivably have a nonzero scale, but only negative, to         
! make it tens, hundreds etc; and the scale can't be changed.                   
                                                                                
  IF (SCALE.LT.0) NTIMES=(NTIMES+REFVAL)/(10**SCALE)                            
ENDIF                                                                           
                                                                                
! ---------------------------------------------------------------------         
! Knowing how many times, we can do the replication.                            
! ---------------------------------------------------------------------         
                                                                                
! But first see if any increments (before the operator) must be                 
! included in the operation (NINCREM descriptors to put on end).                
                                                                                
 CALL BUFDRIP(CMPRES,NOBS,DESCR,N,MAXVAL,VALUES,IVAL,NINCREM,IRC)               
                                                                                
! In the usual case (replicate more than once)                                  
! work out how many extra descriptors, move the rest down to make room          
! (working from right to left to avoid repetition!), add any coordinate         
! increments and repeat (left to right!) to fill the empty slot.                
                                                                                
IF (NTIMES.GT.1) THEN                                                           
  NEXTRA=(NBUNCH+NINCREM)*(NTIMES-1)                                            
  IF (ND+NEXTRA.GT.MAXDES) THEN                                                 
    PRINT *,' No room to replicate descriptors',NEXTRA,NTIMES                   
    IRC=103                                                                     
    RETURN                                                                      
  ENDIF                                                                         
                                                                                
  IF (IVAL+NEXTRA.GT.MAXVAL) THEN                                               
    PRINT *,' No room for repeated values'                                      
    IRC=104                                                                     
    RETURN                                                                      
  ENDIF                                                                         
                                                                                
! First make room, by moving down all the descriptors after those to            
! be replicated, to leave NEXTRA slots for NTIMES-1 further copies.             
                                                                                
  DO I=ND,N+IMBED+NBUNCH+1,-1                                                   
    DESCR(I+NEXTRA)=DESCR(I)                                                    
  ENDDO                                                                         
                                                                                
! Put coordinate increments at end of bunch to be repeated (repeated all        
! but the last time), remembering that there may be operators in between        
! Go back through the descriptors until the right number of element             
! descriptors has been found.                                                   
                                                                                
  INSERT=NINCREM                                                                
  I=N-1                                                                         
  DO WHILE (INSERT.GT.0 .AND. I.GT.0)                                           
    CALL DESFXY(DESCR(I),F,X,Y)                                                 
    IF (F.EQ.0) THEN                                                            
      INSERT=INSERT-1                                                           
      DESCR(N+IMBED+NBUNCH+(NINCREM-INSERT))=DESCR(I)+FLINCR                    
    ENDIF                                                                       
    I=I-1                                                                       
  ENDDO                                                                         
                                                                                
! & repeat (bunch will recur at intervals of NBUNCH+NINCREM)                    
                                                                                
  DO I=1,NEXTRA-NINCREM                                                         
    DESCR(N+IMBED+NBUNCH+NINCREM+I)=DESCR(N+IMBED+I)                            
  ENDDO                                                                         
  ND=ND+NEXTRA                                                                  
                                                                                
! ---------------------------------------------------------------------         
! Set deletion flags & adjust pointers.                                         
! ---------------------------------------------------------------------         
                                                                                
! Flag replication descriptor for deletion later (can't delete before           
! end because of replicated increments: two increments before different         
! replications could come together if a replicator were deleted and the         
! first be wrongly replicated with the second replication) and set X=0          
! and Y=NTIMES to inform users.  (X=0 because a descriptor count would          
! be misleading when the sequence is expanded, and an element count can         
! not always be given (nested delayed replication!).  If NTIMES>256 let         
! it overflow into the X field.  The deletion will not in fact be done          
! at the end; the flag is to avoid reuse.  Flag count for deletion too.         
                                                                                
  DESCR(N)=16384+NTIMES+FLDEL                                                   
                                                                                
  IF (IMBED.EQ.1 .AND. MOD(DESCR(N+1)/FLDEL,2).NE.1) THEN                       
    IF (.NOT.(NOBS.GT.1 .AND. .NOT.CMPRES)) THEN                                
      DESCR(N+1)=DESCR(N+1)+FLDEL                                               
    ENDIF                                                                       
  ENDIF                                                                         
                                                                                
! Move pointer past count & replication descriptor                              
! - but not if several obs not compressed, in which case counts                 
! may vary, so set pointers back to return count as value                       
! (hence count descriptor not flagged for deletion above)                       
                                                                                
  N=N+IMBED+1                                                                   
  IF (NOBS.GT.1 .AND..NOT.CMPRES .AND. IMBED.GT.0) THEN                         
    N=N-IMBED                                                                   
    IBEFOR=COUNT_IBEFOR                                                         
  ENDIF                                                                         
                                                                                
! ---------------------------------------------------------------------         
! Special cases: NTIMES=1 & NTIMES=0                                            
! ---------------------------------------------------------------------         
                                                                                
! Finally the exceptional cases with no replication (this time).                
                                                                                
! If NTIMES=1, flag the replication & count descriptors for deletion            
! (the N-th and - if there is a count in the data - the (N+1)-th...)            
! N.B. the operator won't actually be deleted, it will output NTIMES            
! - see note above.                                                             
                                                                                
ELSE IF (NTIMES.EQ.1) THEN                                                      
  DESCR(N)=16384+NTIMES+FLDEL ! F=1, NTIMES in XXYYY, flag                      
  IF (IMBED.EQ.1 .AND. MOD(DESCR(N+1)/FLDEL,2).NE.1) THEN                       
    DESCR(N+1)=DESCR(N+1)+FLDEL                                                 
  ENDIF                                                                         
  N=N+IMBED+1                 ! past operator & count (if any)                  
                                                                                
! If NTIMES=0, flag for deletion also the descriptors that would be             
! replicated if NTIMES>1 (incl. any increments) and set the values              
! pointer back so that the increments will be overwritten.                      
!                                                                               
! Lines marked "!2.1" have been commented out to prevent removal    !2.1        
! of increment and operator descriptors and corresponding decoded   !2.1        
! data values. Whilst their removal would be no problem for BUFR    !2.1        
! decoding, it would require changes to the element index used for  !2.1        
! MetDB retrieval for the case when the replication count is zero.  !2.1        
                                                                                
ELSE IF (NTIMES.EQ.0) THEN                                                      
!2.1    IVAL=IVAL-NINCREM           ! set values pointer back                   
  DESCR(N)=16384+NTIMES+FLDEL ! F=1, NTIMES in XXYYY, flag                      
                                                                                
! Ensure all descriptors in the replication are flagged for deletion !2         
! including replication descriptors. These normally get unflagged    !2         
! later, so in this case get rid of the descriptor completely.       !2         
                                                                                
  DO I=N+1,N+IMBED+NBUNCH                                                       
    IF (MOD(DESCR(I)/FLDEL,2).NE.1) THEN ! Not flagged         !2               
      CALL DESFXY(DESCR(I),F,X,Y)  ! Decode descriptor         !2               
      IF (F.EQ.1) DESCR(I) = 0     ! Set to 0 if replication   !2               
      DESCR(I) = DESCR(I) + FLDEL  ! Add deletion flag         !2               
    END IF                                                     !2               
  END DO                                                                        
                                    ! flag increments before operator           
!2.1    I=N-1                                                                   
!2.1    DO WHILE (NINCREM.GT.0)                                                 
!2.1      IF (DESCR(I).LT.16384 .AND. MOD(DESCR(I)/FLDEL,2).NE.1) THEN          
!2.1        DESCR(I)=DESCR(I)+FLDEL                                             
!2.1        NINCREM=NINCREM-1                                                   
!2.1        I=I-1                                                               
!2.1      ENDIF                                                                 
!2.1    ENDDO                                                                   
                                    ! & any operator before increments          
!2.1    DO WHILE (DESCR(I).GE.32768)                                            
!2.1      DESCR(I)=DESCR(I)+FLDEL                                               
!2.1      I=I-1                                                                 
!2.1    ENDDO                                                                   
                              ! Finally point past the operator                 
  N=N+IMBED+NBUNCH+1          ! & what would have been replicated               
ENDIF                                                                           
RETURN                                                                          
END SUBROUTINE BUFDRPL                                                          
