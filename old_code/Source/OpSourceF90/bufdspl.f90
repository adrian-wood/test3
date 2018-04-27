SUBROUTINE BUFDSPL(NAME,UNITS,IFORMAT,SCALE,WIDTH,NOBS,IVAL,NCREM,&             
     &VALUES,DESCR)                                                             
! ---------------------------------------------------------------------         
!                                                                               
! Program       : BUFDSPL                                                       
!                                                                               
! Called by     : BUFDATA, BUFDASS                                              
!                                                                               
! Purpose       : to display values of the element just decoded                 
!  (informal)      (code/flag tables or numbers - NOT characters!)              
!                                                                               
!    (formal)     If there's only one value, print it after NAME etc.           
!                 If there's more than one, print 6 per line, below             
!                 the line with NAME etc.                                       
!                 For numbers print SCALE figures after the decimal             
!                 point or (if SCALE<0) a fractionless whole number             
!                 - or print with an exponent if 11 figures too few.            
!                    Look up the meaning of a code figure, but if               
!                 none is found print the figure itself.                        
!                                                                               
! Calls         : CODE to look up the meaning of a code figure                  
!                                                                               
! Parameters    :                                                               
!  (1) NAME     element name from Table B                           (i)         
!                (not changed)                                                  
!  (2) UNITS    element units from Table B                          (i)         
!                (not changed)                                                  
!  (3) IFORMAT   'F' if flag table, 'C' if code table                (i)        
!                (from our Table B, not changed)                                
!  (4) SCALE    from table B (perhaps modified),                    (i)         
!               to decide how many figures to print after decimal point         
!                (not changed)                                                  
!  (5) WIDTH    needed to interpret flag table values               (i)         
!                (to number flags & know how many bits to print)                
!                (not changed)                                                  
!  (6) NOBS     number of obs in message, hence number of values    (i)         
!               of this element to display (if data compressed)                 
!                (not changed)                                                  
!  (7) IVAL     pointer to next (just updated!) slot in value array (i)         
!                (not changed)                                                  
!  (8) NCREM    increment width (zero if all values same,           (i)         
!               hence only one value needs to be displayed)                     
!                (not changed)                                                  
!  (9) VALUES   value array (one or NOBS values to be displayed)    (i)         
!                (not changed)                                                  
! (10) DESCR    element descriptor (for CODE), passed as DESCR(N)   (i)         
!                (not changed)                                                  
!                                                                               
! Error returns : none                                                          
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Revision: 1$                                                                 
! $Date:                                                                        
! $Source:                                                                      
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
!-----------------------------------------------------------------------        
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.                   
!                                                                               
! Met Office, United Kingdom                                                    
!                                                                               
! The use, duplication and disclosure of this code is strictly                  
! prohibited without the permission of The Meteorological Database              
! Team at the above address.                                                    
! ---------------------------------------------------------------------         
                                                                                
IMPLICIT  NONE                                                                  
                                                                                
REAL      VALUES(*)       ! argument (9)                                        
! DOUBLE PRECISION VALUES(*)                                                    
REAL      MISSING                                                               
CHARACTER NAME*54         ! from Table B - argument (1)                         
CHARACTER UNITS*12        ! from Table B - argument (2)                         
CHARACTER IFORMAT*1        ! from our Table B - argument (3)                    
                                                                                
INTEGER   SCALE           ! from Table B - argument (4)                         
INTEGER   NOBS            ! argument (6)                                        
INTEGER   IVAL            ! argument (7)                                        
INTEGER   NCREM           ! argument (8)                                        
INTEGER   DESCR           ! argument (10)                                       
INTEGER   LEFT            ! number left to print (at 6/line)                    
INTEGER   LINE            ! values this line (MIN(6,LEFT))                      
INTEGER   I,J             ! loop variables                                      
INTEGER   ISTART          ! starting subscript for this line                    
INTEGER   ICODEF          ! code figure value for CODE call                     
INTEGER   NWORDS          ! per line (1 or LINE if compressed)                  
INTEGER   WIDTH           ! from Table B - argument (5)                         
INTEGER   WID             ! MIN(WIDTH,24)                     !2.2              
INTEGER   IV              ! value for division by 2 to set bits                 
                                                                                
LOGICAL   LINONE          ! true if first line                                  
CHARACTER*12 WORD(6)      ! to display codes for 6 values                       
CHARACTER*12 TEXT         ! to keep text to edit WORD(I)    !2.1                
CHARACTER*31 BITS(6)      ! to display bits for 6 values                        
CHARACTER*8  FORM(10)     ! FORMat(N) for N figures in fraction                 
CHARACTER*6  FXXYYY       ! to print descriptor as 6 figures                    
                                                                                
DATA MISSING/-9999999./                                                         
                                                                                
DATA FORM/'(F12.1)','(F12.2)','(F12.3)','(F12.4)','(F12.5)',&                   
         &'(F12.6)','(F12.7)','(F12.8)','(F12.9)','(F12.10)'/                   
                                                                                
LOGICAL   HEADSET                                                               
DATA      HEADSET/.FALSE./                                                      
CHARACTER HEAD*132                                                              
                                                                                
IF (.NOT.HEADSET) THEN                                                          
  HEAD='$RCSfile: bufdspl.f,v $ ' //&                                           
      &'$Revision: 1$ $Date: 12/02/2010 14:41:52$'                              
  HEADSET=.TRUE.                                                                
ENDIF                                                                           
                                                                                
LEFT=NOBS                                                                       
LINONE=.TRUE.                                                                   
ISTART=IVAL-NOBS                                                                
DO WHILE (LEFT.GT.0)                                                            
                                                                                
! From number of values left get number on this line (six per line).            
! (LEFT starts at NOBS & is decremented by 6 for every line printed.)           
                                                                                
  LINE=6                                                                        
  IF (LEFT.LT.6) LINE=LEFT                                                      
                                                                                
! Find how many to print on this line                                           
! (more than 1 if data is compressed and NOBS>1)                                
                                                                                
  IF ((NOBS.GT.1 .AND. NCREM.GT.0) .OR. LINONE) THEN                            
    NWORDS=LINE                                                                 
    IF (NCREM.EQ.0) NWORDS=1                                                    
  ENDIF                                                                         
  WRITE (FXXYYY,'(I6.6)') (DESCR/256)*1000+MOD(DESCR,256)                       
                                                                                
! --------------------------- CODE FIGURE -----------------------------         
! If the element is a code figure, look it up in the tables.                    
! (Don't call CODE more than once if all the figures are the same!)             
! ---------------------------------------------------------------------         
                                                                                
  IF (IFORMAT.EQ.'C') THEN                                     !2.      3       
                                                                                
! Look up each code figure to go on this line in the tables.                    
! If no description is returned, display the code figure itself.                
                                                                                
    DO I=1,NWORDS                                                               
      IF (VALUES(ISTART+I-1).EQ.MISSING) THEN                                   
        WORD(I)='   - - - - -'                                                  
      ELSE                                                                      
        ICODEF=VALUES(ISTART+I-1)                                               
        WORD(I)=' '                                                             
        IF (ICODEF.GE.0) CALL CODE(DESCR,ICODEF,WORD(I))                        
        IF (WORD(I).EQ.' ') WRITE (WORD(I)(6:12),'(I7)') ICODEF                 
                                                                                
! For 020003 (present weather) start display with code figure       !2.1        
! (unless description is short, 8 characters or less)               !2.1        
! because starts of descriptions are not unique.                    !2.1        
                                                                                
        IF (DESCR.EQ.20*256+3) THEN                           !2.1              
          IF (WORD(I)(1:4).NE.'    ') THEN                    !2.1              
            TEXT=WORD(I)                                      !2.1              
            IF (ICODEF.LT.10) THEN                            !2.1              
              WRITE(WORD(I),'(I1,1X,A10)')ICODEF,TEXT(1:10)   !2.1              
            ELSE IF (ICODEF.LT.100) THEN                      !2.1              
              WRITE (WORD(I),'(I2,1X,A9)') ICODEF,TEXT(1:9)   !2.1              
            ELSE                                              !2.1              
              WRITE (WORD(I),'(I3,1X,A8)') ICODEF,TEXT(1:8)   !2.1              
            ENDIF                                             !2.1              
          ELSE                                                !2.1              
            WRITE (WORD(I)(1:3),'(I3)') ICODEF                !2.1              
          ENDIF                                               !2.1              
        ENDIF                                                 !2.1              
      ENDIF                                                                     
    ENDDO                                                                       
                                                                                
! Figure(s) looked up & ready for printing: now print descriptions              
! (only one per line if NOBS=1 or no compression)                               
                                                                                
    IF (NOBS.EQ.1 .OR. NCREM.EQ.0) THEN                                         
      IF (LINONE) PRINT *,NAME,'Code ',FXXYYY,' ',WORD(1)                       
    ELSE                                                                        
      IF (LINONE) PRINT *,NAME,'Code ',FXXYYY                                   
      PRINT *,(WORD(I),I=1,LINE)                                                
    ENDIF                                                                       
                                                                                
! --------------------------- FLAG TABLE ------------------------------         
! If it's a flag table rather than a single code figure, split the              
! value into separate flags & print the flag combination(s) as bits.            
! Look up the bits from right to left, skipping the right-hand bit,             
! which is only set for missing data.                                           
! ---------------------------------------------------------------------         
                                                                                
! To display each flag set (if NOBS=1 or all values the same)                   
! take the number representing the set of flags and divide                      
! repeatedly by 2, putting 1 in the string if the result is odd,                
! i.e. the lowest-order bit in what's left is set to 1.                         
                                                                                
  ELSE IF (IFORMAT.EQ.'F') THEN                                !2.3             
    IF (LINONE .AND. WIDTH.GT.24) PRINT *,WIDTH,'-bit flag ',&                  
 &'table: only first 24 flags displayed'                       !2.2             
    WID=MIN(WIDTH,24)                                         !2.2              
                                                                                
    DO I=1,NWORDS                                                               
      IF (VALUES(ISTART+I-1).EQ.MISSING) THEN                                   
        BITS(I)='  - - - - - - - - - - - - - - -'                               
      ELSE                                                                      
        BITS(I)(1:WID)='..............................'       !2.2              
        IV=VALUES(ISTART+I-1)       ! value representing bits                   
        DO J=1,WID                  ! loop round bits         !2.2              
          IF (MOD(IV,2).EQ.1) BITS(I)(WID-J+1:WID-J+1)='1'    !2.2              
          IV=IV/2                   ! lose another bit                          
        ENDDO                                                                   
      ENDIF                                                                     
    ENDDO                                                                       
                                                                                
! If flag table values vary (more than one & not all the same), then            
! print WID characters for each value, dots where bits not set, ones            
! where set.  Up to 24 flags displayed, so print 3 tables per line. !2.2        
                                                                                
! Don't print anything for bit map (printed by BUFDMAP).                        
                                                                                
! (Initialise a string with a character for each bit to dots, then              
! overwrite the dots corresponding to bits which are set with ones,             
! repeatedly dividing by 2 & checking for an odd result as above.)              
                                                                                
    IF (NOBS.EQ.1 .OR. NCREM.EQ.0) THEN                                         
      IF (LINONE .AND. .NOT.(FXXYYY.EQ.'031031')) THEN                          
        PRINT *,NAME,'Flag ',FXXYYY,' ',BITS(1)(1:WID)        !2.2              
      ENDIF                                                                     
    ELSE                                                                        
      IF (LINONE) PRINT *,NAME,'Flag ',FXXYYY                                   
      PRINT *,(BITS(I)(1:WID),'  ',I=1,MIN(3,LINE))           !2.2              
      IF (LINE.GT.3) THEN                                                       
        PRINT *,(BITS(I)(1:WID),'  ',I=4,MIN(6,LINE))         !2.2              
      ENDIF                                                                     
    ENDIF                                                                       
                                                                                
! ----------------------------- NUMBER --------------------------------         
! If whole number, first convert to a character string ending with a            
! point (F12.0), then output the figures omitting the point.                    
! Numbers with negative scales can take values not displayable in 11            
! figures: print such values with an exponent.  (1P puts one figure             
! before the decimal point.)                                                    
! ---------------------------------------------------------------------         
                                                                                
  ELSE                                                        !2.3              
    IF (SCALE.LE.0) THEN                                                        
      DO I=1,LINE                                                               
        IF (VALUES(ISTART+I-1).EQ.MISSING) THEN                                 
          WORD(I)='  - - - - - '                                                
        ELSE IF (VALUES(ISTART+I-1).GT.99999999999.) THEN                       
          WRITE (WORD(I),'(1P,E11.5)') VALUES(ISTART+I-1)                       
        ELSE                                                                    
          WRITE (WORD(I),'(F12.0)') VALUES(ISTART+I-1)                          
        ENDIF                                                                   
      ENDDO                                                                     
                                                                                
! If there's only one value to print, put name & units on same line.            
! Otherwise print name & units on one line, then values 6 to a line.            
                                                                                
      IF (NOBS.EQ.1 .OR. NCREM.EQ.0) THEN                                       
        IF (LINONE) PRINT *,NAME,UNITS,' ',WORD(1)(1:11)                        
      ELSE                                                                      
        IF (LINONE) PRINT *,NAME,UNITS                                          
        PRINT *,(WORD(I)(1:11),I=1,LINE)                                        
      ENDIF                                                                     
                                                                                
! If scale is positive, print SCALE figures after the decimal point.            
! Either one value on the same line as the name & units:                        
                                                                                
    ELSE IF (SCALE.GT.0) THEN                                                   
      IF (NOBS.EQ.1 .OR. NCREM.EQ.0) THEN                                       
        IF (LINONE) THEN                                                        
          IF (VALUES(ISTART).EQ.MISSING) THEN                                   
            WORD(1)='   - - - - -'                                              
            PRINT *,NAME,UNITS,WORD(1)                                          
          ELSE                                                                  
            IF (SCALE.LE.10) THEN                                               
              WRITE (WORD(1),FORM(SCALE)) VALUES(ISTART)                        
              PRINT *,NAME,UNITS,WORD(1)                                        
            ELSE                                                                
              PRINT *,NAME,UNITS,' ',VALUES(ISTART)                             
            ENDIF                                                               
          ENDIF                                                                 
        ENDIF                                                                   
      ELSE                                                                      
                                                                                
! Or name & units, then six values per line:                                    
! (But can't cope with SCALE>10 with 6 values per line!)                        
                                                                                
        IF (LINONE) PRINT *,NAME,UNITS                                          
        DO I=1,LINE                                                             
          IF (VALUES(ISTART+I-1).EQ.MISSING) THEN                               
            WORD(I)='  - - - - - '                                              
          ELSE                                                                  
            IF (SCALE.LE.10) THEN                                               
              WRITE (WORD(I),FORM(SCALE)) VALUES(ISTART+I-1)                    
            ELSE                                                                
              PRINT *,SCALE,'is scale; only 10 figures shown'                   
              WRITE (WORD(I),FORM(10)) VALUES(ISTART+I-1)                       
            ENDIF                                                               
          ENDIF                                                                 
        ENDDO                                                                   
        PRINT *,(WORD(I),I=1,LINE)                                              
      ENDIF                                                                     
    ENDIF                                                                       
  ENDIF                                                                         
                                                                                
  LEFT=LEFT-6                                                                   
  ISTART=ISTART+6                                                               
  LINONE=.FALSE.                                                                
ENDDO                                                                           
RETURN                                                                          
END SUBROUTINE BUFDSPL                                                          
