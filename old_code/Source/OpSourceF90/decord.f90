SUBROUTINE DECORD(DESCR,VALUES,NAMES,ND,NOBS,STRING,DSPLAY,&                    
     &LIST,INLIST)                                                              
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! PROGRAM       : DECORD                                                        
!                                                                               
! PURPOSE       : TO DECODE START OF BUFR MESSAGE                               
!                                                                               
! DESCRIPTION   : THE DESCRIPTORS IN SECTION 3, WITH ANY SEQUENCES              
!                 EXPANDED, ARE EXAMINED UNTIL ALL THE (COORDINATE)             
!                 DESCRIPTORS IN AN INPUT LIST HAVE BEEN FOUND                  
!                 AT LEAST ONCE.                                                
!                 DECODE IS THEN CALLED TO GO SO FAR BUT NO FURTHER.            
!                                                                               
! CALLED BY     : GETVALS, MDBSTOR                                   !2         
!                                                                               
! CALLS         : SCRIPT, BUFDATA, DESFXY, VALUE                     !2         
!                                                                               
! PARAMETERS    : (1) SEQUENCE OF DESCRIPTORS        (TO BE RETURNED)           
!                 (2) ARRAY FOR VALUES               (TO BE RETURNED)           
!                 (3) FOR ANY CHARACTER VALUES       (TO BE RETURNED)           
!                 (4) NUMBER OF DESCRIPTORS          (TO BE RETURNED)           
!                      (PASSED AS LENGTH OF DESCRIPTOR ARRAY)         A         
!                 (5) NUMBER OF REPORTS              (TO BE RETURNED)           
!                      (PASSED AS LENGTH OF VALUE ARRAY)              A         
!                 (6) BUFR MESSAGE                                              
!                 (7) FLAG SET IF DISPLAY OF VALUES REQUIRED                    
!                 (8) LIST OF DESCRIPTORS TO BE FOUND AT START                  
!                 (9) NUMBER OF DESCRIPTORS IN LIST AT (8)                      
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Workfile: decord.f90$ $Folder: OpSourceF90$                                       
! $Revision: 1$ $Date: 12/02/2010 14:41:52$                                     
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         12/02/2010 14:41:52    Richard Weedon
!       further files ported
!       
! $                                                                             
! Revision 2.3  2004/11/03 13:43:54  usmdb                                      
! 2.3.  15 November 2004.  Brian Barwell.  INC132621.                           
! Force DECORD to do a full BUFR decode for uncompressed data with              
! more than one observation.                                                    
!                                                                               
! Revision 2.2  2003/05/06  08:03:02  08:03:02  usmdb (MetDB account c/o John C 
!  Ward)                                                                        
! CALL DECODE is now CALL BUFDATA - S.Cox.                                      
!                                                                               
! Revision 2.1  2002/04/09  11:43:50  11:43:50  usmdb (MetDB account c/o usjh)  
! Now makes use of a Return Code from BUFDATA. Initialised BUFR                 
! in ASCII. Some other improvements to code - S.Cox                             
!                                                                               
! Revision 2.0  2001/03/07  10:19:13  10:19:13  usmdb (Generic MetDB account)   
! Added copyright. Modified header & comments - S.Cox                           
!                                                                               
! Revision 1.2  1997/06/19 13:38:56  uspm                                       
! Add $Log to comment panel                                                     
!                                                                               
! SEPT 96 - MOD FOR BUFR VERSIONS >1 (4 EXTRA BYTES IN SECTION 0).              
!                                                                               
! JULY 96 - IMPROVE LOOP FOR FINDING FIRST INSTANCE OF DESCRIPTORS !A           
!                                                                               
! SEPT 94 - SETTING OF 'BUFR' IN BAD SOURCE MODULE CORRECTED - BUT              
!           WHERE IS OPERATIONAL SOURCE?                                        
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
                                                                                
INTEGER DESCR(*)                                                                
INTEGER FLOMP       ! Flag for compressed data (BUFR section 1)                 
INTEGER FLOPT       ! Flag for optional BUFR section 2                          
INTEGER LIST(INLIST)! List of descriptors to look for                           
INTEGER LMAX        ! Max pos. in STRING to check for 'BUFR'  !2.1              
INTEGER ND          ! Number of descriptors in BUFR sequence  !2.3              
INTEGER NEND        ! Number of descriptors to decode         !2.3              
INTEGER F                                                                       
INTEGER RC          ! Return code from BUFDATA                !2.1              
INTEGER VALUE       ! function to get integer from bits        !3               
INTEGER X                                                                       
INTEGER Y                                                                       
                                                                                
REAL    VALUES(*)                                                               
                                                                                
LOGICAL CMPRES                                                                  
LOGICAL DSPLAY                                                                  
LOGICAL HEADSET                                               !2.1              
                                                                                
CHARACTER BUFR*4                                                                
CHARACTER HEAD*80                                              !2               
CHARACTER NAMES*(*)                                                             
CHARACTER STRING*(*)                                                            
                                                                                
!-----------------------------------------------------------------------        
! SAVE all variables.                                                           
!-----------------------------------------------------------------------        
                                                                                
SAVE                                                          !2.1              
                                                                                
!-----------------------------------------------------------------------        
! Data statements.                                                              
!-----------------------------------------------------------------------        
                                                                                
DATA HEADSET/.FALSE./                                         !2.1              
                                                                                
!-----------------------------------------------------------------------        
! Revision information.                                                         
!-----------------------------------------------------------------------        
                                                                                
IF (.NOT.HEADSET) THEN                                                          
  HEAD = '$Workfile: decord.f90$ ' //&                                            
        &'$Revision: 1$ $Date: 12/02/2010 14:41:52$'                            
  HEADSET=.TRUE.                                                                
ENDIF                                                                           
                                                                                
!-----------------------------------------------------------------------        
! Initialise variables.                                                         
!-----------------------------------------------------------------------        
                                                                                
BUFR=CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)                   !2.1              
MAXDES=ND                                                                       
MAXVAL=NOBS                                                                     
                                                                                
!-----------------------------------------------------------------------        
! Find 'BUFR' (IN ASCII) at start of STRING.                                    
!-----------------------------------------------------------------------        
                                                                                
LMAX=MIN(100,LEN(STRING))                                     !2.1              
                                                                                
N=1                                                                             
DO WHILE (STRING(N:N+3).NE.BUFR .AND. N.LT.LMAX)              !2.1              
  N=N+1                                                                         
ENDDO                                                         !2.1              
                                                                                
IF (N.EQ.LMAX) THEN                                           !2.1              
  PRINT *,' BUFR NOT FOUND'                                                     
  NOBS=0                                                                        
  RETURN                                                                        
ELSE                                                          !2.1              
  N=N+4                    ! move past BUFR                   !2.1              
ENDIF                                                                           
                                                                                
IBEFOR = 0                                                     !2               
NUMBUF = VALUE(STRING(N+3:N+3),IBEFOR,8)  ! BUFR edition no.   !2               
IF (NUMBUF.GE.2) N=N+4                                        !2.1              
                                                                                
!                   SKIP SECTION 1 (ANY OPTIONAL SECTION?)                      
IBEFOR = 0                                                     !2               
L1 = VALUE(STRING(N:N+2),IBEFOR,24)     ! Section 1 length     !2               
IF (NUMBUF.LT.4) THEN                                          !2               
  IBEFOR = 56        ! Edtns. 0-3: flag in byte 8              !2               
ELSE                                                           !2               
  IBEFOR = 72        ! Edtn. 4: flag in byte 10                !2               
END IF                                                         !2               
FLOPT = VALUE(STRING(N:N+9),IBEFOR,1)                          !2               
N=N+L1                                                                          
!                   IF THERE'S A SECTION 2, SKIP IT.                            
IF (FLOPT.EQ.1) THEN                                           !2               
  IBEFOR = 0                                                   !2               
  L2 = VALUE(STRING(N:N+2),IBEFOR,24)   ! Section 2 length     !2               
  N=N+L2                                                                        
ENDIF                                                                           
!                   FIND NO. OF REPORTS & COMPRESSION FLAG                      
IBEFOR = 0                                                     !2               
L3 = VALUE(STRING(N:N+2),IBEFOR,24)   ! Section 3 length       !2               
IBEFOR = 32                           ! Skip octets 1-4        !2               
NOBS = VALUE(STRING(N:N+6),IBEFOR,16) ! NOBS in octets 5-6     !2               
FLOMP = VALUE(STRING(N:N+6),IBEFOR,8) ! Flags in octet 7       !2               
                                                                                
IF (MOD(FLOMP,128).GE.64) THEN                                                  
  CMPRES=.TRUE.                                                                 
ELSE                                                                            
  CMPRES=.FALSE.                                                                
ENDIF                                                                           
                                                                                
!-----------------------------------------------------------------------        
! WE HAVE NOW REACHED THE DESCRIPTORS IN SECTION 3.  COPY THEM TO A             
! FULLWORD ARRAY, CALL SCRIPT TO EXPAND ANY SEQUENCES, AND LOOK FOR             
! THE DESCRIPTORS IN THE INPUT LIST, STOPPING AT THE FIRST INSTANCE             
! OF EACH, AND KEEPING THE HIGHEST OF THOSE FIRST SUBSCRIPTS.                   
! NOTE: IF THERE IS MORE THAT ONE OBSERVATION AND THE MESSAGE IS    !2.3        
! NOT COMPRESSED, THERE IS NO ALTERNATIVE TO DOING A FULL DECODE    !2.3        
! BECAUSE IT IS NECESSARY TO GET TO THE END OF EACH OBSERVATION IN  !2.3        
! ORDER TO FIND THE START OF THE NEXT ONE.                          !2.3        
!-----------------------------------------------------------------------        
                                                                                
ND=(L3-7)/2                                                                     
!                   COPY THE DESCRIPTORS TO FULLWORDS                           
DO I=1,ND                                                     !2.1              
  ID=N+7+(I-1)*2                                                                
  IBEFOR = 0                                                   !2               
  DESCR(I) = VALUE(STRING(ID:ID+1),IBEFOR,16)                  !2               
ENDDO                                                         !2.1              
!                   CHECK FOR NEED TO DO FULL DECODE          !2.3              
IF (.NOT.CMPRES .AND. NOBS.GT.1) THEN                         !2.3              
  NEND = ND                                                   !2.3              
!                   PARTIAL DECODE: SEARCH THROUGH DESCRIPTORS !6               
ELSE                                                           !2.3             
!                   EXPAND ANY TABLE D SEQUENCES               !6               
  CALL SCRIPT(DESCR,ND,.FALSE.)                                !6               
                                                                                
!                   SET NEND TO THE HIGHEST REQUIRED DESCRIPTOR                 
  NEND=0                                                                        
  DO 300 J=1,INLIST                                                             
    DO I=1,ND                                                 !2.1              
      IF (DESCR(I).EQ.LIST(J)) THEN                                             
        IF (I.GT.NEND) NEND=I                                   !A              
        GO TO 300                                                               
      ENDIF                                                                     
    ENDDO                                                     !2.1              
  300   CONTINUE                                                                
                                                                                
! F=3 descriptors should all have been expanded out by SCRIPT.       !5         
! Replications should also have been expanded out unless there was   !5         
! delayed replication. To prevent replications being expanded out a  !5         
! second time by BUFDATA below, replication descriptors (F=1) are    !5         
! here replaced by the harmless 202000 if the replication was not    !5         
! delayed (i.e. Y>0).                                                !5         
                                                                                
  DO I=1,NEND                                                  !5               
    IF (DESCR(I)/16384.EQ.1 .AND. MOD(DESCR(I),256).GT.0)&     !5               
       &DESCR(I) = IDES(202000)                                !5               
  ENDDO                                                        !5               
END IF                                                         !6               
                                                                                
!-----------------------------------------------------------------------        
! WE NOW HAVE A SEQUENCE OF DESCRIPTORS TO DECODE COORDINATES ONLY              
!-----------------------------------------------------------------------        
                                                                                
N=N+L3                                                                          
CALL BUFDATA(DESCR,VALUES,NAMES,NEND,NOBS,STRING(N:),&        !2.2              
            &CMPRES,DSPLAY,MAXDES,MAXVAL,RC)                  !2.2              
                                                                                
IF (RC.EQ.1) THEN                                             !2.1              
  CALL DESFXY(DESCR(NEND),F,X,Y)                                                
  IDE=F*100000+X*1000+Y                                                         
  PRINT *,' ERROR',NEND,'-TH DESCRIPTOR IS',IDE                                 
  NOBS=0                                                                        
ELSE                                                          !2.1              
  ND=NEND           ! RETURN NUMBER OF ELEMENTS DECODED                         
ENDIF                                                         !2.1              
                                                                                
!-----------------------------------------------------------------------        
! return to calling program.                                                    
!-----------------------------------------------------------------------        
                                                                                
RETURN                                                                          
END SUBROUTINE DECORD                                                           
