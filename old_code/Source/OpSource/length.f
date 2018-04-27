      SUBROUTINE LENGTH(STRING,DESCR,ND,NVAL)

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : LENGTH                                                       
!                                                                              
! PURPOSE       : TO WORK OUT THE NUMBER OF VALUES IN A BUFR MESSAGE,          
!                 ALLOWING FOR DELAYED REPLICATION  -  WITHOUT WHICH           
!                 THE JOB CAN BE DONE MORE SIMPLY (NO TABLE B CALLS            
!                 BECAUSE NO NEED TO COUNT BITS): EXPAND DESCRIPTORS           
!                 & MULTIPLY FINAL ND BY NUMBER OF REPORTS.                    
!                 THE NUMBER OF DESCRIPTORS & THE EXPANDED SEQUENCE            
!                 ARE RETURNED.                                                
!
!                 ND IS THE NUMBER OF DESCRIPTORS, INCREMENTED
!                 WHENEVER A SEQUENCE IS EXPANDED OR A REPLICATION DONE.
!                 IT CAN ONLY INCREASE.                   
!
!                 N IS A POINTER TO THE DESCRIPTOR SEQUENCE. IT CAN ONLY
!                 ADVANCE, ALTHOUGH THE SEQUENCE MAY GROW LONGER AHEAD
!                 OF IT.                          
!
!                 NVAL IS THE NUMBER OF VALUES, INCREMENTED WHENEVER AN
!                 ELEMENT DESCRIPTOR (THE VALUE MAY HAVE A Q/C FIELD TOO)
!                 IS FOUND.                   
!
!                 NBITS IS THE NUMBER OF BITS FOR THE VALUES UP TO THIS
!                 POINT.                 
!                                                                              
! CALLS         : DESFXY, TABLEB, TABLED, VALUE, SCRIPT                        
!                                                                              
! ARGUMENTS     : (1) BUFR MESSAGE, EITHER WHOLE OR SECTIONS 3 & 4             
!                 (2) ARRAY FOR DESCRIPTORS        (TO BE RETURNED)            
!                 (3) NUMBER OF DESCRIPTORS        (TO BE RETURNED)            
!                 (4) NUMBER OF VALUES IN MESSAGE  (TO BE RETURNED)            
!                                                                              
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:05$
! $Source: /home/us0400/mdb/op/lib/source/RCS/length.F,v $
!
! CHANGE RECORD :                                                              
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:05    Sheila Needham  
! $
! Revision 2.0  2001/03/07 10:19:16  usmdb
! Added copyright. Modified header and comments - S.Cox
!
! Revision 1.2  1997/09/22 09:53:01  uspm
! Change all labelled statements to be CONTINUE
!
! Revision 1.1  1997/06/19 13:40:40  uspm
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
                                         
      CHARACTER STRING*(*), NAME*60,UNITS*24,FORMAT*1, BUFR*4
      CHARACTER HEAD*132                   
      INTEGER DESCR(*), SEQ(20)                                                 
      LOGICAL CMPRES                                                            
      INTEGER F,X,Y, VALUE, SCALE,REFVAL,WIDTH, FLOPT,FLOMP                     
      DATA IWIDTH/0/,IASSOC/0/, BUFR/'BUFR'/, CMPRES/.FALSE./
      
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/length.F,v $
     &'//'$ $Date: 30/01/2006 20:23:05$ $Revision: 1$'
                        
      NVAL=0                                                                    
      CALL EB2ASC(4,BUFR)                                                       

!-----------------------------------------------------------------------
! IF 'BUFR' AT START, WHOLE MESSAGE; IF NOT, ONLY SECTIONS 3 & 4.    *         
!-----------------------------------------------------------------------

      IF (STRING(1:4).EQ.BUFR) THEN                                             
!                                 SKIP SECTION 1 (ANY OPTIONAL SECTION?)        
        L1=ICHAR(STRING(7:7))                                                   
        FLOPT=ICHAR(STRING(16:16))                                              
        N=4+L1                                                                  
!                                 IF THERE'S A SECTION 2, SKIP IT.              
        IF (FLOPT.GE.128) THEN                                                  
          L2=ICHAR(STRING(N+3:N+3))                                             
          N=N+L2                                                                
        ENDIF                                                                   
      ELSE                                                                      
        N=0                                                                     
      ENDIF                                                                     
!                                 FIND NO. OF REPORTS & COMPRESSION FLAGAG      
      L3=ICHAR(STRING(N+3:N+3))                                                 
      NOBS=ICHAR(STRING(N+5:N+5))*256+ICHAR(STRING(N+6:N+6))                    
      FLOMP=ICHAR(STRING(N+7:N+7))                                              
      IF (FLOMP/128.GE.64) CMPRES=.TRUE.                                        
      ND=(L3-7)/2                                                               
!                                 COPY THE DESCRIPTORS TO FULLWORDS             
      DO 30 I=1,ND                                                              
       ID=N+7+(I-1)*2                                                           
       DESCR(I)=ICHAR(STRING(ID+1:ID+1))*256+ICHAR(STRING(ID+2:ID+2))           
   30 CONTINUE                                                                  

!-----------------------------------------------------------------------
! IF DATA IS COMPRESSED, THERE CAN'T BE ANY DELAYED REPLICATION, SO            
! THE DESCRIPTOR SEQUENCE CAN BE EXPANDED WITHOUT REFERENCE TO THE             
! DATA SECTION OF THE MESSAGE, AND THE NUMBER OF VALUES IS SIMPLY              
! ND*NOBS, WHERE ND IS THE NUMBER OF DESCRIPTORS AFTER EXPANSION.              
!-----------------------------------------------------------------------

      IF (CMPRES) THEN                                                          
        CALL SCRIPT(DESCR,ND)                                                   
        NVAL=ND*NOBS                                                            
        RETURN                                                                  
      ENDIF                                                                     

!-----------------------------------------------------------------------
! IF THE DATA IS NOT COMPRESSED, THE JOB MAY NOT BE SO SIMPLE.                 
!
! EXPRESS EACH DESCRIPTOR AS F,X,Y & CHECK FOR OPERATIONS.                     
! CARRY OUT ANY REPLICATION  (FINDING COUNT IN DATA IF NECESSARY)              
!-----------------------------------------------------------------------

      NBITS=(N+L3+4)*8                                                          
      N=1                                                                       
    1 CONTINUE
      CALL DESFXY(DESCR(N),F,X,Y)                                               
      IF (F.EQ.1) THEN                                                          

!-----------------------------------------------------------------------
! IF THERE ARE INCREMENTS IMMEDIATELY BEFORE THE REPLICATION, FIND HOW          
! MANY COORDINATES ARE INCREMENTED (INCORD), TO ADJUST NVAL BELOW.              
!                                   ~~~~~~                                      
!-----------------------------------------------------------------------

        INCORD=0                                                                
  120   CONTINUE
        LASTEL=N-1-INCORD                                                       
        CALL DESFXY(DESCR(LASTEL),JF,JX,JY)                                     
        IF (JX.GE.4.AND.JX.LE.7 .AND. JY.GE.11.AND.JY.LE.20) THEN               
          INCORD=INCORD+1                                                       
          GO TO 120                                                             
        ENDIF                                                                   
                                                                               
!-----------------------------------------------------------------------
! IF THE REPLICATION COUNT IS NOT IN THE DESCRIPTOR, IT'S THE NEXT DATA         
! ITEM.  FIND FIRST THE FIELD WIDTH, THEN THE COUNT ITSELF.  MOVE THE           
! POINTER PAST THE EXTRA DESCRIPTOR, SO THAT THE INSTRUCTIONS BELOW             
! WORK FOR EITHER KIND OF REPLICATION COUNT.                                    
!-----------------------------------------------------------------------
                                                                               
        IF (Y.EQ.0) THEN                                                        
          CALL DESFXY(DESCR(N+1),JF,JX,JY)                                      
          IF (JF.NE.0 .OR. JX.NE.31) RETURN                                     
          CALL TABLEB(JX,JY,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)               
          WIDTH=WIDTH+IWIDTH                                                    
          Y=VALUE(STRING,NBITS,WIDTH)                                           
          N=N+1                                                                 
        ENDIF                                                                   
                                                                               
!-----------------------------------------------------------------------
! NOW THE REPLICATION CAN BE CARRIED OUT:                                       
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                        
! WORK OUT HOW MANY EXTRA DESCRIPTORS, MOVE THE REST DOWN TO MAKE ROOM          
! (WORKING FROM RIGHT TO LEFT TO AVOID REPETITION!), & REPEAT FROM LEFT         
! TO RIGHT TO FILL THE EMPTY SLOT.                                              
!-----------------------------------------------------------------------
                                                                               
        NEXTRA=X*(Y-1)                                                          
                                                                               
!-----------------------------------------------------------------------
! FIRST MAKE ROOM                                                               
!-----------------------------------------------------------------------
                                                                               
        DO 130 I=ND,N+X+1,-1                                                    
         DESCR(I+NEXTRA)=DESCR(I)
  130   CONTINUE
                                                                               
!-----------------------------------------------------------------------
! THEN REPEAT (BUNCH WILL RECUR AT INTERVALS OF X) & ADJUST COUNTS              
!-----------------------------------------------------------------------
                                                                               
        DO 150 I=1,NEXTRA                                                       
         DESCR(N+X+I)=DESCR(N+I) 
  150   CONTINUE
                                                                               
!-----------------------------------------------------------------------
! FINALLY DELETE THE REPLICATION DESCRIPTOR ITSELF                              
!-----------------------------------------------------------------------
                                                                               
        DO 170 I=N+1,ND+NEXTRA                                                  
         DESCR(I-1)=DESCR(I)
  170   CONTINUE
                                                                               
        ND=ND+NEXTRA-1                                                          
                                                                               
!-----------------------------------------------------------------------
! COORDINATE INCREMENTS DON'T NEED TO BE EXPANDED OUT HERE; SIMPLY ADD          
! THE NUMBER OF INCREMENTED COORDINATES TIMES THE NUMBER OF REPEATS.            
!-----------------------------------------------------------------------
                                                                               
        NVAL=NVAL+INCORD*(Y-1)                                                  

!-----------------------------------------------------------------------
! THE ONLY F=2 OPERATIONS THAT CONCERN US ARE FIELD WIDTH CHANGES & Q/C         
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.2) THEN                                                     
        IF (X.EQ.1) IWIDTH=Y                                                    
        IF (X.EQ.4) IASSOC=Y                                                    

!-----------------------------------------------------------------------
! LOOK UP A SEQUENCE  (OVERWRITING THE SEQUENCE DESCRIPTOR ITSELF)             
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.3) THEN                                                     
        CALL TABLED(X,Y,SEQ,NSEQ)                                               

!-----------------------------------------------------------------------
! INSERT SEQUENCE OF DESCRIPTORS, MOVING THE REST DOWN. ADJUST TOTAL.           
!-----------------------------------------------------------------------

        DO 310 I=ND,N+1,-1                                                      
         DESCR(I+NSEQ-1)=DESCR(I) 
  310   CONTINUE

        DO 320 I=1,NSEQ                                                         
         DESCR(N+I-1)=SEQ(I)
  320   CONTINUE

        ND=ND+NSEQ-1                                                            

!-----------------------------------------------------------------------
! INCREMENT NUMBER OF VALUES (TO BE RETURNED) AND                              
! BIT NUMBER (FOR LOOKING UP ANY DELAYED REPLICATION COUNT)                    
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.0) THEN                                                     
        CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)                   
        NVAL=NVAL+1                                                             
        NBITS=NBITS+WIDTH+IWIDTH                                                

        IF (FORMAT.EQ.'R') THEN                                                 
          IF (IASSOC.GT.0) NVAL=NVAL+1                                          
          NBITS=NBITS+IASSOC                                                    
        ENDIF                                                                   
      ENDIF                                                                     

!-----------------------------------------------------------------------
! MOVE PAST THIS DESCRIPTOR & LOOP IF THERE ARE ANY LEFT.                      
!-----------------------------------------------------------------------

      N=N+1                                                                     
      IF (N.LE.ND) GO TO 1                                                      

      RETURN                                                                    
      END                                                                       
