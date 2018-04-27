      SUBROUTINE SCRIPT(DESCR,ND,DSPLAY)                                        

!-----------------------------------------------------------------------
!
! PROGRAM       : SCRIPT                                                       
!                                                                              
! PURPOSE       : TO EXPAND & DISPLAY A BUFR DESCRIPTOR STRING,                
!                 TO SHOW USERS THE ORDER OF VALUES IN ARRAYS.                 
!                 IF THERE IS DELAYED REPLICATION, THE EXPANSION               
!                 CAN'T BE COMPLETED: DESCRIPTORS SUBJECT TO DELAYED           
!                 REPLICATION ARE INSET IN THE DISPLAY. (N.B. DELAYED          
!                 REPLICATION AND COMPRESSION ARE INCOMPATIBLE)                
!                 BLANK LINES BEFORE COORDINATES GROUP ELEMENTS IN A  !A         
!                 WAY THAT SUGGESTS COORDINATE DEPENDENCES.           !A         
!                                                                              
!                 ND IS THE NUMBER OF DESCRIPTORS, INCREMENTED WHENEVER
!                 A SEQUENCE IS EXPANDED OR A REPLICATION DONE. IT CAN
!                 ONLY INCREASE.                   
!
!                 N IS A POINTER TO THE DESCRIPTOR SEQUENCE. IT CAN ONLY
!                 ADVANCE, ALTHOUGH THE SEQUENCE MAY GROW LONGER AHEAD
!                 OF IT.                          
!
!                 NESTLE IS THE NESTING LEVEL, INCREMENTED WHENEVER A
!                 DELAYED REPLICATION IS FOUND. LINES IN THE DISPLAY
!                 ARE INSET BY NESTLE*2. NEST(NESTLE) IS THE NUMBER OF
!                 DESCRIPTORS FOR DELAYED REPLICATION. IT CAN GROW
!                 THROUGH SEQUENCE EXPANSION & REPLICATION, AND IT IS             
!                 DECREMENTED AS ELEMENT DETAILS ARE DISPLAYED. THE
!                 NESTING LEVEL DROPS WHEN NEST(NESTLE) REACHES ZERO.                                       
!                                                                              
! CALLS         : DESFXY, TABLEB, TABLED, LOCALD                               
!                                                                              
! ARGUMENTS     : (1) SEQUENCE OF DESCRIPTORS                                  
!                 (2) NUMBER OF DESCRIPTORS IN SEQUENCE                        
!                 (3) LOGICAL VARIABLE SET TO TRUE FOR DISPLAY                 
!                                                                              
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:10$
! $Source: /home/us0400/mdb/op/lib/source/RCS/script.F,v $
!
! CHANGE RECORD :                                                              
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:10    Sheila Needham  
! $
! Revision 2.0  2001/03/07 10:19:18  usmdb
! Added copyright. Modified header and comments - S.Cox
!
! Revision 1.2  1997/09/22 09:54:24  uspm
! Change all labelled statements to be CONTINUE
!
! Revision 1.1  1997/06/19 13:42:10  uspm
! Initial revision
!
! Jul 96: CALL LOCALD BEFORE TABLED; BIGGER SEQ ARRAY TO ALLOW        !B         
!         FOR SEQUENCE FROM DATA SET, MAY BE > 16 DESCRIPTORS         !B         
!
! Jul 90: BLANK LINE BEFORE THE FIRST OF A SET OF COORDINATES         !A         
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

      INTEGER DESCR(*),NEST(0:9),SEQ(999)                                       
      CHARACTER NAME*60,UNITS*24,FORMAT*1,SPACES*20
      CHARACTER HEAD*132                              
      INTEGER F,X,Y, SCALE,REFVAL,WIDTH                                         
      LOGICAL DSPLAY                                                            
      DATA SPACES/' '/ 
      
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/script.F,v $
     &'//'$ $Date: 30/01/2006 20:24:10$ $Revision: 1$'
                                                              
!-----------------------------------------------------------------------
! EXPRESS DESCRIPTOR AS F,X,Y & CHECK FOR OPERATORS.                           
!                                                                               
! EITHER CARRY OUT A REPLICATION OR INSET THE ELEMENTS IT COVERS.              
!-----------------------------------------------------------------------

      N=1                                                                       
      NESTLE=0                                                                  
      NEST(0)=ND                                                                
    1 CONTINUE
      CALL DESFXY(DESCR(N),F,X,Y)                                               
                                                                               
      IF (F.EQ.1) THEN                                                          

!-----------------------------------------------------------------------
! IF THE REPLICATION COUNT IS IN THE DESCRIPTOR, USE IT                         
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                 
!-----------------------------------------------------------------------

        IF (Y.NE.0) THEN                                                        

!-----------------------------------------------------------------------
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
  130     CONTINUE

!-----------------------------------------------------------------------
! THEN REPEAT (BUNCH WILL RECUR AT INTERVALS OF X) & ADJUST COUNTS              
!-----------------------------------------------------------------------

          DO 150 I=1,NEXTRA                                                     
           DESCR(N+X+I)=DESCR(N+I)
  150     CONTINUE
                                                                               
          ND=ND+NEXTRA                                                          
          NEST(NESTLE)=NEST(NESTLE)+NEXTRA                                      
        ELSE                                                                    

!-----------------------------------------------------------------------
! IF THE REPLICATION COUNT IS ZERO, INSET THE DESCRIPTORS TO BE COPIED          
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                              
!-----------------------------------------------------------------------

          IF (DSPLAY) PRINT *,SPACES(1:1+2*NESTLE),'REPLICATION FACTOR'         
          N=N+1                                                                 
          NESTLE=NESTLE+1                                                       
          NEST(NESTLE)=X+1                                                      
          NEST(NESTLE-1)=NEST(NESTLE-1)-(X+2)                                   
        ENDIF                                                                   

!-----------------------------------------------------------------------
! THE ONLY F=2 OPERATIONS THAT NEED TO BE CONSIDERED ARE SCALING & Q/C         
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.2) THEN                                                     
        IF (X.EQ.2 .AND. Y.GT.0) ISCALE=Y-128                                   
        IF (X.EQ.2 .AND. Y.EQ.0) ISCALE=0                                       
        IF (X.EQ.4) IASSOC=Y                                                    

!-----------------------------------------------------------------------
! LOOK UP A SEQUENCE  (OVERWRITE THE SEQUENCE DESCRIPTOR ITSELF)               
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.3) THEN                                                     
        CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')                             !B        
        IF (NSEQ.EQ.0) CALL TABLED(X,Y,SEQ,NSEQ)                      !B        

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
        NEST(NESTLE)=NEST(NESTLE)+NSEQ                                          

!-----------------------------------------------------------------------
! PRINT OUT DETAILS OF ELEMENT   (INSET IF NECESSARY)                          
! WITH A BLANK LINE BEFORE THE START OF ANY COORDINATE CHANGE(S)               
!-----------------------------------------------------------------------

      ELSE IF (DSPLAY .AND. F.EQ.0) THEN                                        
        CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)                   
        IF (X.LE.7 .AND. LASTX.GT.7) PRINT *,SPACES                             
                                                                               
        IN=1+2*NESTLE                                                           
        IF (IASSOC.GT.0.AND.X.NE.31) PRINT *,SPACES(1:1+IN),'Q/C FIELD'         
        PRINT *,SPACES(1:IN),NAME(1:60-IN), UNITS(1:12), SCALE+ISCALE           
      ENDIF                                                                     


!-----------------------------------------------------------------------
! MOVE PAST THIS DESCRIPTOR & LOOP IF THERE ARE ANY LEFT.                      
! DROP THE NESTING LEVEL IF A COUNT HAS REACHED ZERO.     (N.B. NESTED         
! REPLICATIONS CAN END AT THE SAME POINT, SO THE LEVEL MAY DROP BY >1)         
!-----------------------------------------------------------------------

      NEST(NESTLE)=NEST(NESTLE)-1                                               
  500 CONTINUE
      IF (NEST(NESTLE).LE.0) THEN                                               
        NESTLE=NESTLE-1                                                         
        IF (NESTLE.GT.0) GO TO 500                                              
      ENDIF                                                                     
      IF (F.NE.3) N=N+1                                                         
      IF (F.EQ.0) LASTX=X                                                       
      IF (N.LE.ND) GO TO 1                                                      

      RETURN                                                                    
      END                                                                       
