      SUBROUTINE TOVSEQ(SEQDES,BULL,DIFF)                                       

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : TOVSEQ                                                       
!                                                                              
! PURPOSE       : TO SEE WHETHER A BUFR MESSAGE CAN BE RE-ENCODED              
!                 WITH A GIVEN SEQUENCE DESCRIPTOR                             
!                                                                              
! DESCRIPTION   : THE AIMS ARE (1) TO RE-ENCODE WITH ONE SEQUENCE              
!                 DESCRIPTOR DATA RECEIVED WITH A LONG SECTION 3               
!                 AND (2) TO CORRECT THE CLASS OF PRESSURES/HEIGHTS            
!                 CODED AS CLASS 7 INSTEAD OF 10 OR VICE VERSA BY              
!                 USING THAT SEQUENCE.                                         
!                 EQUIVALENCE IN OTHER SENSES (REPLICATION EXPANDED            
!                 OUT, F=2 OPERATIONS IN A DIFFERENT ORDER) WON'T              
!                 BE RECOGNISED.                                               
!                                                                              
! CALLED BY     : TOVREP                                                       
!                                                                              
! PARAMETERS    : (1) SEQUENCE DESCRIPTOR                                      
!                 (2) BUFR MESSAGE (STARTING AT 'BUFR')                        
!                 (3) FLAG SET ON RETURN IF SEQUENCE NOT EQUIVALENT            
!                                         TO DESCRIPTORS IN MESSAGE            
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 26/11/2007 11:11:02$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tovseq.F,v $
!                                                                              
! CHANGE RECORD :                                                              
!                                                                              
! $Log:
!  2    Met_DB_Project 1.1         26/11/2007 11:11:02    Brian Barwell
!       Obsolete routine used for SAT120 storage (removed from MDBSTOR load
!       module on 19 November 2007).
!  1    Met_DB_Project 1.0         30/01/2006 20:25:27    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:22  usmdb
! Changed DO label ... label blocks to DO ... ENDDO blocks. Changed
! preprocessor statement to if (MVS), else.... Added copyright and
! modified header - S.Cox
!
! Revision 1.4  98/01/29  11:45:54  11:45:54  usmdb (Generic MetDB account)
! Addition of IBM preprocess directive.
! 
! Revision 1.3  97/09/22  14:02:36  14:02:36  uspm (Pat McCormack)
! Put DATA ASCBUF into preprocessor check
! 
! Revision 1.2  1997/07/31 11:44:23  uspm
! First revision for MVS
!
! Revision 1.1  1997/07/04 14:35:32  uspm
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      INTEGER SEQDES, SEQ(16), DESCR(99), F,X,Y                                 
      CHARACTER BULL*(*), ASCBUF*4                                              
      CHARACTER*132 HEAD
      LOGICAL DIFF
#if defined (MVS)
      DATA ASCBUF/Z42554652/
#else
      DATA ASCBUF/'BUFR'/
#endif

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tovseq.F,v $
     &'//'$ $Date: 26/11/2007 11:11:02$ $Revision: 2$'
      
*                                                                               
* FIRST FIND THE DESCRIPTORS IN THE MESSAGE                                     
*                                                                               
*                                 SKIP SECTION 1 (ANY OPTIONAL SECTION?)        
      N=INDEX(BULL,ASCBUF)+4                                                    
      L1=ICHAR(BULL(N+1:N+1))*256+ICHAR(BULL(N+2:N+2))                          
      FLOPT=ICHAR(BULL(N+7:N+7))                                                
      N=N+L1                                                                    
*                                 IF THERE'S A SECTION 2, SKIP IT.              
      IF (FLOPT.GE.128) THEN                                                    
        L2=ICHAR(BULL(N+1:N+1))*256+ICHAR(BULL(N+2:N+2))                        
        N=N+L2                                                                  
      ENDIF                                                                     
*                                 FIND NO. OF REPORTS & COMPRESSION FLAG        
      L3=ICHAR(BULL(N+1:N+1))*256+ICHAR(BULL(N+2:N+2))                          
      ND=(L3-7)/2                                                               
*                                                                               
* IF THERE IS ONLY ONE DESCRIPTOR, CHECK THAT IT'S THE REQUIRED SEQUENCE        
*                                                                               
      DIFF=.FALSE.                                                              
      IF (ND.EQ.1) THEN                                                         
        DESCR(1)=ICHAR(BULL(N+7:N+7))*256+ICHAR(BULL(N+8:N+8))                  
        IF (DESCR(1).NE.SEQDES) DIFF=.TRUE.                                     
        RETURN                                                                  
      ENDIF                                                                     
*                                                                               
* IF MORE THAN ONE, EXPAND ALL TABLE D REFERENCES IN THE GIVEN SEQUENCE         
*                                                                               
      NX=1                                                                      
      J=1                                                                       
      DESCR(1)=SEQDES                                                           
   10 CALL DESFXY(DESCR(J),F,X,Y)                                               
      IF (F.EQ.3) THEN                                                          
        CALL TABLED(X,Y,SEQ,NSEQ)                                               
        IFXY=300000+X*1000+Y                                                    
        IF (NSEQ.LE.1) PRINT *,IFXY,'NOT IN TABLE D'                            
        DO I=NX,J+1,-1                                              !2.0
          DESCR(I+NSEQ-1)=DESCR(I)                                               
        ENDDO                                                       !2.0
        DO I=1,NSEQ                                                 !2.0
          DESCR(J+I-1)=SEQ(I)                                                    
        ENDDO                                                       !2.0
        NX=NX+NSEQ-1                                                            
      ELSE                                                                      
        J=J+1                                                                   
      ENDIF                                                                     
      IF (J.LE.NX) GO TO 10                                                     
*                                                                               
* AND COMPARE THE EXPANDED SEQUENCE WITH THE DESCRIPTORS IN THE MESSAGE         
*                                                                               
      IF (NX.NE.ND) THEN                                                        
        DIFF=.TRUE.                                                             
        PRINT *,'DIFFERENT NUMBERS OF DESCRIPTORS:'                             
        PRINT *,ND,'IN MESSAGE',NX,'IN EXPANSION OF TARGET SEQUENCE'            
        RETURN                                                                  
      ENDIF                                                                     
*                                                                               
      DO 20 I=1,ND                                                              
       ID=N+7+(I-1)*2                                                           
       MF=ICHAR(BULL(ID:ID))/64                                                 
       MX=ICHAR(BULL(ID:ID))-MF*64                                              
       MY=ICHAR(BULL(ID+1:ID+1))                                                
       CALL DESFXY(DESCR(I),F,X,Y)                                              
*                                                                               
       IF (F.NE.MF) DIFF=.TRUE.                                                 
       IF (Y.NE.MY) DIFF=.TRUE.                                                 
       IF (X.NE.MX) THEN                                                        
         IF (X.EQ.7.AND.MX.NE.10 .OR. X.EQ.10.AND.MX.NE.7) DIFF=.TRUE.          
       ENDIF                                                                    
*                                                                               
       IF (DIFF) THEN                                                           
         PRINT *,I,'TH DESCRIPTOR DIFFERENT:'                                   
         IFXY=F*100000+X*1000+Y                                                 
         MFXY=MF*100000+MX*1000+MY                                              
         PRINT *,MFXY,'IN MESSAGE',IFXY,'IN TARGET SEQUENCE'                    
         RETURN                                                                 
       ENDIF                                                                    
   20 CONTINUE                                                                  
*                                                                               
      RETURN                                                                    
      END                                                                       
