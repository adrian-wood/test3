      SUBROUTINE SAMBUL(POINT,BEND,TTAAII,CCCC,YYGGGG,                  
     &                  AMD,AMDNUB,COR,CORNUB,NFT,OERR,BULL)            
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SAMBUL                                               
!                                                                      
! PURPOSE       : TO FIND THE STARTS OF SAMOS REPORTS IN A BULLETIN    
!                                                                      
! CALLED BY     : MDBSTOR
!                                                                      
! CALLS         : NCHTST,TAFIND                                        
!                                                                      
! PARAMETERS    : (1) POINT    POINTER TO WHERE TO START IN BULLETIN   
!                 (2) BEND     NUMBER OF LAST CHARACTER IN BULLETIN    
!                 (3) TTAAII   TT TYPE OF BULLETIN (NCNC80 - SAMOS)    
!                 (4) CCCC     ORIGINATING CENTRE FOR BULLETIN         
!                 (5) YYGGGG   TIME OF BULLETIN                        
!                 (6) AMD      SET IF BULLETIN AMENDED                 
!                 (7) AMDNUB   NUMBER OF BULLETIN AMENDMENT            
!                 (8) COR      SET IF BULLETIN CORRECTED               
!                 (9) CORNUB   NUMBER OF BULLETIN CORRECTION           
!                (10) NFT      FT NUMBER FOR SAMOSX STORAGE            
!                (11) OERR     LOGICAL SET IF REPORT SPLITTING ERROR   
!                (12) BULL     REPORT DATA                             
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:05$
! $Source: /home/us0400/mdb/op/lib/source/RCS/sambul.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:05    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:46  usmdb
! Separated variable declaration and initialisation. Changed
! pre-processor statement to if (MVS), else.... Added copyright
! and modified header - S.Cox
!
! Revision 1.2  97/07/31  09:32:54  09:32:54  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:11:07  uspm
! Initial revision
!
! 03/06/96 (A.M)  REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY NEW     
!                 PARAMETER 'BULL'.                                    
!
! JAN 93 : MADE FROM TAFBUL                                           
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
                                                                        
      CHARACTER*(*) BULL                                                
      CHARACTER*6 YYGGGG,TTAAII                                         
      CHARACTER*4 CCCC,GGGG                                             
      CHARACTER*2 AMDNUB,CORNUB         ! FOR WHOLE BULLETIN            
      LOGICAL AMD,COR                                                   

      CHARACTER*1 CR,LF,SPACE
      CHARACTER*132 HEAD
      INTEGER BEND,POINT,RSTART, BLKSIZ                             !2.0
      LOGICAL OERR, OLFCR,ONUM,OSPACE                                   
      DATA SPACE/' '/
      DATA BLKSIZ/4240/                                             !2.0
! CR IS CARRIAGE RETURN,  LF IS LINE FEED                               
#if defined (MVS)
      DATA CR/Z15/,LF/Z25/
#else
      CR=CHAR(13)
      LF=CHAR(10)
#endif

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/sambul.F,v $
     &'//'$ $Date: 30/01/2006 20:24:05$ $Revision: 1$'
      
*********************************************************************** 
*                                                                       
* IGNORE CARRIAGE RETURNS, LINE FEEDS AND SPACES BEFORE NEXT GROUP      
*                                                                       
*********************************************************************** 
      GGGG=' '                                                          
   10 IF (POINT.GE.BEND) RETURN                                         
      IF (BULL(POINT:POINT).EQ.CR .OR. BULL(POINT:POINT).EQ.LF          
     -   .OR. BULL(POINT:POINT).EQ.SPACE) THEN                          
        POINT=POINT+1                                                   
        GO TO 10                                                        
      ENDIF                                                             
*********************************************************************** 
*                                                                       
* ACCEPT REPORT IF 5-FIGURE 03... STATION NUMBER, OR IF ONLY 3 FIGURES  
* (PUT 03 IN FRONT).  CALL TAFIND TO INDEX AND STORE.                   
*                                                                       
*********************************************************************** 
      CALL NCHTST(POINT,6,ONUM,INUM,OSPACE,OLFCR,BULL)                  
      IF ((INUM.EQ.6 .AND. BULL(POINT:POINT+1).EQ.'03' .AND. OSPACE)    
     -.OR.(INUM.EQ.4 .AND. OSPACE)) THEN                                
        IF (INUM.EQ.4) THEN                                             
          RSTART=POINT-2                                                
          BULL(RSTART:RSTART+1)='03'                                    
        ELSE                                                            
          RSTART=POINT                                                  
        ENDIF                                                           
        ICC=0                                                           
        CALL TAFIND(RSTART,BEND,AMD,COR,AMDNUB,CORNUB,TTAAII,           
     -              CCCC,YYGGGG,GGGG,ICC,NFT,BLKSIZ,OERR,BULL)          
        POINT=RSTART                                                    
      ELSE                                                              
        IQUAL=INDEX(BULL(POINT:BEND),'=')                               
        IF (IQUAL.EQ.0) RETURN                                          
        POINT=POINT+IQUAL                                               
      ENDIF                                                             
*                                                                       
* IF MORE DATA TO BE PROCESSED, GO BACK TO START                        
*                                                                       
      IF (POINT.LT.BEND) GO TO 10                                       
      RETURN                                                            
      END                                                               
