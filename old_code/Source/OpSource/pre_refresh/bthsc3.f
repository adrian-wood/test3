      SUBROUTINE BTHSC3(REPORT,REPLEN,EXPARR,TOTDEP,POS,POS1A)      !2.0

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : BTHSC3                                                
!                                                                      
! PURPOSE       : TO EXPAND SECTION 3 OF BATHY                          
!                 (SEA DEPTH & CURRENT)                                 
!                                                                      
! CALLED BY     : BTHEXP                                                
!                                                                      
! CALLS         : IVALUE   CONVERTS FROM CHAR TO NUMERIC TYPE           
!                                                                      
! PARAMETERS    : REPORT   CHARACTER STRING OF REPORT (I)               
!                 REPLEN   LENGTH OF REPORT  (I)                        
!                 EXPARR   EXPANSION ARRAY   (O)                        
!                 TOTDEP   TOTAL WATER DEPTH (I)                        
!                 POS      SECTION LOCATING VARIABLE (I/O)              
!                 POS1A    END OF REPLICATED DATA MARKER                
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:08$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bthsc3.F,v $
!                                                                      
! CHANGE RECORD :                                                     
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:08    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:36  usmdb
! Removed unused dummy argument IERR. Separated variable
! declaration and initialisation. Added copyright and modified
! header - S.Cox
!
! Revision 1.2  97/07/31  09:13:58  09:13:58  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 11:09:49  uspm
! Initial revision
!
! JAN 96 - CURRENT DIRECTION IS IN TENS OF DEGREES,                 
!          CURRENT SPEED IN TENTHS OF KNOTS!                          !A
!                                                                      
! INTRODUCED 11/07/94                                              
!                                                                      
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        
                                                                        
      IMPLICIT NONE                                                     
                                                                        
      CHARACTER*4096 REPORT                                             
      CHARACTER*132  HEAD
      REAL           EXPARR(0:300)                                      
      REAL           MISING                                         !2.0
      REAL           TOTDEP                                             
      REAL           CDIR,CSPEED                                        
      REAL           KT2MPS                                       !2.0!A
      INTEGER        REPLEN                                             
      INTEGER        POS,POS1A                                          
      INTEGER        IVALUE                                             
      INTEGER        K5                                                 

      DATA           MISING/-9999999./                              !2.0
      DATA           KT2MPS/0.5148/                                 !2.0
                                                                        
*************************************************************           
*                                                                       
*     INITIALISE VARIABLES                                              
*     K5 - INDICATOR FOR THE METHOD OF CURRENT MEASUREMENT              
*     CDIR - SURFACE CURRENT DIRECTION                                  
*     CSPEED - SURFACE CURRENT SPEED                                    
*                                                                       
*************************************************************           
                                                                        
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bthsc3.F,v $
     &'//'$ $Date: 30/01/2006 20:21:08$ $Revision: 1$'
      
      K5 = MISING                                                       
      CDIR = MISING                                                     
      CSPEED = MISING                                                   
                                                                        
*************************************************************           
*                                                                       
*     CHECK IF SECTION BEGINS '66666'                                   
*     IF NOT THIS IS NOT SECTION 3                                      
*                                                                       
*************************************************************           
                                                                        
      IF (REPORT(POS:POS+4).NE.'66666') GOTO 999                        
                                                                        
*     NEXT GROUP - CHECK REPORT LENGTH HAS NOT BEEN EXCEEDED            
      POS = POS + 6                                                     
      IF (POS.GT.REPLEN) GOTO 999                                       
                                                                        
*************************************************************           
*                                                                       
*     GROUPS WITHIN SECTION 3 ARE OPTIONAL                              
*     CHECK IF NEXT GROUP IS 1ZDZDZDZD  - SEA DEPTH GROUP               
*     THIS GROUP AND THE (00000) GROUP OF SECTION 2                     
*     ARE MUTUALLY EXCLUSIVE                                            
*     IF THIS GROUP HAS NOT BEEN REPORTED THEN THE TOTAL                
*     DEPTH EXTRACTED FROM SECTION 2 WILL BE STORED                     
*                                                                       
*************************************************************           
                                                                        
      IF (REPORT(POS:POS).EQ.'1') THEN                                  
        TOTDEP = IVALUE(REPORT(POS+1:POS+4))                            
*       NEXT GROUP                                                      
        POS = POS + 6                                                   
        IF (POS.GT.REPLEN) GOTO 999                                     
      ENDIF                                                             
                                                                        
*************************************************************           
*                                                                       
*     CHECK IF CURRENT GROUP EXISTS - K5DCDCVCVC                        
*     K5 TAKES VALUES 2,3,4                                             
*     CURRENT DIRECTION IS IN TENS OF DEGREES                           
*     CURRENT SPEED IS IN TENTHS OF KNOTS: CONVERT TO M/S.              
*                                                                       
*************************************************************           
                                                                        
*     CHECK IF THIS GROUP IS NOT CALLSIGN                               
      IF ((POS+9).GE.REPLEN) GOTO 999                                   
      K5 = IVALUE(REPORT(POS:POS))                                      
      IF (K5.EQ.2.OR.K5.EQ.3.OR.K5.EQ.4) THEN                           
        CDIR = IVALUE(REPORT(POS+1:POS+2))                              
        IF (CDIR.NE.MISING) CDIR=CDIR*10.                           !A  
        CSPEED = IVALUE(REPORT(POS+3:POS+4))                            
        IF (CSPEED.NE.MISING) CSPEED=CSPEED*0.1*KT2MPS              !A  
*       NEXT GROUP                                                      
        POS = POS + 6                                                   
        IF (POS.GT.REPLEN) GOTO 999                                     
      ENDIF                                                             
                                                                        
999   CONTINUE                                                          
                                                                        
*************************************************************           
*                                                                       
*     ASSIGN VARIABLES TO ARRAY                                         
*                                                                       
*************************************************************           
                                                                        
      EXPARR(POS1A+2) = TOTDEP                                          
      EXPARR(POS1A+4) = K5                                              
      EXPARR(POS1A+6) = CDIR                                            
      EXPARR(POS1A+8) = CSPEED                                          
                                                                        
      RETURN                                                            
      END                                                               
