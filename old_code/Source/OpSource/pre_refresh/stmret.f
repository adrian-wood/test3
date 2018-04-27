      SUBROUTINE STMRET(WMOLST,ICAOLST,DCNNLST,RAINLST,IDTYPE,USERPOS,  
     &                  STNTYPE,ISTAT,RARRAY,NOBS,NELEM,ARRAY1,CSTR,
     &                  CERROR)    

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : STMRET                                               
!                                                                      
! PURPOSE       : PROGRAM TO RETRIEVE STATION MASTER DETAILS THROUGH MDB                 
!                                                                      
! DESCRIPTION   : THIS PROGRAM SETS UP A CALL TO STNRET WHICH          
!                 LOCATES RECORD IN THE STATION MASTER DATASET.        
!                 THE REQUIRED DETAILS ARE THEN OUTPUT                 
!                                                                      
! CALLED BY     : MDB
!                                                                      
! CALLS         : STNRET                                               
!               : STNARY                                               
!                                                                      
! PARAMETERS    :                                                      
!                                                                      
!Y2K  26.06.1997  STMRET IS YEAR 2000 COMPLIANT.                        
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:32$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stmret.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:32    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:13  usmdb
! Removed unused variable BLKSTN. Removed variables
! NUM and RECNO from call to STNARY as they are unused.
! Added copyright and modified header - S.Cox
!
! Revision 1.3  98/10/21  11:11:08  11:11:08  usmdb (Generic MDB account
! Repeat of 1.2
! 
! Revision 1.1  97/08/06  08:49:14  08:49:14  uspm (Pat McCormack)
! Initial revision
!
! 21NOV95 : CHANGE NAME FROM STNRT TO STMRET                         
!                                                                      
! 05APR94 : CORRECT 'LATEST' RETRIEVAL TO ENABLE THE RETRIEVAL OF    
!           A RECORD FLAGGED AS STATION CLOSED                      
!                                                                      
! 17JAN94 : ENABLE RETRIEVAL OF SURFACE RECORD, UPPER AIR OR BOTH    
!                                                                      
! MAY 92  : FIRST VERSION OF SOFTWARE                                                                   
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

      REAL RARRAY(NOBS,NELEM)                                           
      INTEGER NOBS,NELEM,ISTAT,ARRAY1(*)                                
      CHARACTER*(*) CSTR(*)                                             
      CHARACTER*48  CERROR                                              
      CHARACTER*132 HEAD
*                                                                       
      INTEGER WMOLST(50),DCNNLST(50),RAINLST(50),IDTYPE             !2.0
      INTEGER NOBCON,STNTYPE                                            
      CHARACTER*1 USERPOS,POS                                           
      CHARACTER*4 ICAOLST(50)                                           
*                                                                       
      INTEGER RECNO,BLK,STN,DCNN,RAIN,REGION,LAT1,LAT2                  
      INTEGER LONG1,LONG2,HT1,HT2,CLASS,HAW00,HAW12                     
      INTEGER OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR           
      INTEGER MAP,RUN1,RUN2,OREC,NREC,NUM                               
      INTEGER NCMTIM1(10),NCMTIM2(10),SQUARE                            
      CHARACTER*4 ICAO,UA                                               
      CHARACTER*6 CHASER                                                
      CHARACTER*24 FLAGS                                                
      CHARACTER*24 SYNTIM(10)                                           
      CHARACTER*13 SYN                                                  
      CHARACTER*23 NCM                                                  
      CHARACTER*9 DAY(10)                                               
      CHARACTER*42 STATION                                              
      CHARACTER*32 COUNTRY                                              
      CHARACTER*1 LATP,LONGP                                            
*                                                                       
      LOGICAL STNOK                                                     

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/stmret.F,v $
     &'//'$ $Date: 30/01/2006 20:24:32$ $Revision: 1$'
*                                                                       
*                                                                       
* CHECK THE STATUS OF ISTAT BEFORE STARTING/CONTINUING THE RETRIEVAL    
*                                                                       
      IF(ISTAT.EQ.0)THEN                                                
        NOBCON=1                                                        
      ELSEIF(ISTAT.GT.4)THEN                                            
        CERROR='STATUS INCORRECT FOR RETRIEVAL'                         
        GOTO 999                                                        
      ENDIF                                                             
*                                                                       
      IF((ISTAT.EQ.4).AND.(NOBCON.GT.NOBS))THEN                         
         NOBCON=1                                                       
      ELSEIF((ISTAT.NE.4).AND.(NOBCON.GT.NOBS))THEN                     
         CERROR='PROBLEM WITH THE RETRIEVAL'                            
         GOTO 999                                                       
      ENDIF                                                             
*                                                                       
      DO 10 ILOOP1=1,NOBS                                               
        DO 20 ILOOP2=1,NELEM                                            
          RARRAY(ILOOP1,ILOOP2)=-9999999.0                              
  20    CONTINUE                                                        
  10  CONTINUE                                                          
*                                                                       
      POS = 'A'                                                         
*                                                                       
* CALL STNRET (TO LOCATE AND RETRIEVE CORRECT RECORD)                   
*                                                                       
 100  CALL STNRET(WMOLST,ICAOLST,DCNNLST,RAINLST,                       
     -  IDTYPE,POS,ISTAT,                                               
     -  BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,                              
     -  LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,           
     -  OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,                
     -  FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,                
     -  UA,SYN,NCM,NUM,                                                 
     -  DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,RECNO)                 
*                                                                       
      IF(ISTAT.EQ.8)THEN                                                
        CERROR='DATA REQUESTED NOT AVAILABLE'                           
        NOBS=0                                                      !1.2
        GOTO 999                                                        
      ELSE                                                              
        STNOK = .TRUE.                                                  
        IF (USERPOS.EQ.'L' .AND. FLAGS(9:9)  .EQ.'2'                    
     -                     .AND. NREC        .GT. 0 ) STNOK = .FALSE.   
        IF (STNTYPE.EQ. 1  .AND. FLAGS(15:15).EQ.'2') STNOK = .FALSE.   
        IF (STNTYPE.EQ. 2  .AND. FLAGS(15:15).LT.'2') STNOK = .FALSE.   
      ENDIF                                                             
*                                                                       
* PUT DATA INTO USERS ARRAY                                             
*                                                                       
      IF (STNOK)                                                        
     &   CALL STNARY(BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,                 
     &        LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,     
     &        OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,          
     &        FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,          
     &        UA,SYN,NCM,                                           !2.0
     &        DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,             !2.0
     &        RARRAY,NOBS,NELEM,ARRAY1,CSTR,NOBCON)                     
                                                                       
      IF((NOBCON.GT.NOBS).AND.(ISTAT.EQ.4))THEN                         
          ISTAT=4                                                       
      ELSEIF((NOBCON.LE.NOBS).AND.(ISTAT.EQ.4))THEN                     
          GOTO 100                                                      
      ELSE                                                              
          ISTAT=0                                                       
          NOBS=NOBCON-1                                                 
      ENDIF                                                             
*                                                                       
 999  RETURN                                                            
      END                                                               
