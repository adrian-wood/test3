      SUBROUTINE STNRET(WMOLST,ICAOLST,DCNNLST,RAINLST,                 
     &    IDTYPE,POS,ISTAT,                                             
     &    BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,                            
     &    LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,         
     &    OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,              
     &    FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,              
     &    UA,SYN,NCM,NUM,                                               
     &    DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,RECNO)               

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : STNRET IN STNRT                                      
!                                                                      
! PURPOSE       : CALL STNINDX TO FIND RECORD NUMBER FOR STATION       
!                 DETAILS AND STNFIN TO READ RECORD.                   
!                                                                      
! CALLED BY     : STNRT                                                
!                                                                      
! CALLS         : STNINDX & STNFIN IN STNRT                            
!                                                                      
! PARAMETERS    : (1-6) ARGUMENTS FOR STNINDX                          
!                 (7) RETURN CODE                                      
!                 (8-51) ARGUMENTS FOR STNFIN                          
!                                                                      
!Y2K  26.06.1997  STNRET IS YEAR 2000 COMPLIANT.                        
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:39$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stnret.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:39    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:16  usmdb
! Removed unused variables I and BLKSTN. Removed READONLY from
! default pre-processor OPEN statements as it is non-standard
! in f90/95. Added copyright and modified header - S.Cox
!
! Revision 1.3  97/08/07  15:39:11  15:39:11  uspm (Pat McCormack)
! Latest version from COSMOS with changes date 18/8/97
! 
! Revision 1.2  1997/08/04 16:26:24  uspm
! Add preprocessor checks on O/S around OPEN statements
!
! Revision 1.1  1997/08/04 13:52:07  uspm
! Initial revision
!
! 18/8/97   ADD LAST ARGUMENT TO STNINDX CALL - WAS INCONSISTENT   !A 
!                                                                      
! 26/9/94   CHANGE OPEN OF MAIN DATASET TO READ ONLY TO ENSURE       
!           MONDAY EVENING UPDATE RUN NOT BLOCKED BY USER USE.       
!                                                                      
! 17/1/94   CHANGE UNIT NUMBER FROM 15 TO 88.                        
! 23/10/92  CORRECT INITIALISATION OF IAGAIN AND IJK FOR A RETURN    
!           INTO MDB WITH A NEW REQUEST WITHIN THE SAME JOB.         
! 16/10/92  CHANGE OPEN OF INDEX DATASET TO READ ONLY(RACF).         
! MAY 92    FIRST VERSION OF SOFTWARE  
!                       
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      INTEGER WMOLST(50),DCNNLST(50),RAINLST(50)                        
      CHARACTER*4 ICAOLST(50)                                           
      CHARACTER*1 POS                                                   
      INTEGER ISTAT,INDREC2                                         !2.0
      INTEGER INDREC(1000),INDWMO(1000)                                 
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
      CHARACTER*132 HEAD
*                                                                       
      INTEGER IJK                                                   !2.0
*                                                                       
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/stnret.F,v $
     &'//'$ $Date: 30/01/2006 20:24:39$ $Revision: 1$'
*                                                                       
* RESET IAGAIN AND IJK IF ROUTINE REENTERED WITH NEW REQUEST            
*                                                                       
      IF (ISTAT.EQ.0) THEN                                              
        IAGAIN=0                                                        
        IJK=1                                                           
      ENDIF                                                             
*                                                                       
* DON'T OPEN AGAIN UNLESS NEW REQUEST                                   
*                                                                       
      IF (IAGAIN.EQ.0) THEN                                             
#if defined (MVS)
        OPEN(88,ACCESS='DIRECT',ACTION='READ',                          
     -    FORM='FORMATTED',RECL=692)                                    
        OPEN(89,FILE='STNIND',ACCESS='DIRECT',ACTION='READ',            
     -    FORM='FORMATTED',RECL=692)                                    
#else
        OPEN(88,ACCESS='DIRECT',FORM='FORMATTED',RECL=692)          !2.0
        OPEN(89,FILE='STNIND',ACCESS='DIRECT',                      !2.0
     &          FORM='FORMATTED',RECL=692)                          !2.0      
#endif
        IAGAIN=1                                                        
*                                                                       
* IF NEW REQUEST, CALL INDEX SUBROUTINE TO FIND STATION RECORD NUMBERS  
*                                                                       
        CALL STNINDX(WMOLST,ICAOLST,DCNNLST,RAINLST,                    
     -               IDTYPE,POS,ISTAT,INDREC,INDWMO)                  !A
      ENDIF                                                             
*                                                                       
* GET RECORD NUMBER FROM LIST RETURNED BY STNINDX & READ STATION DETAILS
*                                                                       
      IF (ISTAT.LE.4) THEN                                              
        INDREC2=INDREC(IJK)                                             
        IF (INDREC2.LE.0) THEN                                          
          ISTAT=16                                                      
          GOTO 999                                                      
        ENDIF                                                           
*                                                                       
        CALL STNFIN(INDREC2,BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,          
     -    LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,         
     -    OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,              
     -    FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,              
     -    UA,SYN,NCM,NUM,                                               
     -    DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,RECNO)               
      ENDIF                                                             
*                                                                       
* RETURN REQUIRED STATION DETAILS, SETTING RETURN CODE TO "NO MORE DATA"
* IF NEXT RECORD NUMBER IN LIST IS ZERO.                                
*                                                                       
      IF (ISTAT.EQ.4) THEN                                              
        IJK=IJK+1                                                       
        IF (INDREC(IJK).EQ.0) THEN                                      
          ISTAT=0                                                       
        ENDIF                                                           
      ENDIF                                                             
*                                                                       
  999 RETURN                                                            
      END                                                               
