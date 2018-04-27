!+    Copy selected records from current Stationmaster Log to a new log
                                                                       
      PROGRAM NEWLOG                                                   
                                                                        
      IMPLICIT NONE                                                    
                                                                       
!     Purpose                                                          
!     Copy records from existing Statinmaster Log into a new Log.       
                                                                       
!     Allows creation of a new log, when the old log has become full,  
!     to hold only the most recent log entries (eg current year) and   
!     plenty of blank records thereafter for updates.                  
!     Stationmaster Log is updated as Direct Access and may have been  
!     created with a maximum number of records, which has now been     
!     reached.  A replacement is being created with N blank records.   
!     Estimate N=15210 for a data set size 13 cylinders.               
                                                                       
!     Method                                                           
!     Copy selected records from old log to new log.                 
!     Options:-                                                      
!       a) copy a single record to same position in new log;           
!       b) copy a single record to a different position in new log;     
!       c) copy a range of records to a specified starting position    
!          in new log.                                                 
                                                                      
!     SOURCE: LIBRARY: MCC3.RLLIB.FORT    MEMBER: NEWLOG                
                                                                       
!     EXTERNAL MODULES: None                                            
                                                                        
!      Vsn  Date      Comment                                           
                                                                        
!      1.0  Oct 2006  Original version                     (RJ Lavery)  
                                                                        
!-----------------------------------------------------------------      
!                                                                      
! REVISION INFO :                                                       
!                                                                       
! $Revision: 1$                                                       
! $Date: 27/10/2006 17:34:28$                                           
! $Author: Stan Kellett$
! $Folder: UTILITY.SRCE$                                 
! $Workfile: stnlog.f$
!                                                                       
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         27/10/2006 17:34:28    Stan Kellett    
! $                                                              
!---------------------------------------------                          
! (C) CROWN COPYRIGHT 2006 - MET OFFICE.                               
! All Rights Reserved.                                                 
!                                                                    
! Met Office, United Kingdom                                          
!                                                                   
! The use, duplication and disclosure of this                          
! code is strictly prohibited without the                             
! permission of the Meteorological Database                            
! Team at the above address.                                          
!                                                                     
----------------------------------------------                        
                                                                   
!     Local Parameters                                                 
      INTEGER UIN                     ! Input unit for user request  
      INTEGER UMG                     ! Output unit for messages     
      INTEGER ULG                     ! Input unit for old log       
      INTEGER UNL                     ! Output unit for new log      
      PARAMETER (UIN=05)                                              
      PARAMETER (UMG=06)                                             
      PARAMETER (ULG=13)                                               
      PARAMETER (UNL=14)                                              
                                                                        
                                                                       
!     Local Scalars                                                     
                                                                        
      INTEGER N1                  ! First record to be copied           
      INTEGER N2                  ! Last record to be copied            
      INTEGER NDEST               ! First destination record (optional) 
      INTEGER MXDEST              ! Greatest rec number to be written   
      INTEGER NXTREC              ! Points to next rec for Update       
      CHARACTER*80 CLOG           ! Single record                       
      CHARACTER*80 CHEAD          ! Header 1                            
                                                                        
      INTEGER IOS,IOS1,IOS2       ! i/o status                          
      INTEGER IR                  ! Loop counter = rec read             
      INTEGER JR                  ! Temp store   = rec written          
                                                                        
! Revision info                                                         
      LOGICAL HEADSET                                                   
      CHARACTER*132 HEAD                                                
      DATA HEADSET/.FALSE./                                             
                                                                        
      IF (.NOT.HEADSET) THEN                                            
        HEAD='$Workfile: stnlog.f$ ' //                                 
     & '$Revision: 1$ $Date: 27/10/2006 17:34:28$'                      
        HEADSET=.TRUE.                                                  
      ENDIF                                                             
! --- End of Header --------------------------------------------------- 
                                                                        
! Display version                                                       
      WRITE(UMG,'('' NEWLOG  Version 1.0  Oct 2006''/)')                
                                                                        
                                                                        
! Open data sets for direct access                                     
                                                                      
      OPEN(13,ACTION='READ',ACCESS='DIRECT',FORM='FORMATTED',          
     & RECL=80,IOSTAT=IOS1)                                           
                                                                     
      OPEN(14,ACTION='WRITE',ACCESS='DIRECT',FORM='FORMATTED',        
     & RECL=80,IOSTAT=IOS2)                                            
                                                                       
! Copy selected records across to new log.                              
                                                                        
      MXDEST=0                                                         
      IOS=0                                                             
                                                                        
      DO WHILE (IOS.EQ.0)                                               
                                                                        
! Read next record number to be copied and its destination.             
! Blank entry should register as zero integer value.                    
!   N2 is blank means copy single record N1 to new position NDEST;      
!   NDEST is blank means copy to same position(s) as in existing log.   
! Repeat the above for each instruction card.                           
                                                                        
         READ(UIN,'(3I6)',IOSTAT=IOS) N1,N2,NDEST                       
                                                                        
         IF(IOS.EQ.0) THEN                                             
                                                                       
           IF(NDEST.EQ.0) NDEST=N1                                      
                                                                        
! Copy single record                                                    
           IF(N2.EQ.0) THEN                                             
                                                                        
             READ(ULG,'(A80)',REC=N1,IOSTAT=IOS1) CLOG                 
                                                                        
             IF(IOS1.NE.0) THEN                                         
               WRITE(UMG,'('' Error reading current Log'')')            
               STOP 99                                                 
             END IF                                                     
                                                                        
             WRITE(UNL,'(A80)',REC=NDEST,IOSTAT=IOS2) CLOG              
                                                                       
             IF(IOS2.NE.0) THEN                                        
               WRITE(UMG,'('' Error copying to new Log'')')            
               STOP 99                                                 
             END IF                                                    
                                                                        
! Copy range of records                                                 
           ELSE                                                         
                                                                        
             IF(N2.LT.N1) THEN                                          
               WRITE(UMG,'('' Invalid range of records specified'',I6,  
     &          '' -'',I6,''instruction ignored'')') N1,N2              
             ELSE                                                       
                                                                        
               DO IR=N1,N2                                            
                                                                        
                  JR=NDEST + IR-N1                                     
                                                                      
                  READ(ULG,'(A80)',REC=IR,IOSTAT=IOS1) CLOG           
                                                                       
                  IF(IOS1.NE.0) THEN                                   
                    WRITE(UMG,'('' Error reading current Log'')')       
                    STOP 99                                            
                  END IF                                                
                                                                       
                  WRITE(UNL,'(A80)',REC=JR,IOSTAT=IOS2) CLOG            
                                                                       
                  IF(IOS2.NE.0) THEN                                   
                    WRITE(UMG,'('' Error copying to new Log'')')        
                    STOP 99                                             
                  END IF                                                
                                                                      
               END DO                                                  
                                                                       
             END IF                                                    
!     (End test for valid range of records)                           
                                                                      
           END IF                                                     
!     (End test for single record/ range)                             
                                                                     
         ELSE IF(IOS.GT.0) THEN                                     
           WRITE(UMG,'('' Error reading instruction cards'')')    
           STOP 05                                                
         END IF                                                   
                                                                  
!     Save greatest rec number written to new log.                
         IF(JR.GT.MXDEST) MXDEST=JR                                
                                                                   
!     Next instruction card                                         
      END DO                                                        
                                                                    
!     ---------------  End of Copying -----------                    
                                                                     
!     Finally overwrite the next record for update into header 1,    
!     ie. next rec after the greatest rec number written in new log.  
                                                                       
      NXTREC=MXDEST+1                                                  
      READ(ULG,'(A80)',REC=1,IOSTAT=IOS1) CHEAD                        
                                                                       
      IF(IOS1.NE.0) THEN                                               
        WRITE(UMG,'('' Error reading header'')')                       
        STOP 99                                                         
      END IF                                                            
                                                                        
      WRITE(CHEAD(29:33),'(I5)') NXTREC                                 
      WRITE(UNL,'(A80)',REC=1,IOSTAT=IOS2) CHEAD                        
                                                                        
      IF(IOS2.NE.0) THEN                                                
        WRITE(UMG,'('' Error updating header'')')                       
        STOP 99                                                         
      END IF                                                            
                                                                        
      STOP                                                              
      END                                                               
