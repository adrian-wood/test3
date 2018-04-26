      SUBROUTINE TBUSBUL(POINT,BEND,TTAAII,YYGGGG,NFT,OERR,BULL)    !2.0
                                                                        
      IMPLICIT NONE                                                     
                                                                        
!-----------------------------------------------------------------------
!                                                                       
! PROGRAM       : TBUSBUL                                               
!                                                                       
! PURPOSE       : To pass a TBUS message to the TAF/METAR storage       
!                 program, preparing a pseudo-station-identifier and    
!                 starting an Index entry.                              
!                                                                       
! DESCRIPTION   : Unlike the majority of text messages from GTS, the    
!                 TBUS include "=" not as end-of-report indicators but  
!                 as simple character-group separators; we may not,     
!                 therefore, use use all the usual storage routines as  
!                 "black boxes".  I have combined the functions of      
!                 xxxBUL and TAFIND routines here as all the testing    
!                 for end-of-report and station identifiers may be      
!                 removed, - leaving nothing for TBUSBUL to do other    
!                 than call TBUSIND - - this would have been the only   
!                 data type handled by SDBSYN to require a separate     
!                 xxxIND routine !                                      
!                                                                       
!                 In most data types,reports are extracted from bulle-  
!                 tins and SYNOPT has set the start pointer in the      
!                 bulletin past the header.  I this case, the header    
!                 itself is an important part of the data to be stored  
!                 so reset the pointer there.                           
!                                                                       
! CALLED BY     : MDBSTOR
!                                                                       
! CALLS         : TAFREP, DATIM                                         
!                                                                       
! PARAMETERS    : (1) POINT    Pointer to where to start in bulletin    
!                 (2) BEND     Number of last character in bulletin     
!                 (3) TTAAII   TT type of bulletin (TBUSnn)             
!                 (4) YYGGGG   Time of bulletin                         
!                 (5) NFT      FT number for TBUS storage               
!                 (6) OERR     Logical set if report splitting error    
!                 (7) BULL     Report text                              
!                                                                       
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:17$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tbusbul.F,v $
!                                                                       
! CHANGE RECORD :                                                       
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:17    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:13  usmdb
! Removed byte counts on numeric data type declarations. Removed
! unused dummy argument CCCC, separated variable declaration and
! initialisation. Added copyright and modified header - S.Cox
!
! Revision 1.1  97/07/31  08:52:03  08:52:03  uspm (Pat McCormack)
! Initial revision
! 
! Created from TAFBUL and TAFIND, April 97 by Jim Arnott.              
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
                                                                        
!     Declare parameters.                                               
                                                                        
      CHARACTER*(*) BULL                                                
      CHARACTER*132 HEAD
                                                                        
      INTEGER      POINT , BEND                                     !2.0
      INTEGER      NFT   , ICC                                      !2.0
      LOGICAL      OERR                                             !2.0
      CHARACTER*6  TTAAII, YYGGGG                                       
                                                                        
!     Declare working variables                                         
                                                                        
      INTEGER  NOW(9)        ! Array to retrieve system date/time   !2.0
      INTEGER  DATIME(5)     ! Array for preparing index time.      !2.0
                                                                        
      CHARACTER*23 ENTRY          ! Initialise index entry.             
                                                                        
      INTEGER      BLKSIZ    ! ... of MDB.TBUS archive set.         !2.0

      DATA         BLKSIZ /27998/                                   !2.0

! *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   * 
! *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   * 
                                                                        
! Declare revision information
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tbusbul.F,v $
     &'//'$ $Date: 30/01/2006 20:25:17$ $Revision: 1$'

! Reset POINT to start of bulletin Id so full message is stored.        
                                                                        
      POINT=INDEX(BULL,'TBUS')                                          
                                                                        
! Set bulletin header and blank flags into index entry.                 
                                                                        
      ENTRY( 3:11)=TTAAII                                               
      ENTRY(17:17)=CHAR(0)                                              
                                                                        
! Set missing position in index entry                                   
                                                                        
      ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)               
                                                                        
!     Initialise variables to be passed to TAFREP.                      
!     NEITHER OF THESE IS USED WITH THIS DATA TYPE !                    
                                                                        
      OERR=.FALSE.                                                      
      ICC=0                                                             
                                                                        
! *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   * 
                                                                        
!     Pass message and associated info. on for storing in the database  
!                                                                       
!     Take current Year & Month, Day & time from bulletin               
                                                                        
      CALL DATIM(NOW)                           ! Current date          
                                                                        
      DATIME(1)=NOW(8)                          ! Year                  
      DATIME(2)=NOW(7)                          ! Month                 
                                                                        
      READ (YYGGGG(1:2),'(I2)') DATIME(3)       ! Day                   
      READ (YYGGGG(3:4),'(I2)') DATIME(4)       ! Hour                  
      READ (YYGGGG(5:6),'(I2)') DATIME(5)       ! Minute                
                                                                        
!     Check "YY" falls within valid day range                           
                                                                        
      IF (DATIME(3) .GE. 32) THEN                                       
        WRITE (6,*) 'BAD DAY GROUP (YY): ',YYGGGG(1:2),                 
     &                                      BULL(POINT:POINT+50)        
        GO TO 9                                                         
      ENDIF                                                             
                                                                        
!     Check "GGGG" falls within valied time range                       
                                                                        
      IF (DATIME(4) .GE. 24 .OR. DATIME(5) .GE. 60) THEN                
        PRINT *,'BAD TIME GROUP: ',YYGGGG(3:6),'   ',                   
     &                                      BULL(POINT:POINT+50)        
        GO TO 9                                                         
      ENDIF                                                             
                                                                        
!     Do not store out-of-date information (ie yesterday's data).       
                                                                        
      IF (NOW(6) .NE. DATIME(3)) GOTO 9                                 
                                                                        
! *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   * 
                                                                        
!     Repeat the Identifier sequence from ENTRY as the last arg.        
!      to ensure that bulletin header is stored in index.               
                                                                        
      CALL TAFREP (DATIME, ENTRY , BULL(POINT:BEND),                    
     -             NFT   , BLKSIZ, ENTRY(3:11))                         
                                                                        
  9   RETURN                                                            
      END                                                               
