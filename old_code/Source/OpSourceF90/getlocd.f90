SUBROUTINE GETLOCD(SUBTYPE)                                     !A              
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! ROUTINE       : GETLOCD                                                       
!                                                                               
! PURPOSE       : To read in local sequence descriptors for a                   
!               : particular subtype and pass to LOCALD for                     
!               : use later in modules SSMRET & BUSRET. This is                 
!               : necessary if the storage datasets don't or haven't            
!               : always contained a local sequence descriptor in               
!               : record 2 e.g. SAT120.                                         
!                                                                               
! CALLED BY     : SSMRET, BUSRET                                                
!                                                                               
! CALLS         : LOCALD                                                        
!                                                                               
! ARGUMENTS     : SUBTYPE   i/p  : MetDB subtype                                
!                                                                               
! FILES         : unit 81   read local sequence datasets                        
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Revision: 1$                                                                 
! $Date: 26/01/2010 10:18:13$                                                   
! $Source: /home/us0400/mdb/op/lib/source/RCS/getlocd.F,v $                     
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $                                                                             
! Revision 2.1  2003/05/06 15:16:52  usmdb                                      
! 19 May 2003    C Long                                                         
! 2.1  List SFERICS too, to override old sequence.                              
!                                                                               
! Revision 2.0  2001/01/08  11:58:41  11:58:41  usmdb (MetDB account c/o usjh)  
! Removed READONLY from OPEN statement as it is an                              
! error under f90/95. Added copyright and modified                              
! header - S.Cox                                                                
!                                                                               
! Revision 1.7  98/01/29  17:16:42  17:16:42  usmdb (Generic MDB account)       
! Addition of IBM preprocess directive.                                         
!                                                                               
! Revision 1.6  97/09/10  15:23:45  15:23:45  uspm (Pat McCormack)              
! Added subtype RTOVS                                                           
!                                                                               
! Revision 1.5  1997/08/04 13:10:45  uspm                                       
! First revisioned version for COSMOS - with Y2K change                         
!                                                                               
! Revision 1.4  1997/04/07 12:12:46  uspm                                       
! Copy over version dated 4-3-97 from COSMOS. Insert                            
! 'extra' $ after $Source: to ensure Revision picked up by ident                
!                                                                               
! Revision 1.3  1997/03/19 13:39:45  uspm                                       
! Add ifdef around definition of local sequence filenames                       
! so no brackets used.                                                          
!                                                                               
! Revision 1.2  1997/02/27 12:12:14  uspm                                       
! Add #ifdef #endif around open statement                                       
!                                                                               
! Revision 1.1  1997/02/26 13:27:20  uspm                                       
! Initial revision                                                              
!                                                                               
! 15-09-97  !C  : Added subtype RTOVS - S.Cox                                   
!                                                                               
! 04-03-97  !B  : Added subtype BUOY/DRIFTR - S.Cox                             
!                                                                               
! 17-02-97  !A  : Now works for different subtypes. Dataset names,              
!                 hard-wired here though. Non-portable.                         
!                                                                               
! 28-01-97      : created S.Cox                                                 
!                                                                               
!-----------------------------------------------------------------------        
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.                   
!                                                                               
! Met Office, United Kingdom                                                    
!                                                                               
! The use, duplication and disclosure of this code is strictly                  
! prohibited without the permission of The Meteorological Database              
! Team at the above address.                                                    
!-----------------------------------------------------------------------        
                                                                                
IMPLICIT NONE                                                                   
                                                                                
!-----------------------------------------------------------------------        
! declare variables                                                             
!-----------------------------------------------------------------------        
                                                                                
INTEGER           DUMMY(1)   !- dummy integer array                             
INTEGER           ISET(5)    !- loc seq no. of datasets         !A              
INTEGER           I,J        !- general loop counters                           
INTEGER           ROW        !- ISET, DSET subtype pointer      !A              
                                                                                
CHARACTER*80      DSET(3,5)  !- loc seq dataset names           !A              
CHARACTER*132     HEAD                                                          
CHARACTER*27998   MDBLOCD    !- contains local sequence                         
CHARACTER*(*)     SUBTYPE    !- MetDB subtype                   !A              
                                                                                
!-----------------------------------------------------------------------        
! dynamic common                                                                
!-----------------------------------------------------------------------        
                                                                                
COMMON /GETLO1/MDBLOCD                                                          
                                                                                
!-----------------------------------------------------------------------        
! SAVE statement to ensure variables are still set on next call                 
!-----------------------------------------------------------------------        
                                                                                
SAVE                                                                            
                                                                                
!-----------------------------------------------------------------------        
! data statements                                                     !A        
!-----------------------------------------------------------------------        
                                                                                
 DATA ISET/&                                                                    
&  2,&          !- SAT120,RTOVS (2 local sequence datasets)    !2.1             
&  1,&          !- SFERICS (to override invalid sequence)      !2.1             
&  0,&          !- spare                                       !2.1             
&  0,&          !- spare                                       !2.1             
&  0/           !- spare                                       !2.1             
                                                                                
!if defined (MVS)                                                               
 DATA DSET/ &                                                                   
& '/SDB.BUFR.LOCALSEQ(TOVSEQ)','/SDB.BUFR.LOCALSEQ(TOVCFSEQ)',' ',&             
& '/SDB.BUFR.LOCALSEQ(SFERICS)',' ',' ',&                      !2.1             
& ' ',' ',' ',&                                                !2.1             
& ' ',' ',' ',&                                                !2.1             
& ' ',' ',' '/                                                 !2.1             
!else                                                                           
!endif                                                                          
                                                                                
 HEAD='$RCSfile: getlocd.F,v $ ' //&                                            
     &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'                               
                                                                                
!-----------------------------------------------------------------------        
! determine input subtype                                             !A        
!-----------------------------------------------------------------------        
                                                                                
ROW=0                                                                           
IF (SUBTYPE(1:6).EQ.'SAT120') ROW=1                                             
IF (SUBTYPE(1:5).EQ.'RTOVS')  ROW=1                           !2.1              
IF (SUBTYPE(1:7).EQ.'SFERICS') ROW=2                          !2.1              
                                                                                
!-----------------------------------------------------------------------        
! if the subtype is matched, loop over the sequence descriptor datasets         
! for that subtype (ISET(ROW)). Open the dataset and read in the                
! descriptors, putting them in string MDBLOCD. Close the dataset and            
! call LOCALD to put the sequence descriptor sequence in memory for use         
! later.                                                                        
!-----------------------------------------------------------------------        
                                                                                
IF (ROW.GT.0) THEN                                                              
                                                                                
  DO I=1,ISET(ROW)                                                              
                                                                                
    MDBLOCD(:)=' '                                                              
!if defined (MVS)                                                               
    OPEN(81,FILE=DSET(I,ROW),FORM='FORMATTED',ACTION='READ')                    
!else                                                                           
!endif                                                                          
                                                                                
          J=0                                                                   
 20       READ(81,'(A80)',END=21)MDBLOCD(J*80+1:J*80+80)                        
          J=J+1                                                                 
          GOTO 20                                                               
                                                                                
 21       CONTINUE                                                              
          CLOSE(81)                                                             
                                                                                
          MDBLOCD(J*80+1:J*80+5)=' END '                                        
                                                                                
! For SFERICS current sequence must override old, hence 'NEW'      !2.1         
                                                                                
    IF (ROW.EQ.2) THEN                     ! if SFERICS      !2.1               
      CALL LOCALD(0,0,DUMMY,0,MDBLOCD,'NEW')                 !2.1               
    ELSE                                                     !2.1               
      CALL LOCALD(0,0,DUMMY,0,MDBLOCD,'ADD')                                    
    ENDIF                                                    !2.1               
                                                                                
  ENDDO   !- i                                                                  
ENDIF   !- row.gt.0                                                             
                                                                                
RETURN                                                                          
END SUBROUTINE GETLOCD                                                          
