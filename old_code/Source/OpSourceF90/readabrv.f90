SUBROUTINE READABRV(Stn,Name)                                                   
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! ROUTINE       : READABRV                                                      
!                                                                               
! PURPOSE       : To read the abbreviated station list into memory on           
!               : first call only, and then to match an input WMO               
!               : station number with a WMO station name.                       
!                                                                               
! CALLED BY     : SYNRET                                                        
!                                                                               
! CALLS         : METDB_GETENV - MDBUX (Unix MetDB) only            !2.1        
!                                                                               
! ARGUMENTS     : Stn      (ip) - char*5 WMO station number                     
!               : Name     (op) - char*42 WMO station name                      
!                                                                               
! REVISION INFO :                                                               
!                                                                               
! $Revision: 1$                                                                 
! $Date: 26/01/2010 10:18:13$                                                   
! $Source: /home/us0400/mdb/op/lib/source/RCS/readabrv.F,v $                    
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $                                                                             
! Revision 2.2  2005/05/19 14:40:34  usmdb                                      
! Stan Kellett. CHG013565 and INC154615.                                        
! Increase Max number of stations to 16000 and put in check.                    
!                                                                               
! Revision 2.1  2003/02/03  15:22:47  15:22:47  usmdb (MetDB account c/o John C 
! Ward)                                                                         
! It is now possible to pass the location of the abrreviated                    
! station list in the environment variable METDB_STNABRV - S.Cox                
!                                                                               
! Revision 2.0  2001/01/08  11:59:06  11:59:06  usmdb (MetDB account c/o usjh)  
! Removed READONLY from pre-processor statement as it is                        
! non-standard in f90/f95. Removed TAB, added copyright                         
! and modified header - S.Cox                                                   
!                                                                               
! Revision 1.2  98/01/29  17:11:42  17:11:42  usmdb (Generic MDB account)       
! Addition of IBM preprocess directive.                                         
!                                                                               
! Revision 1.1  97/09/10  15:45:44  15:45:44  uspm (Pat McCormack)              
! Initial revision                                                              
!                                                                               
! 15-09-97       : S.Cox - written.                                             
!                                                                               
!-----------------------------------------------------------------------        
! (C) CROWN COPYRIGHT 2005 - MET OFFICE. All Rights Reserved.                   
!                                                                               
! Met Office, United Kingdom                                                    
!                                                                               
! The use, duplication and disclosure of this code is strictly                  
! prohibited without the permission of Data Storage and Access Team             
! at the above address.                                                         
!-----------------------------------------------------------------------        
                                                                                
IMPLICIT NONE                                                                   
                                                                                
!-----------------------------------------------------------------------        
! Parameter statements.                                                         
!-----------------------------------------------------------------------        
                                                                                
INTEGER        MaxStn                                                           
                                                                                
PARAMETER      (MaxStn=16000)                                 !2.2              
                                                                                
!-----------------------------------------------------------------------        
! Declare variables                                                             
!-----------------------------------------------------------------------        
                                                                                
INTEGER        I           !- general loop counter                              
INTEGER        WmoBlock(99)!- Pointers to Stor arrays from WMO blk              
INTEGER        IndexPos    !- Position in Index array                           
INTEGER        IOR         !- IOSTAT variable for file read                     
INTEGER        J           !- general loop counter                              
INTEGER        NumStn      !- number of stations read from list                 
                                                                                
LOGICAL        InCore      !- TRUE if list held in memory                       
LOGICAL        Match       !- TRUE if input station found in list               
                                                                                
CHARACTER*132  HEAD              !- Revision information                        
CHARACTER*(*)  Name              !- Input station name                          
CHARACTER*2    PrevBlk           !- Previous WMO block number                   
CHARACTER*120  Report            !- Entry read from STNABRV                     
CHARACTER*5    StorStn(MaxStn)   !- Array of WMO station numbers                
CHARACTER*42   StorNam(MaxStn)   !- Array of WMO station names                  
CHARACTER*(*)  Stn               !- Input station number                        
                                                                                
!if ! defined (MVS)                                                             
!endif                                                                          
                                                                                
!-----------------------------------------------------------------------        
! Dynamic common. Compile on IBM mainframe with FPARMS='DC(*)'                  
!-----------------------------------------------------------------------        
                                                                                
COMMON /READABV1/ StorStn,StorNam                                               
                                                                                
!-----------------------------------------------------------------------        
! Save statement to ensure variables still set on return to subroutine          
!-----------------------------------------------------------------------        
                                                                                
SAVE                                                                            
                                                                                
!-----------------------------------------------------------------------        
! Data statements                                                               
!-----------------------------------------------------------------------        
                                                                                
DATA InCore /.FALSE./                                                           
                                                                                
!-----------------------------------------------------------------------        
! Initialise variables                                                          
!-----------------------------------------------------------------------        
                                                                                
Match    = .FALSE.                                                              
I        = 0                                                                    
IndexPos = 0                                                                    
Name(:)  = ' '                                                                  
                                                                                
!-----------------------------------------------------------------------        
! If station list not already in core, do the following...                      
!-----------------------------------------------------------------------        
                                                                                
IF (.NOT.InCore) THEN                                                           
                                                                                
!-----------------------------------------------------------------------        
! Initialise variables.                                                         
!-----------------------------------------------------------------------        
                                                                                
  InCore  = .TRUE.                                                              
  IOR     = 0                                                                   
  NumStn  = 0                                                                   
  PrevBlk = '00'                                                                
                                                                                
  HEAD='$RCSfile: readabrv.F,v $ ' //&                                          
       &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'                             
                                                                                
  DO J=1,99                                                                     
    WmoBlock(J) = 0                                                             
  ENDDO                                                                         
                                                                                
!-----------------------------------------------------------------------        
! Open STNABRV                                                                  
!-----------------------------------------------------------------------        
                                                                                
                                                                                
                                                                                
!if defined (MVS)                                                               
  OPEN (81,FILE='STNABRV',ACTION='READ')                                        
!else                                                                           
!endif                                                                          
                                                                                
!-----------------------------------------------------------------------        
! Read the station list into memory, if not already there.                      
!-----------------------------------------------------------------------        
                                                                                
  DO WHILE ((IOR.EQ.0).AND.(NUMSTN.LT.MAXSTN))                !2.2              
    READ(81,'(A120)',IOSTAT=IOR) Report                                         
    IF (Report(2:6).GT.'00000' .AND. Report(2:6).LT.'99999') THEN               
      NumStn=NumStn+1                                                           
      IF (Report(2:3).NE.PrevBlk(1:2)) THEN                                     
        PrevBlk(1:2) = Report(2:3)                                              
        READ(Report(2:3),'(I2)') IndexPos                                       
        WmoBlock(IndexPos) = NumStn                                             
      ENDIF                                                                     
      StorStn(NumStn) = Report(2:6)                                             
      StorNam(NumStn) = Report(58:99)                                           
    ENDIF                                                                       
  END DO                                                                        
                                                                                
!2.2 If number of stations greater than MAXSTN then write warning.              
  IF (NUMSTN.EQ.MAXSTN) THEN                                  !2.2              
    WRITE(6,*)'Warning in routine READABRV:'                  !2.2              
    WRITE(6,*)'Parameter MAXSTN too small at ',MAXSTN         !2.2              
  ENDIF                                                       !2.2              
                                                                                
  CLOSE(81)                                                                     
ENDIF                                                                           
                                                                                
!-----------------------------------------------------------------------        
! Loop over station numbers in memory. If a match is found between the          
! input station number and the station number in memory, return the             
! station name.                                                                 
!-----------------------------------------------------------------------        
                                                                                
READ(Stn(1:2),'(I2)') IndexPos                                                  
I=WmoBlock(IndexPos)-1                                                          
                                                                                
DO WHILE (I.LT.NumStn .AND. .NOT.Match)                                         
  I=I+1                                                                         
  IF (Stn(1:5).EQ.StorStn(I)(1:5)) THEN                                         
    Match=.TRUE.                                                                
    Name(1:42)=StorNam(I)(1:42)                                                 
  ENDIF                                                                         
END DO                                                                          
                                                                                
RETURN                                                                          
END SUBROUTINE READABRV                                                         
