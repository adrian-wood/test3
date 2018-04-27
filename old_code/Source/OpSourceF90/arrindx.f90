SUBROUTINE ARRINDX(IDESC,NELEM,IREPL,QCREQ,LFLAG,FIXARR,ELMNUM,&                
&SEGNUM,SUBNUM,NROWS,STYP,SEGST,SEGLEN,&                                        
&NSEGS,QCINC,NELREQ,DISPL,SOURCE,NEWSKLARR)   !A                                
                                                                                
!-----------------------------------------------------------------------        
! subroutine     : ARRINDX                                                      
!                                                                               
! portablity     : ANSI standard except for '!' used for comments,              
!                : IMPLICIT NONE and variable length names greater than         
!                : 6 characters.                                                
!                                                                               
! purpose        : to return subsripts and/or flags for the set of              
!                : elements given by the user from a  fixed array of            
!                : data elements.                                               
!                                                                               
! called by      : UPRRET                                                       
!                                                                               
! sub calls      : SKLARR   - to set up skeleton table                          
!                : ARRSUB   - to set up actual subscripts for this              
!                             array of data                                     
!                                                                               
! arguments      : IDESC     (ip) - array of element numbers requested          
!                : NELEM     (ip) - no of elements requested                    
!                : IREPL     (ip) - replication counts for elements in          
!                                  IDESC                                        
!                : QCREQ     (ip) - true for QC elements required               
!                : LFLAG     (ip) - flag for diagnostics                        
!                : FIXARR    (ip) - array of data (FinalProfile)                
!                : ELMNUM    (ip) - element nos. as in DDICT                    
!                : SEGNUM    (ip) - segment number                              
!                : SUBNUM    (ip) - subscript no relative to seg start          
!                : NROWS     (ip) - no of rows in element table                 
!                : STYP      (ip) - pointer to segment replication count        
!                                   (0 for mandatory)                           
!                : SEGST     (ip) - position of segment start                   
!                : SEGLEN    (ip) - length of segment                           
!                : NSEGS     (ip) - no of segments                              
!                : QCINC     (ip) - 2 if QC element precedes each               
!                                   elem in the fixed array , else 1            
!                : NELREQ    (op) - no. of elements required (incl. QC)         
!                : DISPL     (op) - array of subscripts                         
!                : SOURCE    (op) - array of source of data  (see               
!                :                  subroutine VALARR for definitions)          
!                : NEWSKLARR (ip) - TRUE if new MDB call with ISTAT=0 !A        
!                                                                               
!Y2K  01.07.1997  ARRINDX is Year 2000 compliant.                               
!                                                                               
! change history :                                                              
!                                                                               
! 01-09-96       : Written by S.M.Needham                                       
!                                                                               
! 18-04-97   !A  : New argument NEWSKLARR passed in. This means a new           
!                : call has been made to the MetDB with ISTAT=0, so it          
!                : is necessary to call SKLSUB again - S.Cox                    
!                                                                               
! 30-06-97   !B  : Change the the name of dynamic common ARRX2 to               
!                : ARRIND2 to avoid contention - S.Cox                          
!-----------------------------------------------------------------------        
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $                                                                             
! Revision 2.0  2001/01/08 11:58:27  usmdb                                      
! Argument SEGLEN removed from call to ARRSUB.                                  
! Added copyright - S.Cox                                                       
!                                                                               
! Revision 1.5  97/09/22  10:45:07  10:45:07  uspm (Pat McCormack)              
! Change order of varaible declarations to satisfy NAG F90 compiler             
!                                                                               
! Revision 1.4  1997/08/04 12:51:03  uspm                                       
! First revisioned version for MVS - with Y2K changes                           
!                                                                               
! Revision 1.3  1997/07/25 14:56:21  uspm                                       
! Version dated 30-6-97 from MVS                                                
!                                                                               
! Revision 1.2  1997/04/18 15:17:53  uspm                                       
! Latest version ( dated 18-04-97 ) copied from COSMOS                          
!                                                                               
! Revision 1.1  1997/02/17 11:48:31  uspm                                       
! Initial revision                                                              
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
                                                                                
IMPLICIT NONE                                                                   
                                                                                
INTEGER   MAXELM,MAXSEG                                                         
                                                                                
PARAMETER (MAXELM=9999)    !- max no. of elements per table                     
PARAMETER (MAXSEG=99)      !- max no. of segments                               
                                                                                
!-----------------------------------------------------------------------        
! declare variables in the argument list,                                       
! those used as array dimensions first                                          
!-----------------------------------------------------------------------        
                                                                                
INTEGER NELEM                                                                   
INTEGER NROWS                                                                   
INTEGER NSEGS                                                                   
                                                                                
INTEGER DISPL(*)                                                                
INTEGER ELMNUM(NROWS)                                                           
INTEGER I                  !- loop control                                      
INTEGER IDESC(NELEM)                                                            
INTEGER IREPL(NELEM)                                                            
INTEGER IVAL(MAXELM)                                                            
INTEGER NELREQ                                                                  
INTEGER NVALEN(MAXSEG)                                                          
INTEGER QCINC                                                                   
INTEGER SEGLEN(NSEGS)                                                           
INTEGER SEGMENT(MAXELM)                                                         
INTEGER SEGNUM(NROWS)                                                           
INTEGER SEGST(NSEGS)                                                            
INTEGER SOURCE(*)                                                               
INTEGER STYP(NSEGS)                                                             
INTEGER SUBNUM(NROWS)                                                           
                                                                                
REAL    FIXARR(*)                                                               
                                                                                
LOGICAL LFLAG                                                                   
LOGICAL NEWSKLARR          !- TRUE if New MDB call (ISTAT=0)    !A              
LOGICAL QCREQ                                                                   
                                                                                
CHARACTER HEAD*132         !- revision information                              
                                                                                
!-----------------------------------------------------------------------        
! Dynamic common                                                                
!-----------------------------------------------------------------------        
                                                                                
COMMON /ARRIND2/SEGMENT,IVAL,NVALEN                             !B              
                                                                                
SAVE                                                                            
                                                                                
HEAD='&                                                                         
&$Source: /home/us0400/mdb/op/lib/source/RCS/arrindx.F,v $&                     
&'//' $Revision: 1$ '//&                                                        
&'$Date: 26/01/2010 10:18:13$ '//&                                              
&'$Author: Richard Weedon$ $Locker: Stan Kellett$'                                          
                                                                                
!-----------------------------------------------------------------------        
! Set up skeleton displacements if it is a new call to the MDB with             
! ISTAT=0 (NEWSKLARR=.TRUE.)                                          !A        
!-----------------------------------------------------------------------        
                                                                                
IF (NEWSKLARR) THEN                                                             
  CALL SKLARR(IDESC,NELEM,IREPL,QCREQ,ELMNUM,SEGNUM,SUBNUM,NROWS,&              
       &SEGMENT,IVAL,NELREQ,QCINC,NSEGS,SEGLEN,NVALEN,LFLAG)                    
  NEWSKLARR=.FALSE.                                                             
ENDIF                                                                           
                                                                                
!-----------------------------------------------------------------------        
! Now set up actual displacements for this array                                
!-----------------------------------------------------------------------        
                                                                                
CALL ARRSUB(FIXARR,SEGMENT,IVAL,NELREQ,DISPL,SOURCE,NVALEN,&                    
           &STYP,SEGST,NSEGS,QCINC,LFLAG)                     !2.0              
                                                                                
IF(LFLAG)THEN                                                                   
  WRITE(6,*)'In ARRINDX: NELREQ ',NELREQ                                        
  WRITE(6,*)'  source      displ  '                                             
  WRITE(6,'(2I10)')(SOURCE(I),DISPL(I),I=1,NELREQ)                              
ENDIF                                                                           
                                                                                
RETURN                                                                          
END SUBROUTINE ARRINDX                                                          
