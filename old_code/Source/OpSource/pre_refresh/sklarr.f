      SUBROUTINE SKLARR(IDESC,NELEM,IREPL,QCREQ,ELMNUM,SEGNUM,SUBNUM,           
     &                  NROWS,SEGMENT,IVAL,NELREQ,QCINC,NSEGS,SEGLEN,
     &                  NVALEN,LFLAG)            
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! subroutine    : SKLARR - MDB Retrieval                                        
!                                                                               
! purpose       : prepare table of array subscripts relative to segment         
!                 starts for a given list of elements.                          
!                                                                               
! description   : element list is one-to-one with users array EXCEPT            
!                 if user asks for QCFLAGS.  The output table is always         
!                 one-to-one with users array.                                  
!                 If an element is not found, return -999 in the segment        
!                 array.                                                        
!                                                                               
! called by     : ARRINDX                                                       
!                                                                               
! calls         : nothing                                                       
!                                                                               
! arguments     : IDESC    (ip) : array of users element numbers                
!               : NELEM    (ip) : no. of elements in list                       
!               : IREPL    (ip) : counts to go with IDESC                       
!               : QCREQ    (ip) : true if QC flags wanted for each elem         
! The next three arrays define the element index for a fixed array              
!               : ELMNUM   (ip) : element number                                
!               : SEGNUM   (ip) : segment number                                
!               : SUBNUM   (ip) : subscript relative to seg start               
!               : NROWS    (ip) : no of rows in index table                     
! The next 2 arrays give the index tablefor just the required elements          
!               : SEGMENT  (op) : segment number                                
!               : IVAL     (op) : subscript relative to seg start               
!               : NELREQ   (op) : no of elements in output table                
!               : QCINC    (ip) : 2 if QC elements are available (or 1)         
!               : NSEGS    (ip) : no of segments                                
!               : SEGLEN   (ip) : no of elements per segment                    
!               : NVALEN   (op) : actual segment lengths                        
!               : LFLAG    (ip) : true for diagnostics                          
!                                                                               
! written by : S.M.Needham                                                      
!                                                                               
!Y2K  26.06.1997  SKLARR is Year 2000 compliant.                                
!                
! revision info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:18$
! $Source: /home/us0400/mdb/op/lib/source/RCS/sklarr.F,v $
!                                                               
! change record :
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:18    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:09  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  97/09/22  11:20:09  11:20:09  uspm (Pat McCormack)
! Change order of type declarations to satisfy NAG F90 compiler
! 
! Revision 1.2  1997/08/04 13:33:16  uspm
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/17 11:55:53  uspm
! Initial revision
!
! operational from  27 AUG 96                                   
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

      INTEGER      NELEM
      INTEGER      NROWS                                                        
      INTEGER      NSEGS                                                        

      INTEGER      IDESC(NELEM)                                                 
      INTEGER      IREPL(NELEM)                                                 
      LOGICAL      QCREQ                                                        
      INTEGER      ELMNUM(NROWS)                                                
      INTEGER      SEGNUM(NROWS)                                                
      INTEGER      SUBNUM(NROWS)                                                
      INTEGER      SEGMENT(*)                                                   
      INTEGER      IVAL(*)                                                      
      INTEGER      NELREQ                                                       
      INTEGER      QCINC                                                        
      INTEGER      SEGLEN(NSEGS)                                                
      INTEGER      NVALEN(NSEGS)                                                
      LOGICAL      LFLAG                                                        

!----------------------------------------------------------------------         
! local variables                                                               
!----------------------------------------------------------------------         

      INTEGER      QCWID                                                        
      INTEGER      I,J                                                          
      INTEGER      NN                                                           
      INTEGER      MSEG                                                         
      INTEGER      LASTONE                                                      
      INTEGER      DIRN                                                         
                                                                                                                                                               
      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/sklarr.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:24:18$ '
                                                                                
! ----------------------------------------------------------------------        
! first see if q/c flags are wanted and are available: QCWID=0 if there         
! are no QC elements or there are, but the user doesn't want them.              
! Assume the qc element is immediately before the data element.                 
! ----------------------------------------------------------------------        
                                                                                
      IF(LFLAG)PRINT*,' SKLARR:QCREQ ',QCREQ                                    
      QCWID=0                                                                   
      IF (QCINC.EQ.2)THEN                                                       
        IF (QCREQ)THEN                                                          
          QCWID=-1                                                              
        ENDIF                                                                   
      ENDIF                                                                     
      NELREQ=0        ! no. of elements required                                
      LASTONE=0       ! index of previous element found                         
      NN=0                                                                      
!-----------------------------------------------------------------------        
! set up actual segment lengths                                                 
!-----------------------------------------------------------------------        
      DO I=1,NSEGS                                                              
        NVALEN(I)=SEGLEN(I)*QCINC                                               
      ENDDO                                                                     
! ----------------------------------------------------------------------        
! loop round names in users request                                             
! ----------------------------------------------------------------------        
                                                                                
      DO 100 I=1,NELEM                                                          
! ----------------------------------------------------------------------        
! now look up the name in the index, where an element no. is followed by        
! 2 numbers to complete a line in the table.                                    
! Assume the elements are in numerical order of index number, in both           
! the index and the request.  Compare this element number with the found        
! previously to search forward or backward through the index.                   
! ----------------------------------------------------------------------        
                                                                                
        IF(IDESC(I).GT.LASTONE)THEN                                             
          DIRN=1                                                                
        ELSEIF(IDESC(I).EQ.LASTONE)THEN                                         
          DIRN=0                                                                
        ELSE                                                                    
          DIRN=-1                                                               
        ENDIF                                                                   
                                                                                
        DO 50 J=1,NROWS                                                         
          NN=NN+DIRN                                                            
          IF(NN.GT.NROWS)THEN                                                   
            NN=1                                                                
          ELSEIF(NN.LT.1)THEN                                                   
            NN=NROWS                                                            
          ENDIF                                                                 
          IF(IDESC(I).EQ.ELMNUM(NN))THEN                                        
                                                                                
! Got it, so set up a row of the output table                                   
! ----------------------------------------------------------------------        
! if q/c flags are wanted, there are 2 lines per element, the q/c flags         
! coming first                                                                  
! ----------------------------------------------------------------------        
                                                                                
            IF (QCWID.EQ.-1) THEN                                               
              NELREQ=NELREQ+2                                                   
            ELSE                                                                
              NELREQ=NELREQ+1                                                   
            ENDIF                                                               
                                                                                
            SEGMENT(NELREQ)=SEGNUM(NN)                                          
            IVAL(NELREQ)=SUBNUM(NN)                                             
            IF(IREPL(I).GT.1)THEN                                               
              IVAL(NELREQ)=IVAL(NELREQ)+                                        
     &            (IREPL(I)-1) * NVALEN(SEGMENT(NELREQ))                        
            ENDIF                                                               
                                                                                
!----------------------------------------------------------------------         
! Not all segment numbers have QC elements attached. e.g. -99 report            
! text, -1 fo integer elements from the header/trailer etc.                     
!----------------------------------------------------------------------         
            IF (QCWID.EQ.-1) THEN                                               
              MSEG=SEGMENT(NELREQ)                                              
              IF(MSEG.GT.0.AND.MSEG.NE.99)THEN                                  
                SEGMENT(NELREQ-1)=SEGMENT(NELREQ)                               
                IVAL(NELREQ-1)=IVAL(NELREQ)-1                                   
              ELSE                                                              
                SEGMENT(NELREQ-1)=-999                                          
              ENDIF                                                             
            ENDIF                                                               
                                                                                
            LASTONE=IDESC(I)                                                    
                                                                                
            GOTO 100      ! next users element                                  
          ENDIF                                                                 
50      CONTINUE                                                                
                                                                                
! ----------------------------------------------------------------------        
! if a name can't be found, set the segment no. to missing                      
! ----------------------------------------------------------------------        
                                                                                
        IF(QCWID.EQ.0)THEN                                                      
          NELREQ=NELREQ+1                                                       
          SEGMENT(NELREQ)=-999                                                  
        ELSE                                                                    
          NELREQ=NELREQ+2                                                       
          SEGMENT(NELREQ)=-999                                                  
          SEGMENT(NELREQ-1)=-999                                                
        ENDIF                                                                   
100   CONTINUE                                                                  
      IF(LFLAG)PRINT*,' END OF SKLARR: NELREQ=',NELREQ                          
      IF(LFLAG)THEN                                                             
        write(6,*)' seg      disp      seg      disp '                          
        WRITE(6,'(4I8)')(SEGMENT(I),IVAL(I),I=1,NELREQ)                         
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
