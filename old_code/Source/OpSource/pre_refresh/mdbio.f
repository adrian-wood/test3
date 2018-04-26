      SUBROUTINE MDBIO(IDSK,NXBLK,NXHRS,NAMHR,INDLEN,LOCDFG,
     &                 NELMIX,IBLOCK,ITRIES,CNTRY,IDATHR,IDREC,
     &                 RECLEN,CMSG,CTYPE)                           !2.0
                                                                                
!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : MDBIO     (made from MAPRD for use by storage)               
!                                                                              
! PURPOSE       : TO READ MAP BLOCK, RETURNING PARAMETERS AT START,            
!                 then read indexes and messages in data blocks                
!                                                                              
! DESCRIPTION   : 'MAPRD' call first, then any number of 'IDXRD' or            
!                 'MSGRD' calls                                                
!                                                                              
! DATA TYPE(S)  : SYNOPS (sequence checks), AIREPs (midpoint data)             
!                                                                              
! CALLED BY     : AIRIDX, SYNSEQ, SHPSEQ                                        
!                                                                              
! PARAMETERS    : (1)IDSK(5)  IDSK(2) is blocksize, IDSK(3) FT number          
!                 (2)NXBLK    NO OF INDEX BLOCKS IN D/S      (OUTPUT)          
!                 (3)NXHRS    NO OF HOURS PER INDEX BLOCK    (OUTPUT)          
!                 (4)NAMHR    START HOUR OF FIRST INDEX BLK  (OUTPUT)          
!                 (5)INDLEN   LENGTH OF INDEX ENTRIES        (input)           
!                 (6)LOCDFG   LOCAL TABLE D FLAG             (OUTPUT)          
!                 (7)NELMIX   block number for element index (output) !e         
!                 (8)iblock   physical block number to read  (input)           
!                 (9)itries   number of entries read         (output)          
!                (10)cntry    char*(*) array of entries read (output)          
!                (11)idathr   time tag of index read         (output)          
!                (12)idrec    logical record number          (input)           
!                (13)reclen   total length of message        (output)          
!                (14)cmsg     char*(*) actual message        (output)          
!                (15)ctype    type of block to read          (input)           
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:15$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mdbio.F,v $
!                                                                              
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:15    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:37  usmdb
! Removed unused dummy argument LFLAG. Added copyright and
! modified header - S.Cox
!
! Revision 1.2  97/07/31  09:29:52  09:29:52  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 12:55:24  uspm
! Initial revision
!
! AUG 96: CHECK THAT SAME DATA SET AS WELL AS SAME BLOCK!             !A         
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
                                                                               
      IMPLICIT NONE                                                             
                                                                                
      INTEGER    MAXINDX                                                        
      INTEGER    BLKSIZ                                                         
      INTEGER    INDHED                                                         
      PARAMETER  (MAXINDX=7000)                                                 
      PARAMETER  (BLKSIZ=27998) ! SET TO LARGEST MDB BLOCK SIZE                 
      PARAMETER  (INDHED=6)     ! LENGTH OF HEADER IN INDX BLOCK                
                                                                                
! arguments                                                                     
                                                                                
      INTEGER    IDSK(5)        ! DATA SET DETAILS                              
      INTEGER    NXBLK          ! NUMBER OF FIXED INDEX BLOCKS                  
      INTEGER    NXHRS          ! NO OF HOURS PER INDEX                         
      INTEGER    NAMHR          ! START OF FIRST INDEX AFTER 00Z                
      INTEGER    INDLEN         ! LENGTH OF INDEX ENTRY                         
      LOGICAL    LOCDFG         ! LOCAL TABLE D FLAG                            
      INTEGER    NELMIX         ! block number of element index                 
      INTEGER    IBLOCK                                                         
      INTEGER    ITRIES         ! number of index entries                       
      CHARACTER*(*) CNTRY(*)    ! ARRAY OF INDEX ENTRIES                        
      INTEGER    IDATHR         ! 12th argument                                 
      INTEGER    IDREC                                                          
      INTEGER    RECLEN                                                         
      CHARACTER*(*) CMSG        ! THE MESSAGE ITSELF                            
      CHARACTER*5        CTYPE  ! type of block to read                         
                                                                                
      INTEGER    NBLKS          ! TOTAL NO OF RECORDS IN DATASET                
      INTEGER    TOTREC         ! number of messages in data block              
      INTEGER    IRECLN(MAXINDX)                                                
      CHARACTER*(BLKSIZ) BLOCK                                                  
      CHARACTER*(BLKSIZ) CMAP   ! MAP BLOCK                                     
      CHARACTER*132      HEAD   ! Revision information
      INTEGER            ITABD  ! LOCAL TABLE D INDICATOR                       
      INTEGER            FLW    ! START OF OVERFLOW NUM IN INDEX BLOCK          
                                                                                
      INTEGER            INDFT  ! ft number for INDEX BLOCK                     
      INTEGER            MSGFT  ! ft number for DATA BLOCK                      
      INTEGER            INDBLK ! PREVIOUS INDEX BLOCK                          
      INTEGER            MSGBLK ! PREVIOUS DATA BLOCK                           
                                                                                
      INTEGER            NBLIND ! MAX NO INDEX ENTRIES IN BLOCK                 
      INTEGER            COUNT                                                  
      INTEGER            IX                                                     
      INTEGER            NIND                                                   
      INTEGER            NIBL                                                   
      INTEGER            LIMIT                                                  
      INTEGER            NOFLOW ! OVERFLOW RECORD NUMBER                        
      INTEGER            I,J                                                    
      INTEGER            ILEN                                                   
      INTEGER            START                                                  
                                                                                
**********************************************************************          
* COMPILE WITH DYNAMIC COMMON - FPARMS='DC(MAP1)'.                              
* THERE IS NO REFERENCE TO MAP1 ELSEWHERE (COMMON IS JUST TO SAVE SPACE         
* IN THE LOAD LIBRARY), BUT BECAUSE MDBIO IS IN SEVERAL MDB RETRIEVAL           
* MODULES, RETRIEVALS USING MORE THAN ONE OF THESE MODULES WILL FAIL            
* IF THE MAP1 DEFINITIONS ARE INCONSISTENT - SO DON'T ASSUME THAT MAP1          
* CAN BE CHANGED IN ONE MODULE AND OTHERS LEFT TILL LATER!!!                    
**********************************************************************          
                                                                                
      COMMON /MAP1/ IRECLN,CMAP,BLOCK                                           
                                                                                
! --------------------------------------------------------------------          
! --- variables need to be static. SAVE statement added.                        
! --------------------------------------------------------------------          
                                                                                
      SAVE                                                                      
                                                                                
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/mdbio.F,v $
     &'//'$ $Date: 30/01/2006 20:23:15$ $Revision: 1$'
                                                                                
! ********************************************************************          
! ********************************************************************          
! ***                                                              ***          
! *** If CTYPE='MAPRD' read map block.                             ***          
! ***                                                              ***          
! ********************************************************************          
! ********************************************************************          
                                                                                
      IF (CTYPE.EQ.'MAPRD') THEN                                                
        READ(IDSK(3),REC=1) CMAP(1:idsk(2))                                     
                                                                                
        NBLKS=ICHAR(CMAP(1:1))*256+ICHAR(CMAP(2:2))                             
        NXBLK=ICHAR(CMAP(3:3))*256+ICHAR(CMAP(4:4))                             
        NXHRS=ICHAR(CMAP(5:5))*256+ICHAR(CMAP(6:6))                             
        NAMHR=ICHAR(CMAP(7:7))*256+ICHAR(CMAP(8:8))                             
        ITABD=ICHAR(CMAP(NBLKS+8:NBLKS+8))                                      
                                                                                
! Map byte of one after index blocks can only be for element index:             
! return block number (missing if no byte set to one was found)                 
                                                                                
        NELMIX=1+NXBLK+INDEX(CMAP(8+NXBLK+1:8+NBLKS-1),CHAR(1))                 
        IF (NELMIX.EQ.1+NXBLK) THEN                                             
          NELMIX=-9999999                                                       
        ELSE IF (ITABD.GT.1) THEN                                               
          NELMIX=NELMIX+1                                                       
        ENDIF                                                                   
                                                                                
        IF (ITABD.GT.0) THEN                                                    
          LOCDFG=.TRUE.                                                         
        ELSE                                                                    
          LOCDFG=.FALSE.                                                        
        ENDIF                                                                   
                                                                                
! ********************************************************************          
! ********************************************************************          
! ***                                                              ***          
! *** If CTYPE='IDXRD' read index block.                           ***          
! ***                                                              ***          
! ********************************************************************          
! ********************************************************************          
                                                                                
      ELSEIF (CTYPE.EQ.'IDXRD') THEN                                            
        IF (IDSK(3).NE.INDFT .OR. IBLOCK.NE.INDBLK) THEN            !a          
          INDFT=IDSK(3)                                             !a          
          INDBLK=IBLOCK                                                         
          COUNT=0                                                               
                                                                                
! Calculate maximum possible number of index entries in one block               
                                                                                
          NBLIND=(IDSK(2)-INDHED-2)/INDLEN                                      
          FLW=(6+NBLIND*INDLEN)+1                                               
                                                                                
          IX=IBLOCK                                                             
          NIND=1                                                                
          LIMIT=0                                                               
                                                                                
! Loop over index entries and store them in array CNTRY.                        
! The range of I is recalculated each time round the loop.                      
                                                                                
   45     NIBL=(NIND-1)*NBLIND                                                  
                                                                                
          READ (INDFT,REC=IX) CMAP(1:IDSK(2))                       !a          
                                                                                
          IDATHR=ICHAR(CMAP(1:1))*256+ICHAR(CMAP(2:2))                          
          ITRIES=ICHAR(CMAP(3:3))*256+ICHAR(CMAP(4:4))                          
                                                                                
          IF (ITRIES-COUNT.GT.NBLIND) THEN                                      
            LIMIT=NIBL+NBLIND                                                   
            COUNT=COUNT+NBLIND                                                  
            NOFLOW=ICHAR(CMAP(FLW:FLW))*256+ICHAR(CMAP(FLW+1:FLW+1))            
          ELSE                                                                  
            LIMIT=ITRIES                                                        
            NOFLOW=0                                                            
          ENDIF                                                                 
                                                                                
          START=7                                                               
          DO 40 I=1+NIBL,LIMIT                                                  
            CNTRY(I)=CMAP(START:START+INDLEN-1)                                 
            START=START+INDLEN                                                  
   40     CONTINUE                                                              
                                                                                
          IF (LIMIT.NE.ITRIES) THEN                                             
            IX=NOFLOW                                                           
            IF(LOCDFG)IX=IX+1                                                   
            NIND=NIND+1                                                         
            GOTO 45                                                             
          ENDIF                                                                 
        ENDIF                                                                   
                                                                                
! ********************************************************************          
! ********************************************************************          
! ***                                                              ***          
! *** If CTYPE='MSGRD' read data block.                            ***          
! ***                                                              ***          
! ********************************************************************          
! ********************************************************************          
                                                                                
      ELSEIF (CTYPE.EQ.'MSGRD') THEN                                            
                                                                                
! If this block is already in memory, don't read it again.                      
                                                                                
        IF (IDSK(3).NE.MSGFT .OR. IBLOCK.NE.MSGBLK) THEN             !a         
          MSGFT=IDSK(3)                                              !a         
          READ(MSGFT,REC=IBLOCK)BLOCK(1:idsk(2))                     !a         
          MSGBLK=IBLOCK                                                         
          TOTREC=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))                        
                                                                                
! Lengths at start of block                                                     
                                                                                
          J=7                                                                   
          DO 138 I=1,TOTREC                                                     
            IRECLN(I)=ICHAR(BLOCK(J:J))*256+ICHAR(BLOCK(J+1:J+1))               
            J=J+2                                                               
  138     CONTINUE                                                              
        ENDIF                                                                   
                                                                                
! Block has lengths at start and messages at end.                               
! First message ends at end of block.                                           
! Work back to end of message required by subtracting lengths.                  
! Return zero length if record number too big (possible in SYNSEQ call)         
                                                                                
        IF (IDREC.GT.TOTREC) THEN                                               
          RECLEN=0                                                              
          RETURN                                                                
        ENDIF                                                                   
                                                                                
        ILEN=idsk(2)                                                            
        DO 230 I=1,IDREC-1                                                      
          ILEN=ILEN-IRECLN(I)                                                   
  230   CONTINUE                                                                
                                                                                
        RECLEN=IRECLN(IDREC)                                                    
        IF (RECLEN.GT.0) CMSG=BLOCK(ILEN-RECLEN+1:ILEN)                         
                                                                                
! ********************************************************************          
! ********************************************************************          
! ***                                                              ***          
! *** CTYPE not recognised                                         ***          
! ***                                                              ***          
! ********************************************************************          
! ********************************************************************          
                                                                                
      ELSE                                                                      
        WRITE(6,*) 'IN MDBIO: CTYPE NOT RECOGNISED, CTYPE=',CTYPE               
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
