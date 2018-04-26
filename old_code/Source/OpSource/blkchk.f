      SUBROUTINE BLKCHK(BLOCK,IBLOCK,INDEKS,NTRIES)

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : BLKCHK                                              
!                                                                     
! PURPOSE       : Check that the lengths in an MDB data block amount  
!                 to valid pointers, i.e. as lengths are subtracted   
!                 from the blocksize the end of a BUFR message is     
!                 always pointed to, or rather there are ASCII sevens 
!                 before the trailer.  If not, unset index pointers   
!                 with record numbers in the inconsistent range.      
!                  (Needed to cope with occasional error when one     
!                 more AIREP put in data block although not enough    
!                 room for it, so lengths overwritten.)               
!                                                                     
! DATA TYPE(S)  : Any stored as BUFR messages with trailers           
!                  (no space between end of message & trailer!)       
!                                                                     
! CALLED BY     : MDBARK                                              
!                                                                     
! PARAMETERS    : (1) data block                                 (i/o)
!                      (number of records may be reduced)             
!                 (2) data block number (as in index pointers)    (i) 
!                 (3) index for period concerned                 (i/o)
!                      (some pointers may be zeroed)                  
!                 (4) number of entries in index                  (i) 
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:01$
! $Source: /home/us0400/mdb/op/lib/source/RCS/blkchk.F,v $
!                                                                     
! CHANGE RECORD :
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:01    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:30  usmdb
! Replaced function statement with calls to external function
! ICHAR2. Removed unused variable, added copyright and modified
! header - S.Cox
!
! Revision 1.3  99/09/09  10:15:27  10:15:27  usmdb (Generic MetDB account)
! 20 Sept 99     C Long
! 1.3  Avoid alarming messages when last lengths in data block are
! zero after split.
! 
! Revision 1.2  97/07/31  09:11:44  09:11:44  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 10:57:32  uspm
! Initial revision
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

      CHARACTER*(*) BLOCK
      CHARACTER*23 INDEKS(*)
      CHARACTER*4 SEVENS
      CHARACTER*132 HEAD                         !revision information
      INTEGER IBLOCK,NTRIES
      INTEGER BLOCKNO,RECNO
      LOGICAL FIRST_CALL
      DATA FIRST_CALL/.TRUE./
      DATA SEVENS/'7777'/
!
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/blkchk.F,v $
     &'//'$ $Date: 30/01/2006 20:21:01$ $Revision: 1$'

      IF (FIRST_CALL) THEN
        CALL EB2ASC(4,SEVENS)
        FIRST_CALL=.FALSE.
      ENDIF
!
! Get number of records in this data block (from start of block)
!
      NINBLK=ICHAR2(BLOCK(3:4))                                     !2.0
      LEFT=ICHAR2(BLOCK(5:6))                                       !2.0
!
! Check that each record has 7777 (end of BUFR message) before trailer
!
      L=LEN(BLOCK)
      J=0
      DO WHILE (J.LT.NINBLK .AND. BLOCK(L-26:L-23).EQ.SEVENS)
        J=J+1
        L=L-ICHAR2(BLOCK(6+J*2-1:6+J*2)) ! point L to end of report !2.0
      ENDDO
!
! If a length doesn't give a pointer to sevens, either the (J+1)-th ob
! is bad or the J-th length: unset index pointers from the J-th ob on
! (looping round index entries checking for block number to do this).
!
      IF (J.LT.NINBLK .AND. L.GT.6+2*NINBLK+LEFT) THEN              !1.3
       print *,iblock,'th data block lost records',j,'to',ninblk
        n_pointers_zeroed=0
        DO I=1,NTRIES
          RECNO=ICHAR2(INDEKS(I)(20:21))                            !2.0
          BLOCKNO=ICHAR2(INDEKS(I)(22:23))                          !2.0
          IF (BLOCKNO.EQ.IBLOCK .AND. RECNO.GE.J) THEN
            print *,indeks(i)
            INDEKS(I)(20:23)=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
            n_pointers_zeroed=n_pointers_zeroed+1
          ENDIF
        ENDDO
        print *,n_pointers_zeroed,'pointers zeroed'
!
! Reduce count of records in block by number of pointers zeroed.
!
        BLOCK(3:3)=CHAR(J/256)
        BLOCK(4:4)=CHAR(MOD(J,256))
      ENDIF
      RETURN
      END
