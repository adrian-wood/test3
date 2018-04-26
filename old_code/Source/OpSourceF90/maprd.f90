SUBROUTINE MAPRD(IDSK,NXBLK,NXHRS,NAMHR,INDLEN,LFLAG,&        !1.5
&LOCDFG,NELMIX,IBLOCK,ITRIES,CNTRY,&          !1.5
&IDATHR,IDREC,RECLEN,CMSG,CTYPE)              !1.5

!-----------------------------------------------------------------------
!
! PROGRAM       : MAPRD
!
!               : ANSI standard except for '!' used for comments,
!                 IMPLICIT NONE statement, variable name lengths
!                 greater than 6 characters, ENDDO statements, and
!                 a common block which has a mixture of character and
!                 non-character variables, although neither the HP
!                 or Cray FORTRAN77 compilers complain.
!
! PURPOSE       : TO READ MAP BLOCK, RETURNING PARAMETERS AT START,
!                 AND CALLING LOCALD WITH ANY LOCAL SEQUENCE.
!                 FOR OFFLINE DATA MAKE AN INDEX DATA SET (ON FT85).
!
! DESCRIPTION   : MAPRD IS CALLED FIRST, FOLLOWED BY A NUMBER OF CALLS
!                 To read the index and data blocks
!
! DATA TYPE(S)  : ANY
!
! CALLED BY     : TFMRET, SYNRET, BUSRET, SSMRET, SYNSEQ
!
! CALLS         : LOCALD
!
! PARAMETERS    : (1)IDSK(5)  only 3 of the 5 elements are used:
!                      (2) blocksize, (3) ft number, (5) 1 if direct
!                 (2)NXBLK    NO OF INDEX BLOCKS IN D/S      (OUTPUT)
!                 (3)NXHRS    NO OF HOURS PER INDEX BLOCK    (OUTPUT)
!                 (4)NAMHR    START HOUR OF FIRST INDEX BLK  (OUTPUT)
!                 (5)INDLEN   LENGTH OF INDEX ENTRIES
!                 (6)LFLAG    TRUE FOR DIAGNOSTICS
!                 (7)LOCDFG   LOCAL TABLE D FLAG             (OUTPUT)
!                 (8)NELMIX   block number for element index (output)!e
!                 (9)iblock   physical block number to read  (input)
!                (10)itries   number of entries read         (output)
!                (11)cntry    char*(*) array of entries read (output)
!                (12)idathr   time tag of index read         (output)
!                (13)idrec    logical record number          (input)
!                (14)reclen   total length of message        (output)
!                (15)cmsg     char*(*) actual message        (output)
!                (16)ctype    type of block to read          (input)
!
!Y2K  26.06.1997  MAPRO is Year 2000 compliant.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/maprd.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2001/10/03 15:09:13  usmdb
! 15 Oct 2001    C Long
! 2.1  If message is too long for string, don't truncate trailer.
!
! Revision 2.0  2001/01/08  11:58:50  11:58:50  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.6  99/02/11  12:16:58  12:16:58  usmdb (Generic MDB account)
! 15-02-1999 S.Cox
! Change dynamic common name MAP1 to MAPRDDC1. It was conflicting with
! the MDBIO common block MAP1 in ATOVS merge.
! v(G)=28, ev(G)=5
!
! Revision 1.5  99/01/14  14:14:25  14:14:25  usmdb (Generic MDB account)
! 18-01-1999 S.Cox
! Change to cope with new MAP overflow system and removal of
! off-line sequential read code - now redundant.
! v(G)=28, ev(G)=5
!
! Revision 1.4  98/02/19  11:36:23  11:36:23  usmdb (Generic MDB account)
! Remove unwanted write statement.
!
! Revision 1.3  1997/08/04 13:13:59  uspm
! First revisioned version for 1 - with Y2K change
!
! Revision 1.2  1997/02/28 11:12:31  uspm
! Latest version from mainframe
!
! Revision 1.1  1997/02/11 16:35:54  uspm
! Initial revision
!
! 23/02/98 : Remove the MDB WARNING "NOT ENOUGH SPACE IN DATA SET..."
!            as this has a hard-wired limit in MAPRD, but the actual
!            no. varies between subtypes, so it's not useful - S.Cox
!
! 19/12/96 : Correction to off-line data retrieval. This did not work
!            correctly for off-line datasets with a local sequence
!            in record 2 - S.Cox                                     !I
!
! 11/07/96 : Add a check on the number of fixed index blocks when    !H
!            retrieving data off-line. Previously datasets with more
!            than 127 fixed index blocks were being incorrectly
!            copied to the temporary dataset. The remaining fixed
!            index blocks were being treated as overflow blocks which
!            led to index blocks being overwritten by other index
!            blocks in the temporary dataset. Now if the number of
!            fixed index blocks exceeds 127 they will all be treated
!            as fixed index blocks.
!
! 01/04/96 : Check for RECLEN=0 before setting CMSG at end           !G
!            (data block not reread if in core, so may be out of step
!            with up-to-date index searched by SYNSEQ during storage)
!            & return RECLEN=0 if IDREC> number of obs in data block.
!
! 25/03/96 : Correct code to restore offline data so that it copes   !F
!            with data sets remade by MDBARC (overflows no longer
!            in the overflow pool) as well as straight copies.
!            (TAFs & METARs are archived by MDBARC, others copied.)
!
! 29-01-96   Return block number of element index (bit counts for
!            fast retrieval) if map says there is one.               !E
!
!            Note: when this routine is copied to a Unix machine, the
!                  call to FILEINF must be commented out. It is an
!                  IBM system routine and is not needed on Unix.
!
! 19-01-96   S.Cox - to make it work on HP-UX machine find "!D"
!
!          : Originally there were 3 entry points to the maprd
!          : subroutine, for maprd, idxrd and msgrd. Having more than
!          : one entry point for a subroutine is bad practice and on
!          : the HP caused unpredictable results. The routine was
!          : re-written to have a single entry point. The number of
!          : arguments has been increased and these are now an
!          : amalgamation of those that were passed to/from the three
!          : original entry points maprd, idxrd, msgrd. An additional
!          : argument, CTYPE is used to tell the routine whether to
!          : read a map block, index block or data block. A SAVE
!          : statement has been added. Part of the code calls an IBM
!          : routine FILEINF to allocate more space for the temporaray
!          : dataset. On the HP we don't need to do this, so the
!          : offending line has been commented out. The temporary
!          : dataset only applies to off-line data, which we aren't
!          : extracting anyway.
!
! 30/03/95   MAKE INDEX DATA SET FOR OFFLINE DATA IN A SINGLE PASS !C
!            (COMPLETE REWRITE OF HORRIBLY INEFFICIENT CODE! - NOT
!             OPERATIONAL TILL ?]
! 24/03/95   CLAIM EXACT NUMBER OF BLOCKS NEEDED FOR TEMPORARY INDEX
!            DATA SET RATHER THAN ACCEPTING FORTRAN DEFAULT        !B
!            & START A NEW TEMPORARY DATA SET IF MORE BLOCKS NEEDED
! 10/11/94   CORRECTION TO HOW OFF-LINE RECORDS ARE WRITTEN TO
!            TEMPORARY DATASET WHEN TABLE D ENTRY IS PRESENT      A
! 03/11/94   CHANGE PARAMETER IN CALL TO LOCALD TO 'NEW' TO PREVENT
!            ARRAY OVERFLOWING AT TIME OF RETRIEVAL
! 25/07/94   CHANGE UNIT NUMBER OF TEMPORARY DATASET TO FT85 TO AVOID
!            CLASHING WITH STATION MASTER UNIT NUMBERS.
! 18/04/94   CORRECTION TO THE WAY LOCAL TABLE D ENTRIES ARE HANDLED.
! 28/03/94   INTEGER*2 AND EQUIVALENCE STATEMENTS REMOVED. TWO MORE
!            PARAMETERS ADDED TO ARGUMENT LIST IN INTERFACE; INDLEN
!            AND LOCDFG. INDLEN ALLOWS THE HANDLING OF VARIABLE
!            LENGTH INDEX ENTRIES; LOCDFG IS A FLAG INDICATING THE
!            PRESENCE OF A LOCAL TABLE D ENTRY. CHANGE TO RETRIEVE
!            LOCAL D ENTRY. CHANGE TO ALLOW THE RETRIEVAL OF OFF-LINE
!            INDEX OVERFLOW RECORDS.
! 22/07/93   FOR OFF-LINE I/O, COPY THE INDEX RECORDS TO TEMP DISK
!            SPACE.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
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
INTEGER    IDSK(5)        ! DATA SET DETAILS
INTEGER    NXBLK          ! NUMBER OF FIXED INDEX BLOCKS
INTEGER    NXHRS          ! NO OF HOURS PER INDEX
INTEGER    NAMHR          ! START OF FIRST INDEX AFTER 00Z
INTEGER    INDLEN         ! LENGTH OF INDEX ENTRY

LOGICAL    LFLAG          ! DIAGNOSTICS FLAG
LOGICAL    LOCDFG         ! LOCAL TABLE D FLAG
INTEGER    NELMIX         ! block number of element index      !E

INTEGER    IDATHR
INTEGER    NTRIES         ! NO. OF INDEX ENTRIES
INTEGER    NBLKS          ! TOTAL NO OF RECORDS IN DATASET
INTEGER    TOTREC
INTEGER    IRECLN(MAXINDX)
CHARACTER*(*)  CNTRY(*)   ! ARRAY OF INDEX ENTRIES
CHARACTER*(*)      CMSG   ! THE MESSAGE ITSELF
CHARACTER*(BLKSIZ) BLOCK
CHARACTER*(BLKSIZ) CMAP   ! MAP BLOCK
CHARACTER*(BLKSIZ) CTABD  ! LOCAL TABLE D ENTRY
INTEGER            ITABD  ! LOCAL TABLE D INDICATOR
INTEGER            FLW1   ! START OF OVERFLOW NUM IN INDEX BLOCK
INTEGER            FLW2   ! FLW1+1
INTEGER            IRLEN  ! RECORD LENGTH
INTEGER            IFT    ! INPUT FILE NUMBER, THEN FILE NUMBER
                          ! FOR INDEX DATA SET IF INPUT OFFLINE

INTEGER            ICREC  ! CURRENT SEQUENTIAL RECORD NUMBER

INTEGER            IPIND        ! PREVIOUS INDEX BLOCK
INTEGER            IPDAT        ! PREVIOUS DATA BLOCK
INTEGER            IBLOCK
INTEGER            NBLIND       ! MAX NO INDEX ENTRIES IN BLOCK
INTEGER            COUNT
INTEGER            IX
INTEGER            NIND
INTEGER            NIBL
INTEGER            LIMIT
INTEGER            NOFLOW     ! OVERFLOW RECORD NUMBER
INTEGER            ITRIES
INTEGER            I,J,K
INTEGER            IDREC
INTEGER            ILEN
INTEGER            JL
INTEGER            RECLEN
INTEGER            START

REAL               X
CHARACTER*5        CTYPE      ! type of block to read         !D

!-----------------------------------------------------------------------
! dynamic common
!-----------------------------------------------------------------------

COMMON /MAPRDDC1/ IRECLN,CMAP,CTABD,BLOCK                     !1.6

!-----------------------------------------------------------------------
! variables need to be static. SAVE statement added.
!-----------------------------------------------------------------------

SAVE                                                          !D

CHARACTER*80 HEAD                                        !2
HEAD = '$Workfile: maprd.f90$ ' //&
      &'$Revision: 1$ $Date: 26/01/2010 10:18:13$ '

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** If CTYPE = 'MAPRD' or 'MAPR1' read map block.                ***
! *** (MAPR1 is like MAPRD but always looks for the BUFR sequence  *!2
! ***  indicator after the map even if there are >12000 records.)  *!2
! ***                                                              ***
! ********************************************************************
! ********************************************************************

IF (CTYPE.EQ.'MAPRD' .OR. CTYPE.EQ.'MAPR1') THEN               !2

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

  IF (LFLAG) WRITE(6,*) 'IN MAPRD'

  NELMIX = 0                                                  !1.5
  IRLEN = IDSK(2)
  NBLIND = (IRLEN - INDHED - 2)/INDLEN
  FLW1 = (6 + NBLIND*INDLEN) + 1
  FLW2 = FLW1 + 1
  NOFLOW = 0

  IFT  = IDSK(3)
  IF (LFLAG) WRITE(6,10)IRLEN,IFT                             !1.5
 10  FORMAT(' RECORD LENGTH ',I6,' FT',I2.2)                     !1.5

  READ(IFT,REC=1) CMAP(1:IRLEN)                               !1.5

  NBLKS = ICHAR(CMAP(1:1)) * 256 + ICHAR(CMAP(2:2))
  NXBLK = ICHAR(CMAP(3:3)) * 256 + ICHAR(CMAP(4:4))
  NXHRS = ICHAR(CMAP(5:5)) * 256 + ICHAR(CMAP(6:6))
  NAMHR = ICHAR(CMAP(7:7)) * 256 + ICHAR(CMAP(8:8))

!-----------------------------------------------------------------------
! If the number of blocks in the merge dataset is less than 12000, the
! sequence record indicator will be in the map block immediately
! after the data block pointers, otherwise it will be in byte 9 of
! the map block unless 'MAPR1' was specified.                        !2
!-----------------------------------------------------------------------

  IF (NBLKS.LE.12000 .OR. CTYPE.EQ.'MAPR1') THEN               !2
    ITABD = ICHAR(CMAP(NBLKS+8:NBLKS+8))                      !1.5
  ELSE                                                        !1.5
    ITABD = ICHAR(CMAP(9:9))                                  !1.5
  ENDIF                                                       !1.5

  IF (LFLAG) THEN
    WRITE(6,*) 'NO OF RECORDS IN DATASET       = ',NBLKS
    WRITE(6,*) 'NO OF INDEX BLOCKS             = ',NXBLK
    WRITE(6,*) 'HOURS PER INDEX                = ',NXHRS
    WRITE(6,*) 'START OF FIRST INDEX AFTER 00Z = ',NAMHR
  ENDIF

!-----------------------------------------------------------------------
! CHECK FOR LOCAL TABLE D ENTRY:    0 -> NO ENTRY
!                                   2 -> ENTRY PRESENT IN SECOND RECORD
!
! IF AN ENTRY IS PRESENT, THEN ALL BLOCK NUMBERS MUST BE CALCULATED
! TO ALLOW FOR THIS EXTRA RECORD; THIS IS DONE AT A HIGHER LEVEL
!-----------------------------------------------------------------------

  LOCDFG = ITABD.EQ.2
  IF (LOCDFG) THEN
    READ(IFT,REC=2) CTABD(1:IRLEN)                            !1.5
    IF (LFLAG) PRINT*,' LOCAL TABLED ENTRY IN MDB DATASET'
    IF(CTABD(1:1).EQ.'3')CALL LOCALD(0,0,X,X,CTABD,'NEW')
    ICREC = 2                          ! TWO RECORDS READ
  ELSE
    IF (LFLAG) WRITE(6,*) ' NO LOCAL SEQUENCE IN SECOND RECORD'
    ICREC = 1                          ! ONLY ONE RECORD READ
  ENDIF

!-----------------------------------------------------------------------
! A call to MAPRD implies a new data set, so unset I/O-saving variables
! so there's no chance of leaving block from previous data set in core.
! (ICREC is the last block read from the offline data set, IPDAT the
! data block now in core: both are needed in case the data set changes!)
!-----------------------------------------------------------------------

  IPIND = 0
  IPDAT = 0

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** If CTYPE='IDXRD' read index block.                           ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

ELSEIF (CTYPE.EQ.'IDXRD') THEN                                !D

  IF (LFLAG) PRINT*,' IN IDXRD',IBLOCK
  IF (IBLOCK.NE.IPIND) THEN
    COUNT = 0

!-----------------------------------------------------------------------
! calculate maximum possible number of index entry in one block
!-----------------------------------------------------------------------

    NBLIND = (IRLEN - INDHED - 2)/INDLEN

    IX   = IBLOCK
    NIND = 1
    LIMIT = 0

!-----------------------------------------------------------------------
! loop over index entries and store them in array 'cntry'. the implicit
! index 'k' corresponds to the current index of 'cntry' because the
! lower limit (1 + nibl) and the upper limit, 'limit', are recalculated
! each time round the loop.
!-----------------------------------------------------------------------

   45     NIBL = (NIND - 1) * NBLIND
    NOFLOW = 0

    READ(IFT,REC=IX) CMAP(1:IRLEN)
    IF (LFLAG) PRINT *,IX,'TH BLOCK READ'

    IDATHR = ICHAR(CMAP(1:1)) * 256 + ICHAR(CMAP(2:2))
    NTRIES = ICHAR(CMAP(3:3)) * 256 + ICHAR(CMAP(4:4))

    IF (NTRIES - COUNT.GT.NBLIND) THEN
      LIMIT = NIBL + NBLIND
      COUNT = COUNT + NBLIND
      NOFLOW = ICHAR(CMAP(FLW1:FLW1))*256 + ICHAR(CMAP(FLW2:FLW2))
    ELSE
      LIMIT = NTRIES
    ENDIF

    IF (LFLAG) THEN
      PRINT*,' IDATHR = ', IDATHR
      PRINT*,' NTRIES = ', NTRIES
      PRINT*,' NIBL   = ', NIBL
      PRINT*,' LIMIT  = ', LIMIT
      PRINT*,' NBLIND = ', NBLIND
      PRINT*,' NOFLOW  = ', NOFLOW
    ENDIF

    START = 7
    DO 40 K = 1 + NIBL, LIMIT
      CNTRY(K) = CMAP(START:START+INDLEN-1)
      START  = START + INDLEN
   40     CONTINUE

    IF (NOFLOW.GT.0) THEN                                     !1.5
      IX = NOFLOW
      IF(LOCDFG)IX=IX+1
      NIND = NIND + 1
      GOTO 45
    ENDIF

    IPIND = IBLOCK
    ITRIES = NTRIES

    IF (LFLAG) PRINT*,' TIMTAG IS',IDATHR
  ENDIF

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** If CTYPE='MSGRD' read data block.                            ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

ELSEIF (CTYPE.EQ.'MSGRD') THEN                                !D

  IF (LFLAG) WRITE(6,*) 'IN MSGRD   READING BLOCK',IBLOCK

!-----------------------------------------------------------------------
! If this block is already in memory, don't read it again.
! If the data set is offline, check against the last block read (here
! or in MAPRD section) to see whether to read on or rewind & restart.
!-----------------------------------------------------------------------

  IF (IBLOCK.NE.IPDAT) THEN
    READ(IDSK(3),REC=IBLOCK)BLOCK(1:IRLEN)                    !1.5
    IPDAT = IBLOCK

    TOTREC = ICHAR(BLOCK(3:3)) * 256 + ICHAR(BLOCK(4:4))

    J = 7
    DO 138 I = 1, TOTREC
      IRECLN(I) = ICHAR(BLOCK(J:J)) * 256 + ICHAR(BLOCK(J+1:J+1))
      J = J + 2
  138     CONTINUE
  ENDIF

  IF (LFLAG) PRINT*,' BLOCK ',IBLOCK,' RECORD ',IDREC

!-----------------------------------------------------------------------
! Block has lengths at start and messages at end.
! First message ends at end of block.
! Work back to end of message required.
! Return zero length if record number too big (possible in SYNSEQ call)
! If message is too long for string, truncate it - but assume it   !2.1
! has trailer at end, so keep last 23 characters & truncate rest.  !2.1
!-----------------------------------------------------------------------

  IF (IDREC.GT.TOTREC) THEN                                   !G
    RECLEN=0                                                  !G
    RETURN                                                    !G
  ENDIF                                                       !G

  ILEN = IRLEN
  DO 230 JL = 1, IDREC - 1
    ILEN = ILEN - IRECLN(JL)
  230  CONTINUE

  RECLEN = IRECLN(IDREC)
  IF (RECLEN.GT.0) THEN                                      !2.1
    CMSG=BLOCK(ILEN-RECLEN+1:ILEN)                           !2.1
    IF (RECLEN.GT.LEN(CMSG)) THEN                            !2.1
      CMSG(LEN(CMSG)-22:)=BLOCK(ILEN-22:ILEN)                !2.1
      PRINT *,'MAPRD truncated',RECLEN,'-character bulletin' !2.1
      RECLEN=LEN(CMSG)                                       !2.1
    ENDIF                                                    !2.1
  ENDIF                                                      !2.1

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** CTYPE not recognised                                         ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

    ELSE                                                          !D
      WRITE(6,*) 'MAPRD: CTYPE NOT RECOGNISED, CTYPE = ',CTYPE    !1.5
    ENDIF                                                         !D

 RETURN
 END SUBROUTINE MAPRD
