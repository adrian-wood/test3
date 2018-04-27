***********************************************************************
*                                                                     *
* PROGRAM       : MDBDECOD                                            *
*                                                                     *
* PURPOSE       : To decode a chain of messages in a MetDB data set   *
*                                                                     *
* CALLS         : LOCALD, DATE31, MDBFIND, DEBUFR                     *
*                                                                     *
* INPUT         : Input identifier, blocksize, yyyymmdd/hhmmZ         *
*                                                                     *
* CHANGE RECORD :                                                     *
*  21 DEC 99 - make it work for SYNOPs (different times in chain)  !1.2
*                                                                     *
***********************************************************************
      INTEGER*2 BLOCKS,XBLOKS,XHOURS,AMSTAR,IMTAG,NTRIES,LASLEF,NOFLOW
      CHARACTER DATIME*14,IDENT*9,MESAGE*28000,TEXT*28000
      CHARACTER ENTRY*23(15000),C*333
      CHARACTER*4 BUFR/Z42554652/,SEVENS/Z37373737/
      REAL VALUES(50000)
      INTEGER DESCR(50000)
      LOGICAL SYNOP                                                !1.2

! Read blocksize, identifier & date/time from input

      READ *,IDENT,LKSIZE,DATIME
      OPEN (3,FILE='MDB',ACCESS='DIRECT',RECL=LKSIZE)
      NBLIND=(LKSIZE-8)/23

! Read data set parameters from 1st block & then any local sequences
! See if SYNOPs or not (for index search) from sequence title.     !1.2

      READ (3,REC=1) BLOCKS,XBLOKS,XHOURS,AMSTAR,TEXT(1:LKSIZE-8)
      IXBLOKS=XBLOKS
      IXHOURS=XHOURS
      NSEQBL=ICHAR(TEXT(BLOCKS:BLOCKS))
      IF (NSEQBL.GT.0) THEN
        READ (3,REC=NSEQBL) TEXT(1:LKSIZE)
        SYNOP=INDEX(TEXT(1:80),'SYNOP').GT.0                       !1.2
        CALL LOCALD(0,0,X,X,TEXT,'ADD')
        NSQ=1
      ELSE
        NSQ=0
      ENDIF

! Convert date/time string to numbers

   10 READ (DATIME(1:4),'(I4)') IYEAR
      READ (DATIME(5:6),'(I2)') IMONTH
      READ (DATIME(7:8),'(I2)') IDAY
      READ (DATIME(10:11),'(I2)') IHOUR
      READ (DATIME(12:13),'(I2)') IMIN

! Convert input hour to index block number

      INDHOR=MOD(IHOUR+24-AMSTAR,IXHOURS)
      CALL DATE31(IDAY,IMONTH,IYEAR,ICENDAY)
      ICENTHR=(ICENDAY-1)*24+IHOUR-INDHOR
      NBLOCK=MOD(ICENTHR/IXHOURS,IXBLOKS)+NSQ+2

! Read in index entries for period - allowing for overflow blocks!

      NIND=1
   20 NIBL=(NIND-1)*NBLIND                 ! entries already read in
      READ (3,REC=NBLOCK) IMTAG,NTRIES,LASLEF,
     &                (ENTRY(I),I=NIBL+1,NIBL+NBLIND),NOFLOW
      IF (NOFLOW.GT.0) THEN
        NBLOCK=NOFLOW+NSQ
        NIND=NIND+1
        GO TO 20
      ENDIF

! Loop round index entries looking for same ident & timee
! (First byte of entry can have COR flag set with INDHOR!)
! (Second byte for U/A can have unpredictable non-zero minute,
!  so only check minute if minute in request is non-zero.)
! A SYNOP index entry gives the time of the latest ob in the       !1.2
! chain, so only check the identifier in this loop if SYNOP,       !1.2
! decoding everything in the chain to get the one requested.       !1.2

      I=1
      DO WHILE (I.LE.NTRIES .AND. (ENTRY(I)(3:9).NE.IDENT(1:7)
     & .OR. (.NOT.SYNOP .AND. (MOD(ICHAR(ENTRY(I)(1:1)),128).NE.INDHOR
     & .OR. (IMIN.GT.0 .AND. ICHAR(ENTRY(I)(2:2)).NE.IMIN)))))     !1.2
        I=I+1
      ENDDO

! If entry found, read in ob pointed to & decode it

      IF (I.LE.NTRIES) THEN
        IREC=ICHAR(ENTRY(I)(20:20))*256+ICHAR(ENTRY(I)(21:21))
        IBLOCK=ICHAR(ENTRY(I)(22:22))*256+ICHAR(ENTRY(I)(23:23))
   30   CALL MDBFIND(IBLOCK,IREC,IXBLOKS+NSQ,LKSIZE,MESAGE,LEN)
        NDESCR=50000
        NOBS=50000
        IN=INDEX(MESAGE,BUFR)
        CALL DEBUFR(DESCR,VALUES,C,NDESCR,NOBS,MESAGE(IN:),.TRUE.)
        PRINT *,' '

! If there's a trailer after the message, reset block & record
! & decode the next message if the pointer is nonzero

        IF (MESAGE(LEN-3:LEN).NE.SEVENS .AND.
     &      MESAGE(LEN-26:LEN-23).EQ.SEVENS) THEN
          IREC=ICHAR(MESAGE(LEN-3:LEN-3))*256
     &        +ICHAR(MESAGE(LEN-2:LEN-2))
          IBLOCK=ICHAR(MESAGE(LEN-1:LEN-1))*256
     &          +ICHAR(MESAGE(LEN:LEN))
          IF (IREC.GT.0 .AND. IBLOCK.GT.0) GO TO 30
        ENDIF
      ENDIF

! Go round again if more input, to decode another message

      READ (*,*,END=99) IDENT,LKSIZE,DATIME
      GO TO 10

   99 STOP
      END
***********************************************************************
*                                                                     *
* PROGRAM       : MDBFIND                                             *
*                                                                     *
* PURPOSE       : TO FIND THE N-TH MESSAGE IN THE NBLOCK-TH BLOCK     *
*                 IN AN MDB DATA SET (ALREADY OPEN)                   *
*                                                                     *
* DESCRIPTION   : READS BLOCK, COUNTS LENGTHS TO GET TO N-TH RECORD,  *
*                 PRINTS MESSAGE & RETURNS IT                         *
*                                                                     *
* CALLED BY     : MDBDEBUF                                            *
*                                                                     *
* PARAMETERS    : (1) BLOCK NUMBER                                    *
*                 (2) RECORD NUMBER                                   *
*                 (3) NUMBER OF INDEX BLOCKS IN DATA SET              *
*                 (4) BLOCKSIZE OF MDB DATA SET                       *
*                 (5) MESSAGE (OUTPUT)                                *
*                 (6) LENGTH OF MESSAGE                           !1.1*
*                                                                     *
***********************************************************************
      SUBROUTINE MDBFIND(NBLOCK,N,NIB,LKSIZE,MESAGE,LEN)          !1.1
*
      CHARACTER MESAGE*(*),BLOCK*27998
      INTEGER*2 RECLEN(999)
      EQUIVALENCE (BLOCK(7:),RECLEN)
*
* READ IN THE BLOCK REQUIRED  (NUMBERING SKIPS MAP & INDEX BLOCKS)
*
      NB=1+NIB+NBLOCK
      IF (NB.NE.LASTNB) THEN
        READ (3,REC=NB) BLOCK(1:LKSIZE)
        LASTNB=NB
      ENDIF
*
* BLOCK HAS LENGTHS AT START AND MESSAGES AT END.  FIRST MESSAGE ENDS
* AT END OF BLOCK.  WORK BACK TO END OF MESSAGE REQUIRED.  PRINT IT.
*
      LEN=LKSIZE
      DO 10 I=1,N-1
   10 LEN=LEN-RECLEN(I)
      MESAGE=BLOCK(LEN-RECLEN(N)+1:LEN)
*
      NLINES=RECLEN(N)/80
      DO 20 I=0,NLINES-1
   20 PRINT *,MESAGE(1+I*80:80+I*80)
      PRINT *,MESAGE(1+NLINES*80:RECLEN(N))
      PRINT *,' '
      LEN=RECLEN(N)                                               !1.1
      RETURN
      END
