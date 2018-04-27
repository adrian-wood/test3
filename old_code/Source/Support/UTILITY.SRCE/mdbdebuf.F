***********************************************************************
*
* PROGRAM       : MDBDEBUF
*
* PURPOSE       : TO DECODE A BUFR MESSAGE IN AN MDB DATA SET
*
* DESCRIPTION   : INPUT EITHER BLOCK & RECORD NUMBER OR (SATELLITE
*                 DATA ONLY) DATE/TIME AS IN ENCODE ERROR MESSAGES.
*                 (SEE JOB MDBDEBUF IN DBJCLLIB)
*
* CALLS         : LOCALD, DATE31, MDBFIND, DEBUFR
*
* INPUT         : BLOCK & RECORD, BLOCKSIZE, DATE/TIME
*
*Y2K  16.06.1997  MDBDEBUF IS YEAR 2000 COMPLIANT.
*Y2K                       ROUTINE CONTAINS DATE MANAGEMENT.
*
* REVISION INFO :
*
* $Revision: 1$
* $Date: 28/02/2006 12:07:33$
* $Source: /home/us0400/mdb/op/lib/utility/RCS/mdbdebuf.F,v $
*
* CHANGE RECORD :
*
* $Log:
*  1    Met_DB_Project 1.0         28/02/2006 12:07:33    Sheila Needham  
* $
* Revision 1.3  2002/02/05 14:03:44  usmdb
* Check for BLOCKS>12000 - S.Cox
*
* Revision 1.2  2000/12/11  10:22:44  10:22:44  usmdb (Generic MDB account)
* If obs are chained, decode all messages in chain. C.Long
*
***********************************************************************
      INTEGER*2 BLOCKS,XBLOKS,XHOURS,AMSTAR, IMTAG,NTRIES,LASLEF
      CHARACTER DATIME*14, MESAGE*28000,TEXT*28000
      CHARACTER ENTRY*12(2400), C*333                              !1.2
      REAL VALUES(50000)
      INTEGER DESCR(50000)
      CHARACTER*4 BUFR,SEVENS                                      !1.2
      BUFR=CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)    ! X'42554652' !1.2
      SEVENS=CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55)  ! X'37373737' !1.2
*
* READ BLKSIZE AND EITHER BLOCK/RECORD NUMBER OR DATE/TIME FROM INPUT
*
      READ *,NBLOCK,N,LKSIZE, DATIME
      OPEN (3,FILE='MDB',ACCESS='DIRECT',RECL=LKSIZE)
*
* READ DATA SET PARAMETERS FROM 1ST BLOCK & THEN ANY LOCAL SEQUENCES
*
      READ (3,REC=1) BLOCKS,XBLOKS,XHOURS,AMSTAR,TEXT(1:LKSIZE-8)
      IXBLOKS=XBLOKS
      IXHOURS=XHOURS

      IF (BLOCKS.LE.12000) THEN                                     !1.3
        NSEQBL = ICHAR(TEXT(BLOCKS:BLOCKS))                         !1.3
      ELSE                                                          !1.3
        NSEQBL = ICHAR(TEXT(1:1))                                   !1.3
      ENDIF                                                         !1.3

      IF (NSEQBL.GT.0) THEN                   ! IF ANY LOCAL SEQUENCES,
        READ (3,REC=NSEQBL) TEXT(1:LKSIZE)    ! READ BLOCK WITH THEM IN
        CALL LOCALD(0,0,X,X,TEXT,'ADD')       ! & MAKE THEM USABLE LATER
        NSQ=1
      ELSE
        NSQ=0
      ENDIF
*
* IF CHARACTER DATE/TIME INPUT, CONVERT IT TO NUMBERS
* (ASSUME DATE/TIME INPUT IS SATELLITE OPTION - DATA NOT CHAINED)   !1.2
*
   10 IF (DATIME(9:9).EQ.'/' .AND. DATIME(14:14).EQ.'Z') THEN
        READ (DATIME(1:4),'(I4)') IYEAR
        READ (DATIME(5:6),'(I2)') IMONTH
        READ (DATIME(7:8),'(I2)') IDAY
        READ (DATIME(10:11),'(I2)') IHOUR
        READ (DATIME(12:13),'(I2)') IMIN
*
* CONVERT INPUT HOUR TO INDEX BLOCK NUMBER
*
        INDHOR=MOD(IHOUR+24-AMSTAR,IXHOURS)
        CALL DATE31(IDAY,IMONTH,IYEAR,ICENDAY)
        ICENTHR=(ICENDAY-1)*24+IHOUR-INDHOR
        NBLOCK=MOD(ICENTHR/IXHOURS,IXBLOKS)+NSQ+2
*
* LOOP ROUND INDEX ENTRIES LOOKING FOR SAME HOUR & MINUTE
*
        READ (3,REC=NBLOCK) IMTAG,NTRIES,LASLEF,(ENTRY(I),I=1,NTRIES)
        DO 20 I=1,NTRIES
         NTHOUR=MOD(ICHAR(ENTRY(I)(2:2)),32)
         NTMIN=ICHAR(ENTRY(I)(3:3))/4
         IF (NTHOUR.EQ.INDHOR .AND. NTMIN.EQ.IMIN) THEN
           PRINT *,ENTRY(I)
           IREC=ICHAR(ENTRY(I)(10:10))
           IBLOCK=ICHAR(ENTRY(I)(11:11))*256+ICHAR(ENTRY(I)(12:12))
*
           CALL MDBFIND(IBLOCK,IREC,IXBLOKS,LKSIZE,MESAGE,LEN)     !1.2
           NDESCR=50000
           NOBS=50000
           IN=INDEX(MESAGE,BUFR)
           CALL DEBUFR(DESCR,VALUES,C,NDESCR,NOBS,MESAGE(IN:),.TRUE.)
           PRINT *,' '
         ENDIF
   20   CONTINUE

! IF BLOCK/RECORD NUMBER INPUT, ASSUME DATA MAY BE CHAINED         !1.2
! - DECODE ALL MESSAGES IN CHAIN                                   !1.2

      ELSE
   30   CALL MDBFIND(NBLOCK,N,IXBLOKS+NSQ,LKSIZE,MESAGE,LEN)       !1.2
        NDESCR=50000
        NOBS=50000
        IN=INDEX(MESAGE,BUFR)
        CALL DEBUFR(DESCR,VALUES,C,NDESCR,NOBS,MESAGE(IN:),.TRUE.)
        PRINT *,' '

! IF THERE'S A TRAILER AFTER THE MESSAGE, RESET BLOCK & RECORD     !1.2
! & DECODE THE NEXT MESSAGE IF THE POINTER IS NONZERO              !1.2

        IF (MESAGE(LEN-3:LEN).NE.SEVENS .AND.                      !1.2
     &      MESAGE(LEN-26:LEN-23).EQ.SEVENS) THEN                  !1.2
          N=ICHAR(MESAGE(LEN-2:LEN-2))                             !1.2
          NBLOCK=ICHAR(MESAGE(LEN-1:LEN-1))*256                    !1.2
     &          +ICHAR(MESAGE(LEN:LEN))                            !1.2
          IF (N.GT.0 .AND. NBLOCK.GT.0) GO TO 30                   !1.2
        ENDIF
      ENDIF
*
* GO ROUND AGAIN IF MORE INPUT, TO DECODE ANOTHER MESSAGE
*
      READ (*,*,END=99) NBLOCK,N,LKSIZE, DATIME
      GO TO 10
*
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
*                 (6) LENGTH OF MESSAGE                           !1.2*
*                                                                     *
***********************************************************************
      SUBROUTINE MDBFIND(NBLOCK,N,NIB,LKSIZE,MESAGE,LEN)          !1.2
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
      LEN=RECLEN(N)                                               !1.2
      RETURN
      END
