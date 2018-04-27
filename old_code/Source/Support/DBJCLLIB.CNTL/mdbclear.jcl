//M12CLEAR JOB (M12,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q
//*
//* TO EMPTY AN INDEX BLOCK IN AN MDB DATA SET & FREE ITS DATA BLOCKS
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000)',TIME.GO=(0,2),
//  PARM.GO='???,????,27998/'  Index block, number of entries, blocksiz
//*                         *******************************************
***********************************************************************
*                                                                     *
* PROGRAM       : MDBCLEAR                                            *
*                                                                     *
* PURPOSE       : TO CLEAR AN INDEX BLOCK IN AN MDB DATA SET          *
*                 & ANY REFERENCE TO IT IN THE MAP BLOCK              *
*                                                                     *
* CALLS         : PARM               INCLUDE MET.PROGLIB(CLPARM)      *
*                                                                     *
* PARAMETERS    : (1) NUMBER OF INDEX BLOCK TO BE CLEARED             *
* (PARM FIELD)       (NUMBERED FROM START OF DATA SET, BUT IGNORING   *
*                     ANY SEQUENCE BLOCK! I.E SAME AS NUMBERS IN MAP) *
*                 (2) NUMBER OF ENTRIES IN THAT INDEX BLOCK (TO CHECK)*
*                 (3) BLOCKSIZE                                       *
*                                                                     *
*Y2K  16.06.1997  MDBCLEAR is Year 2000 compliant.
*                                                                     *
***********************************************************************
      INTEGER*2 TIMTAG,COUNT                                       !1.2
      INTEGER BLOCKS,XBLOKS                                        !1.2
      INTEGER MAXBLK,BLKSIZ
      CHARACTER*1 MAP(65535)                                       !1.2
      CHARACTER*27998 INDEKS                                       !1.2
      CHARACTER*60 PARMS
      INTEGER*2 ZEROS(13999)                                       !1.2
      DATA ZEROS/13999*0/                                          !1.2
*
      CALL PARM(1,PARMS)                   ! READ PARM FIELD AS STRING
      READ (PARMS,*) NINDEX,NTRIES,BLKSIZ  ! READ 3 NUMBERS FROM STRING
*
* Read first block of data set to get number of blocks etc.        !1.2
* This read gets the map itself too if the data set is small;      !1.2
* if not, read in the map from the end of the data set.            !1.2
*
      OPEN (1,ACCESS='DIRECT',RECL=BLKSIZ) ! USE INPUT BLOCKSIZE IN OPEN
      READ (1,REC=1) (MAP(I),I=1,BLKSIZ)   ! & READ                !1.2
      BLOCKS=ICHAR(MAP(1))*256+ICHAR(MAP(2))                       !1.2
      XBLOKS=ICHAR(MAP(3))*256+ICHAR(MAP(4))                       !1.2
*
      IF (BLOCKS.LE.12000) THEN            ! map at start          !1.2
        NBLSEQ=ICHAR(MAP(8+BLOCKS))                                !1.2
      ELSE                                 ! map at end            !1.2
        NBLSEQ=ICHAR(MAP(9))                                       !1.2
        NMAPBLK=(BLOCKS+(BLKSIZ-1))/BLKSIZ                         !1.2
        DO N=1,NMAPBLK                                             !1.2
          READ (1,REC=BLOCKS-NMAPBLK+N) (MAP(8+I),                 !1.2
     &          I=(N-1)*BLKSIZ+1,MIN(N*BLKSIZ,BLOCKS))             !1.2
        ENDDO                                                      !1.2
      ENDIF                                                        !1.2
      IF (NBLSEQ.EQ.0) NSQ=0
      IF (NBLSEQ.GT.0) NSQ=1               ! NSQ=1 TO SKIP SEQ BLOCK
*
* Read start of index block & check against input number of entries.
*
      READ (1,REC=NSQ+NINDEX) TIMTAG,COUNT
      IF (COUNT.NE.NTRIES) THEN
        PRINT *,' NOT THE SAME COUNT - TRY AGAIN!'
        STOP
      ENDIF
*
* Release data blocks (& any index overflows) owned by index block
*
      DO N=8+XBLOKS+1,8+BLOCKS-1-NSQ-NMAPBLK                       !1.2
        IF (MAP(N).EQ.CHAR(NINDEX)) MAP(N)=CHAR(0)                 !1.2
        IF (MAP(N).EQ.CHAR(128+NINDEX)) MAP(N)=CHAR(0)             !1.2
      ENDDO                                                        !1.2
      MAP(7+NINDEX)=CHAR(0)                ! Zero data volume byte !1.2
*
* Zero index block & write back updated map
*
      WRITE (1,REC=NSQ+NINDEX) (ZEROS(I),I=1,BLKSIZ/2)
      WRITE (*,1) TIMTAG/256,TIMTAG-(TIMTAG/256)*256
    1 FORMAT(I3.2,'/',I2.2,'Z data deleted')

      IF (BLOCKS.LE.12000) THEN                                    !1.2
        WRITE (1,REC=1) (MAP(I),I=1,BLKSIZ)                        !1.2
      ELSE                                                         !1.2
        DO N=1,NMAPBLK                                             !1.2
          WRITE (1,REC=BLOCKS-NMAPBLK+N) (MAP(8+I),                !1.2
     &            I=(N-1)*BLKSIZ+1,MIN(N*BLKSIZ,BLOCKS))           !1.2
        ENDDO                                                      !1.2
      ENDIF                                                        !1.2
      STOP
      END
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
 INCLUDE PR(CLPARM)
/*
//*               ****************
//*               ** Set DsName **
//GO.FT01F001 DD DISP=SHR,DSN=MDB.???
//*                  *************
