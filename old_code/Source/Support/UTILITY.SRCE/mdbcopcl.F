***********************************************************************
*                                                                     *
* PROGRAM       : MDBPRAMS                                            *
*                                                                     *
* PURPOSE       : PRINT/COPY MDB DATA SET WRITTEN TO BY AMDSTO        *
*                                                                     *
* CALLS         : PARM, DATE31, JSL82 (& AMDSTO IF STORAGE REQUESTED) *
*                                                                     *
* PARAMETERS    : PARM='BLOCK,NUMBER,BLOCKSIZE,MODE'                  *
*                  MODE='P': PRINT OBS                                *
*                  MODE='S': COPY DATA (ASSUMES 23-BYTE INDEX ENTRIES,*
*                                      INPUT & OUTPUT BLOCKSIZE SAME) *
*                                                                     *
*Y2K  16.06.1997  MDBCOPCL IS YEAR 2000 COMPLIANT.
*Y2K                       ROUTINE CONTAINS DATE MANAGEMENT.
*                                                                     *
***********************************************************************
      CHARACTER PARMIN*20, MODE*1, SP*74/' '/
      INTEGER   INDHED,BLKSIZ, NOW(9),CENDAY,CENTHR, DATIME(5),TOR(5)
*
      INTEGER*2 TIMTAG,NTRIES,NREPS
      INTEGER*2 BLKTAG,NINBLK,LEFT,RECLEN(9999),NOFLOW, RECDIS(9999)
      INTEGER*2 BLOCKS,XBLOKS,XHOURS,AMSTAR, OFLOWS,INDENT
      INTEGER*2 LAT,LONG, IBLOCK,IREC, MASK(12)/0,-1,-1,9*0/
      LOGICAL COR
*
* RECLEN IS THE ARRAY OF REPORT LENGTH SLOTS AFTER THE BLOCK HEADER.
* ITS DIMENSION IS ARBITRARY, IT ONLY NEEDS TO BE BIG ENOUGH FOR THE
* REPORTS THAT WILL FIT IN A BLOCK.  WE DON'T KNOW WHERE THE LENGTHS
* WILL END AND THE REPORTS, SLOTTED IN FROM THE END, WILL MEET THEM!
*
      PARAMETER (INDHED=6, MAXOFL=5,MAXNID=672)
      CHARACTER*15476 MAP,BLOCK
      CHARACTER*23 INDEKS(MAXOFL*MAXNID) ! INDEX ENTRIES
      COMMON /MDBUFS/ MAP, RECLEN,BLOCK, RECDIS, INDEKS
*
* READ IBL & NOBL (INDICATING THE INDEX BLOCK(S) WANTED) & BLOCKSIZE
* FROM PARM='FIRST INDEX BLOCK, NUMBER OF BLOCKS, BLOCK SIZE, MODE',
* WHERE MODE (MAY NOT BE SET) IS 'P' FOR PRINT & 'S' FOR STORE - OR
* 'N' TO PRINT COUNTS IN SYNOP & U/A TRAILERS (NUMBER OF GOOD VALUES)
*
      CALL PARM(1,PARMIN)
      READ (PARMIN,*) IBL,NOBL,BLKSIZ,MODE
*
* IF FIRST TIME, READ IN MAP BLOCK (TO GET NUMBER OF INDEX BLOCKS)
*
* MAP BLOCK:             (THE BYTE FOR EACH BLOCK IS SET TO ITS INDEX
*                        BLOCK NUMBER - SO LESS THAN 256*XHOURS DATA!)
* ------------------------------------------------------------ - - - -
* : NO. OF : NO. OF : HOURS : START OF : FIRST :  2ND  :     : 1ST   :
* : BLOCKS : INDEX  : PER   : 1ST SLOT : INDEX : INDEX :     : DATA  :
* : IN D/S : BLOCKS : BLOCK : AFTER 0Z : BLOCK : BLOCK :     : BLOCK :
* ------------------------------------------------------------ - - - -
* 0        2        4       6          8       9      10    8+XBLOKS
*
      OPEN (1,ACCESS='DIRECT',RECL=BLKSIZ)                              00030018
      READ (1,REC=1) BLOCKS,XBLOKS,XHOURS,AMSTAR,
     -               MAP(:BLKSIZ-12),OFLOWS,INDENT
      NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
      NBLSEQ=ICHAR(MAP(BLOCKS:BLOCKS))
      IF (NBLSEQ.EQ.0) NSQ=0
      IF (NBLSEQ.GT.0) NSQ=1               ! NSQ=1 TO SKIP SEQ BLOCK
*
      IF (MODE.EQ.'S') THEN
        OPEN (2,ACCESS='DIRECT',RECL=BLKSIZ)                            00030018
      ENDIF
*
* IF IBL=0, READ THE WHOLE DATA SET; IBL<100 IS AN INDEX BLOCK NUMBER;
* IBL=100 OR MORE IS DDHH FOR AN INDEX BLOCK, SO WORK OUT BLOCK NUMBER,
* WHICH INVOLVES WORKING OUT THE FULL DATE FROM DD & THE CURRENT DATE.
*
      CALL DATIM(NOW)
      IF (IBL.GE.100) THEN
        IDAY=IBL/100
        IHOUR=MOD(IBL,100)
*
* SET YEAR & MONTH FROM DAY
*
        IYEAR=NOW(8)
        IMONTH=NOW(7)
        IF (IDAY.GT.NOW(6)) THEN
          IMONTH=IMONTH-1
          IF (IMONTH.EQ.0) THEN
            IYEAR=IYEAR-1
            IMONTH=12
          ENDIF
        ENDIF
*
* AND FIND INDEX BLOCK NUMBER FROM CENTURY-HOUR
*
        CALL DATE31(IDAY,IMONTH,IYEAR,CENDAY)
        N=XHOURS
        INDHOR=MOD(IHOUR+24-AMSTAR,N)
        CENTHR=(CENDAY-1)*24+IHOUR
        N=XBLOKS
        IBL=MOD((CENTHR-INDHOR)/XHOURS,N)+2+NSQ
*
* IF IBL IS BETWEEN 0 & 100, IT'S A BLOCK NUMBER ALREADY. IF IBL=0,
* EVERYTHING IS WANTED, SO SET RANGE OF LOOP FROM INDEX BLOCK COUNT.
*
      ELSE IF (IBL.EQ.0) THEN
        IBL=2+NSQ
        NOBL=XBLOKS
      ENDIF
***********************************************************************
*
* INDEX BLOCK:
* ----------------------------------------------------------- - - - -
* : DATE/ : NO. OF : LENGTH FREE : FIRST 23-   : SECOND   :
* : TIME  : ENTRIES: IN LAST BLK : BYTE ENTRY  : ENTRY    :
* ----------------------------------------------------------- - - - -
* 0       2        4             6            29         52
*
* READ IN INDEX BLOCK FOR SLOT & SORT ENTRIES INTO IDENTIFIER ORDER.
*
***********************************************************************
      IF (NOBL.GT.0) NOBL=NOBL-1
      DO 110 NINDEX=IBL,IBL+NOBL
*
* READ INDEX BLOCK(S). IF THERE IS A CONTINUATION INDEX BLOCK, IT HAS
* THE SAME FORMAT AS THE FIRST; ITS ENTRIES FOLLOW ON IN THE ARRAY,
* NTRIES BEING THE TOTAL NUMBER OF ENTRIES INCLUDING ANY CONTINUATION.
*
* (N.B. THERE'S NO CHECK HERE FOR CURRENT DATA, SO BEWARE IF THERE ARE
*       BREAKS IN THE TIME SEQUENCE, IE. GAPS IN THE DATA!)
*
      IX=NINDEX
      IF (IX.GT.1+NSQ+XBLOKS) IX=IX-XBLOKS
      NIND=1                               ! CONTINUATION NUMBER
   10 NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN
      READ (1,REC=IX) TIMTAG,NTRIES,NREPS,
     -                (INDEKS(I),I=NIBL+1,NIBL+NBLIND),NOFLOW
      IF (NOFLOW.GT.0) THEN
        IX=NOFLOW+NSQ
        NIND=NIND+1
        GO TO 10
      ENDIF
*
      IDAY=TIMTAG/256
      IHR=TIMTAG-IDAY*256
*
* GET FULL DATE (TO PASS TO STORAGE PROGRAM) AS ABOVE, FROM DAY OF DATA
*                                                        & CURRENT DATE.
      IYEAR=NOW(8)
      IMONTH=NOW(7)
      IF (IDAY.GT.NOW(6)) THEN
        IMONTH=IMONTH-1
        IF (IMONTH.EQ.0) THEN
          IYEAR=IYEAR-1
          IMONTH=12
        ENDIF
      ENDIF
*
* SORT INDEX ENTRIES INTO ALPHABETICAL ORDER IF PRINT IS TO BE DONE
*
      IF (MODE.NE.'S') THEN
        L=INDENT
        N=NTRIES
        CALL JSL82(INDEKS,L,N,MASK)
      ENDIF
*
* THE INDEX ENTRIES ARE STANDARD 23-BYTE ENTRIES WITH NO CHAINING
*
      DO 100 INT=1,NTRIES
       N=ICHAR(INDEKS(INT)(12:12))
       READ (INDEKS(INT)(13:14),'(A2)') LAT
       READ (INDEKS(INT)(15:16),'(A2)') LONG
       REALAT=LAT/100.
       REALON=LONG/100.
       READ (INDEKS(INT)(20:21),'(A2)') IREC
       READ (INDEKS(INT)(22:23),'(A2)') IBLOCK
*
       IF (MODE.NE.'S') THEN
         WRITE (*,1) INDEKS(INT)(3:11), REALAT,REALON
    1    FORMAT (' ',A9,2F7.2)
       ENDIF
*
* READ DATA BLOCK IF IT'S NOT IN CORE.
*
       IF (IBLOCK.NE.NBLOCK) THEN
         NBLOCK=IBLOCK
         READ (1,REC=1+NSQ+XBLOKS+NBLOCK) BLKTAG,NINBLK,LEFT,
     -     (RECLEN(I),I=1,NINBLK),BLOCK(1+6+2*NINBLK:BLKSIZ)
         RECDIS(1)=RECLEN(1)
         DO 20 I=2,NINBLK
   20     RECDIS(I)=RECDIS(I-1)+RECLEN(I)
       ENDIF
*
* THE TIME MAY NOT BE IN THE REPORT, SO GET IT FROM THE FIRST BYTES OF
* THE "INDEX ENTRY" ON THE END (SET FROM THE BULLETIN HEADER IF NEC.)
*
       I=BLKSIZ-RECDIS(IREC)
       L=RECLEN(IREC)
***********************************************************************
*
* FINALLY EITHER PRINT THE REPORTS OR PASS THEM TO AMDSTO FOR STORAGE
*
***********************************************************************
       IF (MODE.NE.'S') THEN
         IF (L.LT.95) THEN
           WRITE (*,2) IDAY,IT,ITR,BLOCK(I+1:I+L-23)//SP(1:95-L)
         ELSE
           WRITE (*,2) IDAY,IT,ITR,BLOCK(I+1:I+72)
         ENDIF
    2    FORMAT (1X,I2.2,'/',I4.4,1X,I4.4,1X,A72)
*
* TO STORE (IN A REMADE DATA BASE) SET DATE/TIME & CALL AMDSTO
*
       ELSE
         DATIME(1)=IYEAR
         DATIME(2)=IMONTH
         DATIME(3)=IDAY
         DATIME(4)=IHR+MOD(ICHAR(INDEKS(INT)(1:1)),64)
         DATIME(5)=ICHAR(INDEKS(INT)(2:2))
*
         TOR(1)=IYEAR
         TOR(2)=IMONTH
         TOR(3)=IDAY
         ITOR=ICHAR(INDEKS(INT)(18:18))*256+ICHAR(INDEKS(INT)(19:19))
         TOR(4)=IHR+ITOR/60
         IF (TOR(4).GE.24) THEN
           TOR(3)=TOR(3)+TOR(4)/24
           TOR(4)=TOR(4)-24
         ENDIF
         TOR(5)=MOD(ITOR,60)
*
         ICCCC=ICHAR(BLOCK(I+9:I+9))
         COR=.FALSE.
         IF (ICHAR(INDEKS(INT)(1:1)).GE.128) COR=.TRUE.
         CALL AMDSTO(BLOCK(I+1:I+L),L,INDEKS(INT),DATIME,
     -               ICCCC,COR,2,0,TOR)
       ENDIF
  100 CONTINUE
      IF (MODE.NE.'S') PRINT *,' '
  110 CONTINUE
      STOP
      END
