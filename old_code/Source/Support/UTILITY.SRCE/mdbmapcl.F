***********************************************************************
*                                                                     *
* PROGRAM       : MDBMAP                                              *
*                                                                     *
* PURPOSE       : PRINT IDENTIFIER RANGES IN EACH DATA BLOCK OF MDB   *
*                 DATA SET BY SORTING INDEX ENTRIES                   *
*                                                                     *
* CALLS         : PARM, SORTCH                                        *
*                                                                     *
*Y2K  16.06.1997  MDBMAPCL IS YEAR 2000 COMPLIANT.
*                                                                     *
***********************************************************************
      INTEGER   BLKSIZ
      INTEGER*2 BLOCKS,XBLOKS,XHOURS,AMSTAR, OFLOWS,INDENT
      INTEGER*2 TIMTAG,NTRIES,NREPS, NOFLOW, IBLOCK
*
      PARAMETER (INDHED=6,MAXOFL=5,MAXBLK=27998)
      CHARACTER MAP*27998, FIRSTID*9,OFL*5, PARMIN*20
      CHARACTER*23 INDEKS(MAXOFL*MAXBLK/23) ! INDEX ENTRIES
      CHARACTER*23 MASK/'XXXXXXXXXXX'/ ! TO SORT ON BLOCK THEN IDENT
      COMMON /MDBUFS/ MAP,INDEKS
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
      CALL PARM(1,PARMIN)
      READ (PARMIN,*) BLKSIZ
*
      OPEN (1,ACCESS='DIRECT',RECL=BLKSIZ)                              00030018
      READ (1,REC=1) BLOCKS,XBLOKS,XHOURS,AMSTAR,
     -               MAP(:BLKSIZ-12),OFLOWS,INDENT
      NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
      NBLSEQ=ICHAR(MAP(BLOCKS:BLOCKS))
      IF (NBLSEQ.EQ.0) NSQ=0
      IF (NBLSEQ.GT.0) NSQ=1               ! NSQ=1 TO SKIP SEQ BLOCK
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
      DO 110 NINDEX=1+NSQ+1,1+NSQ+XBLOKS
*
* READ INDEX BLOCK(S). IF THERE IS A CONTINUATION INDEX BLOCK, IT HAS
* THE SAME FORMAT AS THE FIRST; ITS ENTRIES FOLLOW ON IN THE ARRAY,
* NTRIES BEING THE TOTAL NUMBER OF ENTRIES INCLUDING ANY CONTINUATION.
*
* (N.B. THERE'S NO CHECK HERE FOR CURRENT DATA, SO BEWARE IF THERE ARE
*       BREAKS IN THE TIME SEQUENCE, IE. GAPS IN THE DATA!)
*
      IX=NINDEX
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
* PRINT DAY/HOUR OF INDEX BLOCK
*
      IDAY=TIMTAG/256
      IHOUR=TIMTAG-IDAY*256
      WRITE (*,1) IDAY,IHOUR, NTRIES,NREPS
    1 FORMAT (/I3.2,'/',I2.2,'Z',I9,' INDEX ENTRIES',I9,' OBS'/)
*
* SORT INDEX ENTRIES INTO ORDER OF BLOCK NUMBER & IDENT, COPYING BLOCK
* NUMBER TO START OF ENTRY TO SORT ON IT FIRST & THEN IDENT.
*
      DO I=1,NTRIES
       INDEKS(I)(1:2)=INDEKS(I)(22:23)
      ENDDO
*
      L=INDENT
      N=NTRIES
      CALL SORTCH(INDEKS,L,N,MASK)
*
* PRINT IDENTIFIER RANGES BLOCK BY BLOCK
*
      FIRSTID=INDEKS(1)(3:11)
      NINBLK=ICHAR(INDEKS(1)(12:12))
      OFL=' '                           ! SET TO + IF OVERFLOWS REACHED
      DO 100 I=2,NTRIES
       N=ICHAR(INDEKS(I)(12:12))
       IF (I.EQ.NTRIES .OR. INDEKS(I)(22:23).NE.INDEKS(I-1)(22:23)) THEN
         IF (I.EQ.NTRIES) NINBLK=NINBLK+N
         READ (INDEKS(I-1)(22:23),'(A2)') IBLOCK
         IF (IBLOCK.GE.BLOCKS-1-NSQ-XBLOKS-OFLOWS) OFL='+'
         PRINT *,IBLOCK,OFL,FIRSTID,INDEKS(I-1)(3:11),NINBLK,'OBS'
         NINBLK=0
         FIRSTID=INDEKS(I)(3:11)
       ENDIF
       NINBLK=NINBLK+N
  100 CONTINUE
  110 CONTINUE
      STOP
      END
