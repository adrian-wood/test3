***********************************************************************
*                                                                     *
* PROGRAM       : MDBIND23                                            *
*                                                                     *
* PURPOSE       : PRINT MDB INDEX (23-BYTE ENTRIES AS THEY ARE)       *
*                                                                     *
* CALLS         : PARM, DATE31, SORTCH                                *
*                                                                     *
* PARAMETERS    : PARM='BLOCK,NUMBER,BLOCKSIZE'                       *
*                                                                     *
* CHANGE RECORD:                                                      *
*   MAY 98 - MADE FROM MDBPRINT                                       *
*                                                                     *
***********************************************************************
      CHARACTER PARMIN*20, SP*74/' '/
      INTEGER   INDHED,BLKSIZ, NOW(9),CENDAY,CENTHR, DATIME(5)
*
      INTEGER*2 TIMTAG,NTRIES,NREPS
      INTEGER*2 BLKTAG,NINBLK,LEFT,NOFLOW
      INTEGER*2 BLOCKS,XBLOKS,XHOURS,AMSTAR, OFLOWS,INDENT
      INTEGER*2 LAT,LONG, ITOR, IBLOCK,IREC, MASK(12)/0,8*-1,3*0/
*
* RECLEN IS THE ARRAY OF REPORT LENGTH SLOTS AFTER THE BLOCK HEADER.
* ITS DIMENSION IS ARBITRARY, IT ONLY NEEDS TO BE BIG ENOUGH FOR THE
* REPORTS THAT WILL FIT IN A BLOCK.  WE DON'T KNOW WHERE THE LENGTHS
* WILL END AND THE REPORTS, SLOTTED IN FROM THE END, WILL MEET THEM!
*
      PARAMETER (INDHED=6,MAXOFL=9,MAXNID=1216)
      CHARACTER*27998 MAP,BLOCK
      CHARACTER*23 INDEKS(MAXOFL*MAXNID) ! INDEX ENTRIES
*
* READ IBL & NOBL (INDICATING THE INDEX BLOCK(S) WANTED) & BLOCKSIZE
* FROM PARM='FIRST INDEX BLOCK, NUMBER OF BLOCKS, BLOCK SIZE'.
*
      CALL PARM(1,PARMIN)
      READ (PARMIN,*) IBL,NOBL,BLKSIZ
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
     &               MAP(:BLKSIZ-12),OFLOWS,INDENT
      NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
      NBLSEQ=ICHAR(MAP(BLOCKS:BLOCKS))
      IF (NBLSEQ.EQ.0) NSQ=0
      IF (NBLSEQ.GT.0) NSQ=1               ! NSQ=1 TO SKIP SEQ BLOCK
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
     &                (INDEKS(I),I=NIBL+1,NIBL+NBLIND),NOFLOW
      IF (NOFLOW.GT.0) THEN
        IX=NOFLOW+NSQ
        NIND=NIND+1
        GO TO 10
      ENDIF
*
      IDAY=TIMTAG/256
      IHR=TIMTAG-IDAY*256
*
* SORT INDEX ENTRIES INTO ALPHABETICAL ORDER
*
      L=INDENT
      N=NTRIES
      CALL JSL82(INDEKS,L,N,MASK)
*
* THE INDEX ENTRIES ARE STANDARD 23-BYTE ENTRIES POINTING TO THE MOST
* RECENT REPORT IN A CHAIN GOING BACK THROUGH THE PERIOD COVERED BY THE
* INDEX BLOCK.  EACH REPORT IS FOLLOWED BY 23 BYTES MODELLED ON AN INDEX
* ENTRY & ENDING WITH A POINTER TO THE NEXT REPORT IN THE CHAIN.  THE
* 9-BYTE IDENTIFIER FIELD IS USED FOR BULLETIN DETAILS, AND IS SET TO
* TTAAII(3:6)//AMDCOR//CCCC, WHERE AMDCOR=(AMD NUMBER)*16+(COR NUMBER).
*
      DO INT=1,NTRIES-1
        IF (INDEKS(INT)(1:17).EQ.INDEKS(INT+1)(1:17)) THEN
          PRINT *,INDEKS(INT),IDAY,IHR
        ENDIF
      ENDDO
      PRINT *,IDAY,IHR,NTRIES,'ENTRIES'
  110 CONTINUE
      STOP
      END
