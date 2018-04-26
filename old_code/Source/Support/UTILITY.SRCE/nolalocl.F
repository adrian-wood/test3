***********************************************************************
*                                                                     *
* PROGRAM       : NOLALO                                              *
*                                                                     *
* PURPOSE       : PRINT IDENTIFIERS OF AIRFIELDS WITH NO LAT/LONG     *
*                                                                     *
*Y2K  16.06.1997  NOLALOCL IS YEAR 2000 COMPLIANT.
*Y2K                       ROUTINE CONTAINS DATE MANAGEMENT.
*                                                                     *
***********************************************************************
      CHARACTER PARMIN*12
      INTEGER   INDHED,BLKSIZ, NOW(9),CENDAY,CENTHR
*
      INTEGER*2 TIMTAG,NTRIES,NREPS
      INTEGER*2 BLKTAG,NINBLK,LEFT,RECLEN(999),NOFLOW, RECDIS(999)
      INTEGER*2 BLOCKS,XBLOKS,XHOURS,AMSTAR, OFLOWS,INDENT
      INTEGER*2 LAT,LONG, ITOR, IBLOCK,IREC, MASK(12)/0,-1,-1,9*0/
*
* RECLEN IS THE ARRAY OF REPORT LENGTH SLOTS AFTER THE BLOCK HEADER.
* ITS DIMENSION IS ARBITRARY, IT ONLY NEEDS TO BE BIG ENOUGH FOR THE
* REPORTS THAT WILL FIT IN A BLOCK.  WE DON'T KNOW WHERE THE LENGTHS
* WILL END AND THE REPORTS, SLOTTED IN FROM THE END, WILL MEET THEM!
*
      PARAMETER (INDHED=6, MAXOFL=5,MAXNID=1020)
      CHARACTER*23476 MAP,BLOCK
      CHARACTER*23 INDEKS(MAXOFL*MAXNID) ! INDEX ENTRIES
      COMMON /MDBUFS/ MAP, RECLEN,BLOCK, RECDIS, INDEKS
*
* READ IBL (INDEX BLOCK(S) WANTED) & BLOCKSIZE FROM PARM='IBL,BLKSIZ/'
*
      CALL PARM(1,PARMIN)
      READ (PARMIN,*) IBL,BLKSIZ
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
     -                 MAP(:BLKSIZ-12),OFLOWS,INDENT
      NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
*
* IF IBL=0, READ THE WHOLE DATA SET; IBL<100 IS AN INDEX BLOCK NUMBER;
* IBL=100 OR MORE IS DDHH FOR AN INDEX BLOCK, SO WORK OUT BLOCK NUMBER.
*
      IF (IBL.GE.100) THEN
        IDAY=IBL/100
        IHOUR=MOD(IBL,100)
        CALL DATIM(NOW)
        IF (IDAY.GT.NOW(6)) NOW(7)=NOW(7)-1
        IF (NOW(7).EQ.0) THEN
          NOW(8)=NOW(8)-1
          NOW(7)=12
        ENDIF
*
        CALL DATE31(IDAY,NOW(7),NOW(8),CENDAY)
        N=XHOURS
        INDHOR=MOD(IHOUR+24-AMSTAR,N)
        CENTHR=(CENDAY-1)*24+IHOUR
        N=XBLOKS
        IXBLOK=MOD((CENTHR-INDHOR)/XHOURS,N)+2
        NXBLOK=IXBLOK
      ELSE IF (IBL.GT.0) THEN
        IXBLOK=IBL
        NXBLOK=IXBLOK
      ELSE
        IXBLOK=2
        NXBLOK=XBLOKS+1
      ENDIF
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
      DO 110 NINDEX=IXBLOK,NXBLOK
*
* READ INDEX BLOCK(S). IF THERE IS A CONTINUATION INDEX BLOCK, IT HAS
* THE SAME FORMAT AS THE FIRST; ITS ENTRIES FOLLOW ON IN THE ARRAY,
* NTRIES BEING THE TOTAL NUMBER OF ENTRIES INCLUDING ANY CONTINUATION.
*
      IX=NINDEX
      NIND=1                               ! CONTINUATION NUMBER
   10 NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN
      READ (1,REC=IX) TIMTAG,NTRIES,NREPS,
     -                (INDEKS(I),I=NIBL+1,NIBL+NBLIND),NOFLOW
      IF (NOFLOW.GT.0) THEN
        IX=NOFLOW
        NIND=NIND+1
        GO TO 10
      ENDIF
*
      IDAY=TIMTAG/256
      IHR=TIMTAG-IDAY*256
      L=INDENT
      N=NTRIES
      CALL JSL82(INDEKS,L,N,MASK)
*
* THE INDEX ENTRIES ARE STANDARD 23-BYTE ENTRIES. PRINT ID IF NO LAT.
*
      DO 100 INT=1,NTRIES
       READ (INDEKS(INT)(13:14),'(A2)') LAT
       READ (INDEKS(INT)(15:16),'(A2)') LONG
       IF (LAT.EQ.-32768) PRINT *,INDEKS(INT)(3:7)
  100 CONTINUE
  110 CONTINUE
      STOP
      END
