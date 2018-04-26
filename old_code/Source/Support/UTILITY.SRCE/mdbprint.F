***********************************************************************
*                                                                     *
* PROGRAM       : MDBPRINT                                            *
*                                                                     *
* PURPOSE       : PRINT MDB DATA SET (OR COPY IT)                     *
*                                                                     *
* CALLS         : PARM, DATE31, JSL82 (& TAFREP OR AIRSTO IF MODE='S')*
*                                                                     *
* PARAMETERS    : PARM='BLOCK,NUMBER,BLOCKSIZE,MODE'                  *
*                  MODE='P': PRINT OBS (BEST FOR TAFS & METARS)       *
*                  MODE='N': PRINT OBS (BETTER FOR SYNOPS, INCLUDES   *
*                                        COUNT OF GOOD VALUES)        *
*                  MODE='S': COPY DATA USING TAFREP                   *
*                  MODE='A': COPY DATA USING AIRSTO                   *
*                             (ASSUMES INPUT & OUTPUT BLOCKSIZE SAME) *
* --------- N.B. TO COPY U/A DATA ADD BYTE 17 TO IDENT  ------------  *
* --------- IN TAFREP CALL (USE ALTERNATIVE LINE AT END)------------  *
*                                                                     *
*Y2K  16.06.1997  MDBPRINT IS YEAR 2000 COMPLIANT.
*Y2K                       ROUTINE CONTAINS DATE MANAGEMENT.
*                                                                     *
* CHANGE RECORD:                                                      *
*   APR 97 - ALLOW FOR ZERO POINTERS (SKIPPING RECORD)               !A
*                                                                     *
***********************************************************************
      CHARACTER PARMIN*20, MODE*1, SP*74/' '/
      INTEGER   INDHED,BLKSIZ, NOW(9),CENDAY,CENTHR, DATIME(5),TOR(5)
*
      INTEGER*2 TIMTAG,NTRIES,NREPS
      INTEGER*2 BLKTAG,NINBLK,LEFT,RECLEN(9999),NOFLOW, RECDIS(9999)
      INTEGER*2 BLOCKS,XBLOKS,XHOURS,AMSTAR, OFLOWS,INDENT
      INTEGER*2 LAT,LONG, ITOR, IBLOCK,IREC, MASK(12)/0,5*-1,6*0/
*
* RECLEN IS THE ARRAY OF REPORT LENGTH SLOTS AFTER THE BLOCK HEADER.
* ITS DIMENSION IS ARBITRARY, IT ONLY NEEDS TO BE BIG ENOUGH FOR THE
* REPORTS THAT WILL FIT IN A BLOCK.  WE DON'T KNOW WHERE THE LENGTHS
* WILL END AND THE REPORTS, SLOTTED IN FROM THE END, WILL MEET THEM!
*
      PARAMETER (INDHED=6, MAXOFL=5,MAXNID=1216)
      CHARACTER*27998 MAP,BLOCK
      CHARACTER*23 INDEKS(MAXOFL*MAXNID) ! INDEX ENTRIES
      DATA CENTHR/0/
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
     &               MAP(:BLKSIZ-12),OFLOWS,INDENT
      NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
      NBLSEQ=ICHAR(MAP(BLOCKS:BLOCKS))
      IF (NBLSEQ.EQ.0) NSQ=0
      IF (NBLSEQ.GT.0) NSQ=1               ! NSQ=1 TO SKIP SEQ BLOCK
*
      IF (MODE.EQ.'S' .OR. MODE.EQ.'A') THEN
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
      IF (CENTHR.EQ.0) THEN                                          !B
        CALL DATE31(IDAY,IMONTH,IYEAR,CENDAY)                        !B
        CENTHR=(CENDAY-1)*24+IHR                                     !B
      ENDIF                                                          !B
*
* SORT INDEX ENTRIES INTO ALPHABETICAL ORDER
*
      IF (MODE.NE.'S' .AND. MODE.NE.'A') THEN                        !B
        L=INDENT
        N=NTRIES
        CALL JSL82(INDEKS,L,N,MASK)
      ENDIF                                                          !B
*
* THE INDEX ENTRIES ARE STANDARD 23-BYTE ENTRIES POINTING TO THE MOST
* RECENT REPORT IN A CHAIN GOING BACK THROUGH THE PERIOD COVERED BY THE
* INDEX BLOCK.  EACH REPORT IS FOLLOWED BY 23 BYTES MODELLED ON AN INDEX
* ENTRY & ENDING WITH A POINTER TO THE NEXT REPORT IN THE CHAIN.  THE
* 9-BYTE IDENTIFIER FIELD IS USED FOR BULLETIN DETAILS, AND IS SET TO
* TTAAII(3:6)//AMDCOR//CCCC, WHERE AMDCOR=(AMD NUMBER)*16+(COR NUMBER).
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
       IF (IBLOCK.GT.0 .AND. IREC.GT.0) THEN                         !A
         IF (MODE.NE.'S' .AND. MODE.NE.'A') THEN
           WRITE (*,1) INDEKS(INT)(3:11), REALAT,REALON
    1      FORMAT (' ',A9,2F7.2)
         ENDIF
*
* READ DATA BLOCK IF IT'S NOT IN CORE.
*
   15    IF (IBLOCK.NE.NBLOCK) THEN
           NBLOCK=IBLOCK
           READ (1,REC=1+NSQ+XBLOKS+NBLOCK) BLKTAG,NINBLK,LEFT,
     &     (RECLEN(I),I=1,NINBLK),BLOCK(1+6+2*NINBLK:BLKSIZ)
           RECDIS(1)=RECLEN(1)
           DO I=2,NINBLK
             RECDIS(I)=RECDIS(I-1)+RECLEN(I)
           ENDDO
         ENDIF
*
* THE TIME MAY NOT BE IN THE REPORT, SO GET IT FROM THE FIRST BYTES OF
* THE "INDEX ENTRY" ON THE END (SET FROM THE BULLETIN HEADER IF NEC.)
*
   30    I=BLKSIZ-RECDIS(IREC)
         L=RECLEN(IREC)
         IHOUR=MOD(ICHAR(BLOCK(I+L-22:I+L-22)),32)+IHR
         IT=IHOUR*100+ICHAR(BLOCK(I+L-21:I+L-21)) ! MINUTES AS WELL
         NL=ICHAR(BLOCK(I+L-11:I+L-11)) ! NUMBER OF TEMPS & WINDS IF U/A
***********************************************************************
*
* FINALLY EITHER PRINT THE REPORTS OR PASS THEM TO TAFREP FOR STORAGE
*
***********************************************************************
         ITOR=ICHAR(BLOCK(I+L-5:I+L-5))*256+ICHAR(BLOCK(I+L-4:I+L-4)) !B
         IF (ITOR.GT.65535-20) ITOR=ITOR-65536  ! BEFORE NOMINAL HOUR !B

! TIME OF RECEIPT, IN MINUTES FROM INDEX TIME, IS STORED IN 16 BITS:
! IT RANGES FROM A SMALL NEGATIVE NUMBER (SYNOP A FEW MINUTES BEFORE
! NOMINAL TIME) TO A DELAY OF A MONTH OR MORE (CLIMAT, TESAC).  THE
! ABOVE ASSUMES NOT EARLY UNLESS WITHIN 20 MINUTES OF NOMINAL HOUR.
! SET DATE OF RECEIPT BY ADDING DISPLACEMENT TO CENTURY-HOUR OF
! INDEX BLOCK & CONVERTING BACK TO DATE.

         CALL DATE13((CENTHR+ITOR/60)/24+1,                          !B
     &                             TOR(3),TOR(2),TOR(1))             !B
         TOR(4)=MOD(CENTHR+ITOR/60,24)                               !B
         TOR(5)=ITOR-(ITOR/60)*60                                    !B
         ITR=TOR(4)*100+TOR(5)                                       !B
*
* SEE WHETHER AMEND OR COR
*
         IF (MODE.NE.'S' .AND. MODE.NE.'A') THEN
           NAC=ICHAR(BLOCK(I+L-16:I+L-16))
           IF (NAC.GT.0) THEN
             NAMD=NAC/16
             NCOR=MOD(NAC,16)
             IF (NAMD.GT.0) PRINT *,'   AMD'
             IF (NCOR.GT.0) PRINT *,'   COR'
           ENDIF
*
* PRINT UP TO 80 BYTES OF REPORT, STARTING WITH DATE/TIME OF DATA AND
* TIME OF RECEIPT; OPTION 'N' PRINTS NUMBER OF GOOD VALUES TOO.
*
           IF (MODE.EQ.'P') THEN
             IF (L.LT.95) THEN
               WRITE (*,2) IDAY,IT,TOR(3),ITR,
     &                     BLOCK(I+1:I+L-23)//SP(1:95-L)
             ELSE
               WRITE (*,2) IDAY,IT,TOR(3),ITR,BLOCK(I+1:I+72)
             ENDIF
           ELSE IF (MODE.EQ.'N') THEN
             IF (L.LT.103) THEN
               WRITE (*,3) IDAY,IT,TOR(3),ITR,NL,
     &                     BLOCK(I+1:I+L-23)//SP(1:103-L)
             ELSE
               WRITE (*,3) IDAY,IT,TOR(3),ITR,NL,BLOCK(I+1:I+80)
             ENDIF
           ENDIF
    2      FORMAT (1X,I2.2,'/',I4.4,1X,I2.2,'/',I4.4,1X,A72)
    3      FORMAT (4X,I2.2,'/',I4.4,'Z    TOR ',I2.2,'/',I4.4,'Z',
     &           I6,' VALUES   ',A80)
*
* TO STORE (IN A REMADE DATA BASE) SET DATE/TIME & CALL TAFREP
* (NO NEED TO SET MINUTES - JUST STORE THEM AGAIN IN TRAILER)
* N.B. U/A STORAGE NEEDS BYTE 17 ON END OF IDENT - USE COMMENTED LINE!
*
         ELSE
           DATIME(1)=IYEAR
           DATIME(2)=IMONTH
           DATIME(3)=IDAY
           DATIME(4)=IHOUR
           DATIME(5)=0

           IF (MODE.EQ.'A') THEN
             CALL AIRSTO(DATIME,BLOCK(I+L-22:I+L),BLOCK(I+1:I+L-23),
     &                   2,BLKSIZ,INDEKS(INT)(3:11),TOR)
           ELSE
             CALL TAFREP(DATIME,BLOCK(I+L-22:I+L),BLOCK(I+1:I+L-23),
     &                   2,BLKSIZ,INDEKS(INT)(3:11))
*    &           BLKSIZ,INDEKS(INT)(3:11)//INDEKS(INT)(17:17)) ! IF U/A !
           ENDIF
         ENDIF
*
* GET RECORD NUMBER OF NEXT REPORT IN CHAIN (BLOCK NUMBER MAY BE DIFF)
*
         READ (BLOCK(I+L-3:I+L-2),'(A2)') IREC
         READ (BLOCK(I+L-1:I+L),'(A2)') IBLOCK ! CHAIN MAY CROSS BLOCKS!
         IF (IREC.NE.0) GO TO 15
       ENDIF                                                          !A
  100 CONTINUE
      IF (MODE.NE.'S' .AND. MODE.NE.'A') PRINT *,' '
      CENTHR=CENTHR+XHOURS  ! CENTURY-HOUR OF NEXT INDEX BLOCK
  110 CONTINUE
      STOP
      END
