***********************************************************************
*                                                                     *
* PROGRAM       : MDBPRIN       (LIKE MDBPRINT BUT NOT CHAINED)       *
*                                                                     *
* PURPOSE       : PRINT MDB DATA SET     (WRITTEN BY AMDSTO)          *
*                                                                     *
* CALLS         : PARM, DATE31, SORTCH                                *
*                                                                     *
* PARAMETERS    : PARM='BLOCK,NUMBER,BLOCKSIZE,MODE'                 !B
*                  MODE='P' FOR PRINT, 'S' FOR STORAGE (COPY)        !B
*                                                                     *
*Y2K  16.06.1997  MDBPRIN IS YEAR 2000 COMPLIANT.
*Y2K                      ROUTINE CONTAINS DATE MANAGEMENT.
*                                                                     *
* CHANGE RECORD:                                                      *
*  APR 97 - CHECK FOR ZERO POINTERS IN INDEX (SKIPPING RECORDS)      !A
*  OCT 98 - MAKE IT COPY DATA LIKE MDBPRINT (FOR USE BY MDBXTEND)    !B
*                                                                     *
***********************************************************************
!
!$Log:
! 1    Met_DB_Project 1.0         28/02/2006 12:07:38    Sheila Needham  
!$
!Revision 1.2  1999/06/22 10:40:57  usmdb
!22 June 1999      C Long
!1.2 Change blocksize of 15476 to 27998
!
!$Revision: 1$
!$Date: 28/02/2006 12:07:38$
!
!

      CHARACTER PARMIN*20, SP*74/' '/, MODE*1                        !B
      INTEGER   INDHED,BLKSIZ,NOW(9),CENDAY,CENTHR,DATIME(5),TOR(5)  !B
*
      INTEGER*2 TIMTAG,NTRIES,NREPS
      INTEGER*2 BLKTAG,NINBLK,LEFT,RECLEN(9999),NOFLOW, RECDIS(9999)
      INTEGER*2 BLOCKS,XBLOKS,XHOURS,AMSTAR, OFLOWS,INDENT
      INTEGER*2 LAT,LONG, ITOR, IBLOCK,IREC
      LOGICAL CORF                                                   !B
*
* RECLEN IS THE ARRAY OF REPORT LENGTH SLOTS AFTER THE BLOCK HEADER.
* ITS DIMENSION IS ARBITRARY, IT ONLY NEEDS TO BE BIG ENOUGH FOR THE
* REPORTS THAT WILL FIT IN A BLOCK.  WE DON'T KNOW WHERE THE LENGTHS
* WILL END AND THE REPORTS, SLOTTED IN FROM THE END, WILL MEET THEM!
*
      PARAMETER (INDHED=6,MAXNID=672)
      CHARACTER*27998 MAP,BLOCK                                    !1.2
      CHARACTER*23 INDEKS(MAXNID)       ! INDEX ENTRIES
      CHARACTER*23 MASK/'  XXXXXXXXX'/  ! TO SORT BY IDENTIFIER
      COMMON /MDBUFS/ MAP, RECLEN,BLOCK, RECDIS, INDEKS
      DATA CENTHR/0/
*
* READ IBL & NOBL (INDICATING THE INDEX BLOCK(S) WANTED) & BLOCKSIZE
* FROM PARM='FIRST INDEX BLOCK, NUMBER OF BLOCKS, BLOCK SIZE'
*
      CALL PARM(1,PARMIN)
      READ (PARMIN,*) IBL,NOBL,BLKSIZ,MODE                           !B
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
      NBLSEQ=ICHAR(MAP(BLOCKS:BLOCKS))
      IF (NBLSEQ.EQ.0) NSQ=0
      IF (NBLSEQ.GT.0) NSQ=1               ! NSQ=1 TO SKIP SEQ BLOCK
*
* OPEN OUTPUT DATASET TOO IF COPY IS TO BE DONE
*
      IF (MODE.EQ.'S') THEN
        OPEN (2,ACCESS='DIRECT',RECL=BLKSIZ)
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
* READ INDEX BLOCK(S).
* (N.B. THERE'S NO CHECK HERE FOR CURRENT DATA, SO BEWARE IF THERE ARE
*       BREAKS IN THE TIME SEQUENCE, IE. GAPS IN THE DATA!)
*
      IX=NINDEX
      IF (IX.GT.1+NSQ+XBLOKS) IX=IX-XBLOKS
      READ (1,REC=IX) TIMTAG,NTRIES,NREPS,
     -                (INDEKS(I),I=1,NTRIES)
*
      IDAY=TIMTAG/256
      IHR=TIMTAG-IDAY*256
      IF (MODE.EQ.'S') PRINT *,IDAY,'TH',IHR,'Z',NTRIES,'ENTRIES'
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
* IF HANDLING ALL DATA, SET CENTURY-HOUR OF INDEX BLOCK (NEEDED BELOW)
*
      IF (CENTHR.EQ.0) THEN                                          !B
        CALL DATE31(IDAY,IMONTH,IYEAR,CENDAY)                        !B
        CENTHR=(CENDAY-1)*24+IHR                                     !B
      ENDIF                                                          !B
*
* SORT INDEX ENTRIES (ONLY FOR PRINT, NOT COPY), THEN LOOP ROUND THEM
*
      IF (MODE.NE.'S') THEN                                          !B
        L=INDENT
        N=NTRIES
        CALL SORTCH(INDEKS,L,N,MASK)                                 !B
      ENDIF                                                          !B
*
      DO 100 INT=1,NTRIES
       READ (INDEKS(INT)(20:21),'(A2)') IREC
       READ (INDEKS(INT)(22:23),'(A2)') IBLOCK
*
* READ DATA BLOCK IF IT'S NOT IN CORE.
*
       IF (IBLOCK.GT.0 .AND. IREC.GT.0) THEN                        !A
         IF (IBLOCK.NE.NBLOCK) THEN
           NBLOCK=IBLOCK
           READ (1,REC=1+NSQ+XBLOKS+NBLOCK) BLKTAG,NINBLK,LEFT,
     &         (RECLEN(I),I=1,NINBLK),BLOCK(1+6+2*NINBLK:BLKSIZ)
           RECDIS(1)=RECLEN(1)
           DO I=2,NINBLK
             RECDIS(I)=RECDIS(I-1)+RECLEN(I)
           ENDDO
         ENDIF
*
* THE TIME MAY NOT BE IN THE REPORT, SO GET IT FROM THE FIRST BYTES OF
* THE INDEX ENTRY (SET FROM THE BULLETIN HEADER IF NEC.)
*
   30    I=BLKSIZ-RECDIS(IREC)
         L=RECLEN(IREC)
         IHOUR=MOD(ICHAR(INDEKS(INT)(1:1)),32)                       !B
         MINUTE=ICHAR(INDEKS(INT)(2:2))                              !B
         IT=(IHOUR+IHR)*100+MINUTE                                   !B
***********************************************************************
*
* FINALLY PRINT THE REPORTS
*
***********************************************************************
         ITOR=ICHAR(INDEKS(INT)(18:18))*256+ICHAR(INDEKS(INT)(19:19))

! SET TIME OF RECEIPT BY ADDING DISPLACEMENT TO CENTURY-HOUR OF
! INDEX BLOCK & CONVERTING BACK TO DATE.

         CALL DATE13((CENTHR+ITOR/60)/24+1,TOR(3),TOR(2),TOR(1))     !B
         TOR(4)=MOD(CENTHR+ITOR/60,24)                               !B
         TOR(5)=ITOR-(ITOR/60)*60                                    !B
         ITR=TOR(4)*100+TOR(5)                                       !B
*
* PRINT UP TO 80 BYTES OF REPORT, STARTING WITH DATE/TIME OF DATA AND
* TIME OF RECEIPT.
*
         IF (MODE.NE.'S') THEN                                       !B
           IF (L.LT.95) THEN
             WRITE (*,2) INDEKS(INT)(3:8),IDAY,IT,TOR(3),ITR,
     &                   BLOCK(I+1:I+L-23)//SP(1:95-L)
           ELSE
             WRITE (*,2) INDEKS(INT)(3:8),IDAY,IT,TOR(3),ITR,
     &                   BLOCK(I+1:I+72)
           ENDIF
    2      FORMAT(1X,A6,I2.2,'/',I4.4,'Z TOR=',I2.2,'/',I4.4,'Z ',A72)
         ELSE                                                        !B

! OR CALL AMDSTO TO STORE THE DATA AGAIN, SETTING DATA TIME & TIME OF
! RECEIPT, DATA TIME FROM CENTURY-HOUR OF INDEX BLOCK + DISPLACEMENT

           CALL DATE13((CENTHR+IHOUR)/24+1,                          !B
     &                 DATIME(3),DATIME(2),DATIME(1))                !B
           DATIME(4)=MOD(CENTHR+IHOUR,24)                            !B
           DATIME(5)=MINUTE                                          !B

! ICCCC IS COLLECTING CENTRE (NUMBER) TO GO IN BUFR MESSAGE: 65535
! LEAVES WHAT'S THERE.  CORF IS SET IF FIRST BIT OF INDEX ENTRY IS SET.

           ICCCC=65535                                               !B
           CORF=ICHAR(INDEKS(INT)(1:1)).GE.128                       !B
           CALL AMDSTO(BLOCK(I+1:I+L),L,INDEKS(INT),DATIME,          !B
     &                 ICCCC,CORF,2,0,TOR)                           !B
         ENDIF                                                       !B
       ENDIF                                                         !A
  100 CONTINUE
      IF (MODE.NE.'S') PRINT *,' '                                   !B
      CENTHR=CENTHR+XHOURS                                           !B
  110 CONTINUE
      STOP
      END
