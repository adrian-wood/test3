***********************************************************************
*                                                                     *
* PROGRAM       : MDBINDEX                                            *
*                                                                     *
* PURPOSE       : TO LIST MESSAGES IN A SATELLITE DATA BASE        !1.2
*                 (12-BYTE INDEX ENTRIES), PRINTING TIME &         !1.2
*                 POSITION FROM THE INDEX.                         !1.2
*                                                                     *
* CHANGE RECORD :                                                     *
*   DEC 90: MADE FROM TOVMAP WITH 12-BYTE ENTRIES & BLKSIZE=23476     *
*                                                                     *
***********************************************************************
! $LOG: MDBINDEX.F

      INTEGER*2 TIMTAG,NTRIES,LASLEF, NOFLOW
      INTEGER   DAY,HOUR,TIME,REC, TOR
      CHARACTER BLOCK*27998, SEA*3,LIM*3, INDOVR*2                !1.2
      CHARACTER*12 INDEKS(9999),ENTRY                             !1.2
*
      CHARACTER SPACES*5/' '/, TITLE*150 /'04Z  05Z  06Z  07Z  08Z  09Z
     - 10Z  11Z  12Z  13Z  14Z  15Z  16Z  17Z  18Z  19Z  20Z  21Z  22Z
     -23Z  00Z  01Z  02Z  03Z  04Z  05Z  06Z  07Z  08Z  09Z'/
*
      CHARACTER*132 HEAD
      HEAD=' '
*
      CALL PARM(0,LBLOCK)                                          !1.2
      OPEN (3,FILE='SAT120',ACCESS='DIRECT',RECL=LBLOCK)           !1.2
      NTOTAL=0
      NBLIND=(LBLOCK-8)/12
*
* FIRST READ MAP BLOCK TO SEE HOW MANY INDEX BLOCKS & HOURS PER BLOCK
* SEE IF THERE'S A SEQUENCE BLOCK AFTER THE MAP BLOCK (NSQ=1 IF SO).
*
      READ (3,REC=1) BLOCK(1:LBLOCK)                               !1.2
      NBLOKS=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))               !1.2
      NXBLKS=ICHAR(BLOCK(4:4))                                     !1.2
      NHOURS=ICHAR(BLOCK(6:6))                                     !1.2

      NSQ=0                                                        !1.2
      IF (NBLOKS.LT.12000) THEN
        IF (ICHAR(BLOCK(8+NBLOKS:8+NBLOKS)).GT.0) NSQ=1            !1.2
      ELSE                                                         !1.2
        IF (ICHAR(BLOCK(9:9)).GT.0) NSQ=1                          !1.2
      ENDIF                                                        !1.2
*
* THEN GO ROUND THAT NUMBER OF BLOCKS, PRINTING OUT ENTRIES.
* FIRST THE COLUMN HEADINGS.
*
      DO N=1,NXBLKS
        IX=1+NSQ+N
        NIND=1                             ! CONTINUATION NUMBER
   10   NIBL=(NIND-1)*NBLIND               ! ENTRIES ALREADY READ IN
        READ (3,REC=IX) TIMTAG,NTRIES,LASLEF,
     &                (INDEKS(I),I=NIBL+1,NIBL+NBLIND),INDOVR
        NOFLOW=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))
        IF (NOFLOW.GT.0) THEN
          IX=NOFLOW+NSQ
          NIND=NIND+1
          GO TO 10
        ENDIF

        IF (NTRIES.GT.0) THEN
          PRINT *,' '
          PRINT *,N,'TH INDEX BLOCK',NTRIES,'ENTRIES'
          PRINT *,'  BLOCK  RECORD  DATE/TIME  SAT ID  SOUNDINGS ',     '
     &   '   LATS   LONGS       TOR'
          DAY=TIMTAG/256
          HOUR=TIMTAG-DAY*256
        ENDIF
        NHOUR=0
*
* PUT TOGETHER THE IDENTIFIER & SET THE LAND/SEA AND LIMITED AREA FLAGS
*
        DO I=1,NTRIES
          ENTRY=INDEKS(I)
          ID=MOD(ICHAR(ENTRY(1:1)),64)*8+ICHAR(ENTRY(2:2))/32
          SEA=' '
          LIM=' '
          IF (ICHAR(ENTRY(1:1))/128.EQ.1) SEA='SEA'
          IF (MOD(ICHAR(ENTRY(1:1))/64,2).EQ.0) LIM='LIM'
*
* FIND THE HOUR OF THE DATA, ADDING IT TO THAT IN THE BLOCK'S TIME TAG
*
          IHR=MOD(ICHAR(ENTRY(2:2)),32)
          HR=HOUR+IHR
          IDAY=DAY
          IF (HR.GE.24) THEN
            HR=HR-24
            IDAY=DAY+1                      ! USE ZPDATE!!!
          ENDIF
          MINS=ICHAR(ENTRY(3:3))/4
          TIME=HR*100+MINS
*
* PUT THE TIME OF RECEIPT RELATIVE TO THE INDEX SLOT IN A PRINTABLE FORM
*
          MOR=ICHAR(ENTRY(9:9))*3           ! MINUTES AFTER SLOT HOUR
          TOR=(HOUR+MOR/60)*100+MOD(MOR,60) ! PRINTABLE TIME
          IF (TOR.GT.2400) TOR=TOR-2400
          IF (MOR.EQ.765) TOR=-999999999    ! UNPRINTABLE IF MISSING
*
* BLOCK & RECORD NUMBER & NUMBER OF OBSERVATIONS
*
          NBLOCK=ICHAR(ENTRY(11:11))*256+ICHAR(ENTRY(12:12))
          REC=ICHAR(ENTRY(10:10))
          NOBS=MOD(ICHAR(ENTRY(3:3)),4)*256+ICHAR(ENTRY(4:4))
*
          MINLAT=ICHAR(ENTRY(5:5))-90
          MAXLAT=ICHAR(ENTRY(6:6))-90
          MINLON=ICHAR(ENTRY(7:7))*2-180
          MAXLON=ICHAR(ENTRY(8:8))*2-180
*
* FINALLY PRINT THIS ENTRY.  AT THE END OF AN INDEX SLOT PRINT THE TOTAL
*
          WRITE (*,2) NBLOCK,REC,IDAY,TIME,ID,NOBS,
     &               MINLAT,MAXLAT,MINLON,MAXLON,TOR, SEA,LIM
    2     FORMAT (3I7,'/',I4.4,'Z',2I7,6X,4I4,I9.4,'Z', 2X,A3,2X,A3)
*
          NHOUR=NHOUR+NOBS
        ENDDO
*
        PRINT *,' '
        WRITE (*,1) NHOUR,NHOURS
    1   FORMAT (I9,' SOUNDINGS FOR',I2,' HOURS')
        PRINT *,' '
*
        NTOTAL=NTOTAL+NHOUR
      ENDDO
*
      PRINT *,' '
      PRINT *,NTOTAL,'SOUNDINGS IN WHOLE DATA BASE'
      PRINT *,' '
      STOP
      END
