***********************************************************************
*                                                                     *
* PROGRAM       : MDBSATOB                                            *
*                                                                     *
* PURPOSE       : TO LIST MESSAGES IN E.G. SATOB DATA BASE, PRINTING  *
*                 THE TIME & POSITION INFORMATION IN THE INDEX.       *
*                 (SHOULD WORK FOR ANY DATA SET WITH BLOCKSIZE=15476  *
*                  AND 23-BYTE INDEX ENTRIES)                         *
*                                                                     *
*Y2K  16.06.1997  MDBSATOB IS YEAR 2000 COMPLIANT.
*                                                                     *
* CHANGE RECORD :                                                     *
*                                                                     *
***********************************************************************
      INTEGER*2 TIMTAG,NTRIES,LASLEF
      INTEGER DAY,HOUR,TIME,REC, TOR, BLOCK
      CHARACTER INDEKS*15476,ENTRY*23, SEA*3,LIM*3
*
      CHARACTER SPACES*5/' '/, TITLE*150 /'04Z  05Z  06Z  07Z  08Z  09Z
     - 10Z  11Z  12Z  13Z  14Z  15Z  16Z  17Z  18Z  19Z  20Z  21Z  22Z
     -23Z  00Z  01Z  02Z  03Z  04Z  05Z  06Z  07Z  08Z  09Z'/
*
      EQUIVALENCE (INDEKS(1:2),TIMTAG),
     -            (INDEKS(3:4),NTRIES),
     -            (INDEKS(5:6),LASLEF)
*
      OPEN (3,FILE='MDB',ACCESS='DIRECT',RECL=15476)
      NTOTAL=0
*
* FIRST READ MAP BLOCK TO SEE HOW MANY INDEX BLOCKS & HOURS PER BLOCK
*
      READ (3,REC=1) INDEKS
      NBLOKS=ICHAR(INDEKS(4:4))
      NHOURS=ICHAR(INDEKS(6:6))
*
* THEN GO ROUND THAT NUMBER OF BLOCKS, PRINTING OUT ENTRIES.
* FIRST THE COLUMN HEADINGS.
*
      DO 10 N=1,NBLOKS
      READ (3,REC=N+1) INDEKS
      IF (NTRIES.GT.0) THEN
        PRINT *,' '
        WRITE (*,'('' INDEX BLOCK'',I3)') N
        PRINT *,'  BLOCK  RECORD  DATE/TIME  SAT IDENT  SOUNDINGS ',    '
     -   '   LATS   LONGS       TOR'
        DAY=TIMTAG/256
        HOUR=TIMTAG-DAY*256
      ENDIF
      NHOUR=0
*
      DO 20 I=1,NTRIES
      ENTRY=INDEKS(7+23*(I-1):29+23*(I-1))
*
* FIND THE HOUR OF THE DATA, ADDING IT TO THAT IN THE BLOCK'S TIME TAG
*
      IHR=MOD(ICHAR(ENTRY(1:1)),32)
      HR=HOUR+IHR
      IDAY=DAY
      IF (HR.GE.24) THEN
        HR=HR-24
        IDAY=DAY+1                          ! USE ZPDATE!!!
      ENDIF
      MINS=ICHAR(ENTRY(2:2))
      TIME=HR*100+MINS
*
* PUT THE TIME OF RECEIPT RELATIVE TO THE INDEX SLOT IN A PRINTABLE FORM
*
      MOR=ICHAR(ENTRY(18:18))*256+ICHAR(ENTRY(19:19)) !MINS FROM SLOT HR
      TOR=(HOUR+MOR/60)*100+MOD(MOR,60)     ! PRINTABLE TIME
      IF (TOR.GT.2400) TOR=TOR-2400
      IF (MOR.EQ.765) TOR=-999999999        ! UNPRINTABLE IF MISSING
*
* RECORD NUMBER, BLOCK NUMBER & NUMBER OF OBSERVATIONS (WHICH OVERFLOWS
* (INTO BYTE 11 FOR SATOBS, SO ADD THAT TIMES 256 IF NOT SPACE)
*
      REC=ICHAR(ENTRY(20:20))*256+ICHAR(ENTRY(21:21))
      BLOCK=ICHAR(ENTRY(22:22))*256+ICHAR(ENTRY(23:23))
      NOBS=ICHAR(ENTRY(12:12))
      IF (ENTRY(11:11).NE.' ') NOBS=NOBS+ICHAR(ENTRY(11:11))*256
*
* GET LAT/LONG(S) & PRINT ENTRY. EITHER LAT/LONG BOX OR SINGLE POSITION.
*
      IF (NOBS.GT.1) THEN
        MINLAT=ICHAR(ENTRY(13:13))-90
        MAXLAT=ICHAR(ENTRY(14:14))-90
        MINLON=ICHAR(ENTRY(15:15))*2-180
        MAXLON=ICHAR(ENTRY(16:16))*2-180
*
        WRITE (*,2) BLOCK,REC,IDAY,TIME,ENTRY(3:10),NOBS,
     &   MINLAT,MAXLAT,MINLON,MAXLON,TOR
    2   FORMAT (3I7,'/',I4.4,'Z  ',A8,I7,6X,4I4,I9.4,'Z')
*
      ELSE IF (NOBS.EQ.1) THEN
        LAT=ICHAR(ENTRY(13:13))*256+ICHAR(ENTRY(14:14))
        LON=ICHAR(ENTRY(15:15))*256+ICHAR(ENTRY(16:16))
        IF (LAT.GT.32768) LAT=LAT-65536
        IF (LON.GT.32768) LON=LON-65536
        RLAT=LAT/100.
        RLON=LON/100.
*
        WRITE (*,3) BLOCK,REC,IDAY,TIME,ENTRY(3:10),NOBS,
     &   RLAT,RLON,TOR
    3   FORMAT (3I7,'/',I4.4,'Z  ',A8,I7,6X,2F8.2,I9.4,'Z')
      ENDIF
*
      NHOUR=NHOUR+NOBS
   20 CONTINUE
*
      PRINT *,' '
      WRITE (*,1) NHOUR,NHOURS
    1 FORMAT (I9,' SOUNDINGS FOR',I2,' HOURS')
      PRINT *,' '
*
      NTOTAL=NTOTAL+NHOUR
   10 CONTINUE
*
      PRINT *,' '
      PRINT *,NTOTAL,'SOUNDINGS IN WHOLE DATA BASE'
      PRINT *,' '
      STOP
      END
