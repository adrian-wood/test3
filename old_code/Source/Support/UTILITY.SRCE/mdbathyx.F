***********************************************************************
*                                                                     *
* PROGRAM       : MDBATHYX                                            *
*                                                                     *
* PURPOSE       : TO PRINT BATHY/TESAC INDEX                          *
*                                                                     *
*Y2K  16.06.1997  MDBATHYX IS YEAR 2000 COMPLIANT.
*                                                                     *
* CHANGE RECORD :                                                     *
*   DEC 95: MADE FROM MDBINDEX (TOV, ERS)                             *
*                                                                     *
***********************************************************************
      INTEGER*2 TIMTAG,NTRIES,LASLEF
      INTEGER DAY,HOUR,TIME,BLOCK,REC,TOR,DAYSAGO
      CHARACTER INDEKS*15476,ENTRY*23,BATHTES*7
*
      EQUIVALENCE (INDEKS(1:2),TIMTAG),
     -            (INDEKS(3:4),NTRIES)
*
      OPEN (3,FILE='SUBSEA',ACCESS='DIRECT',RECL=15476)
      NTOTAL=0
*
* FIRST READ MAP BLOCK TO SEE HOW MANY INDEX BLOCKS (DAILY BLOCKS)
*
      READ (3,REC=1) INDEKS
      NBLOKS=ICHAR(INDEKS(4:4))
*
* THEN GO ROUND THAT NUMBER OF BLOCKS, PRINTING OUT ENTRIES.
*
      DO 10 N=1,NBLOKS
      READ (3,REC=2+N) INDEKS         ! SKIP MAP & SEQUENCES
      IF (NTRIES.GT.0) DAY=TIMTAG/256
      PRINT *,'DATE/TIME SHIP         LAT/LONG (BATHY  DAYS  BLOCK/'
      PRINT *,'                                 BLANK) LATE  RECORD'
      PRINT *,' '
*
* LOOP ROUND ENTRIES IN BLOCK
*
      DO 20 I=1,NTRIES
        ENTRY=INDEKS(6+I*23-22:6+I*23)
*
        HOUR=ICHAR(ENTRY(1:1))
        MINS=ICHAR(ENTRY(2:2))
        TIME=HOUR*100+MINS
*
        LAT=ICHAR(ENTRY(13:13))*256+ICHAR(ENTRY(14:14))
        LON=ICHAR(ENTRY(15:15))*256+ICHAR(ENTRY(16:16))
        IF (LAT.GT.32768) LAT=LAT-65536
        IF (LON.GT.32768) LON=LON-65536
        RLAT=LAT/100.
        RLON=LON/100.
*
        IF (MOD(ICHAR(ENTRY(17:17)),2).EQ.1) THEN
          BATHTES=' TESAC '
        ELSE
          BATHTES=' '
        ENDIF
*
        TOR=ICHAR(ENTRY(18:18))*256+ICHAR(ENTRY(19:19))
        DAYSAGO=TOR/1440
*
        REC=ICHAR(ENTRY(20:20))*256+ICHAR(ENTRY(21:21))
        BLOCK=ICHAR(ENTRY(22:22))*256+ICHAR(ENTRY(23:23))
*
* FINALLY PRINT THIS ENTRY.  AT THE END OF AN INDEX SLOT PRINT THE TOTAL
*
        WRITE (*,2) DAY,TIME, ENTRY(3:11), RLAT,RLON,
     &              BATHTES,DAYSAGO, BLOCK,REC
    2   FORMAT (I3,'/',I4.4,'Z  ',A9, 2F7.2, A7, 2I4,'/',I2.2)
   20 CONTINUE
*
      NTOTAL=NTOTAL+NTRIES
      PRINT *,' '
      PRINT *,NTRIES,'REPORTS FOR ABOVE DAY'
      PRINT *,' '
   10 CONTINUE
*
      PRINT *,' '
      PRINT *,NTOTAL,'SOUNDINGS IN WHOLE DATA BASE'
      STOP
      END
