***********************************************************************
*                                                                     *
* PROGRAM       : TAFBLOCK                                            *
*                                                                     *
* PURPOSE       : PRINT OUT NEGATIVE TIMES OF RECEIPT IN MDB          *
*                                                                     *
* PARAMETERS    : PARM='B,BLKSIZE/', WHERE B IS FIRST DATA BLOCK      *
*                                                                     *
*Y2K  16.06.1997  TAFTORCL IS YEAR 2000 COMPLIANT.
*                                                                     *
***********************************************************************
      CHARACTER PARMIN*20, TRAILER*23, SP*60/' '/,P*3
      INTEGER   BLKSIZ
*
      INTEGER*2 BLKTAG,NINBLK,LEFT,RECLEN(999)
      INTEGER*2 LAT,LONG, ITOR, IBLOCK,IREC
*
* RECLEN IS THE ARRAY OF REPORT LENGTH SLOTS AFTER THE BLOCK HEADER.
* ITS DIMENSION IS ARBITRARY, IT ONLY NEEDS TO BE BIG ENOUGH FOR THE
* REPORTS THAT WILL FIT IN A BLOCK.  WE DON'T KNOW WHERE THE LENGTHS
* WILL END AND THE REPORTS, SLOTTED IN FROM THE END, WILL MEET THEM!
*
      PARAMETER (INDHED=6)
      CHARACTER*23476 BLOCK
      COMMON /MDBUFS/ RECLEN,BLOCK
*
* READ IBL (BLOCK TO START AT) & BLOCKSIZE FROM PARM='IBL,BLKSIZ/'
*
      CALL PARM(1,PARMIN)
      READ (PARMIN,*) IBL,BLKSIZ
*
      OPEN (1,ACCESS='DIRECT',RECL=BLKSIZ)                              00030018
*
* READ DATA BLOCK IF IT'S NOT IN CORE.
*
  100 READ (1,REC=IBL,IOSTAT=LAST) BLKTAG,NINBLK,LEFT,
     -   (RECLEN(I),I=1,NINBLK),BLOCK(1+6+2*NINBLK:BLKSIZ)
      IF (LAST.GT.0) STOP
*
      IBLDAY=BLKTAG/256
      IBLHOR=BLKTAG-IBLDAY*256
*
      IDISPL=BLKSIZ
      DO 10 I=1,NINBLK
       TRAILER=BLOCK(IDISPL-22:IDISPL)
       L=RECLEN(I)-23
       IDISPL=IDISPL-(L+23)
*
       IHOUR=MOD(ICHAR(TRAILER(1:1)),32)
       MIN=ICHAR(TRAILER(2:2))
       IT=(IBLHOR+IHOUR)*100+MIN
*
       READ (TRAILER(18:19),'(A2)') ITOR
       ITOR=ITOR-IHOUR*60
*
       IF (L.GT.0 .AND. ITOR.LT.0) THEN
         IF (L.GE.64) THEN
           WRITE (*,1) IBLDAY,IT, ITOR, BLOCK(IDISPL+1:IDISPL+64)
    1      FORMAT (I3.2,'/',I4.4,I7,2X,A64)
         ELSE
           WRITE(*,1)IBLDAY,IT,ITOR,BLOCK(IDISPL+1:IDISPL+L)//SP(:64-L)
         ENDIF
       ENDIF
   10 CONTINUE
      IBL=IBL+1
      GO TO 100
      STOP
      END
