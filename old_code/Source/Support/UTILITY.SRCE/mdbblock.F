***********************************************************************
*                                                                     *
* PROGRAM       : MDBBLOCK                                            *
*                                                                     *
* PURPOSE       : PRINT ONE DATA BLOCK IN MDB DATA SET (23-BYTE       *
*                 INDEX ENTRIES, CHAINED REPORTS WITH TRAILERS)       *
*                                                                     *
* INPUT         : PARM='BLOCK NUMBER, BLOCKSIZE', WHERE BLOCKS ARE    *
*                 NUMBERED FROM START OF DATA SET                     *
*                 (SO IBL HERE IS 1+IXBLOKS+IBL IN STORAGE PROGRAMS)  *
*                                                                     *
*Y2K  16.06.1997  MDBBLOCK IS YEAR 2000 COMPLIANT.
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
      CHARACTER*27998 BLOCK
      COMMON /MDBUFS/ RECLEN,BLOCK
*
* READ IBL (DATA BLOCK WANTED) & BLOCKSIZE FROM PARM='IBL,BLKSIZ/'
*
      CALL PARM(1,PARMIN)
      READ (PARMIN,*) IBL,BLKSIZ
*
      OPEN (1,ACCESS='DIRECT',RECL=BLKSIZ)                              00030018
*
* READ DATA BLOCK IF IT'S NOT IN CORE.
*
      READ (1,REC=IBL) BLKTAG,NINBLK,LEFT,
     -   (RECLEN(I),I=1,NINBLK),BLOCK(1+6+2*NINBLK:BLKSIZ)
      PRINT *,'TIME TOR - BOTH RELATIVE TO TIME OF INDEX BLOCK'
*
      IDISPL=BLKSIZ
      DO 10 I=1,NINBLK
       TRAILER=BLOCK(IDISPL-22:IDISPL)
       L=RECLEN(I)-23       ! RECLEN MAY BE ZERO IF BLOCK HAS BEEN SPLIT
       IF (L.GT.0) THEN
         IDISPL=IDISPL-(L+23)
*
         IHOUR=MOD(ICHAR(TRAILER(1:1)),32)
         MIN=ICHAR(TRAILER(2:2))
         IT=IHOUR*100+MIN
*
         READ (TRAILER(18:19),'(A2)') ITOR
         ITR=(ITOR/60)*100 + ITOR-(ITOR/60)*60
*
         READ (TRAILER(20:21),'(A2)') IREC
         READ (TRAILER(22:23),'(A2)') IBLOCK
*
         P='-->'
         IF (IREC.GT.I) P='>>>'
*
         IF (L.GE.49) THEN
           WRITE (*,1) I,IT,ITR, BLOCK(IDISPL+1:IDISPL+49),
     -               TRAILER(3:11), P,IREC
    1    FORMAT (I4,2I5.4,1X,A49,1X,A9,A3,I3)
         ELSE
           WRITE (*,1) I,IT,ITR, BLOCK(IDISPL+1:IDISPL+L)//SP(:49-L),
     -               TRAILER(3:11), P,IREC
         ENDIF
       ELSE
         PRINT *,' '
       ENDIF
   10 CONTINUE
      STOP
      END
