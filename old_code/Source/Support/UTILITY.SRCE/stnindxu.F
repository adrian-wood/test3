**********************************************************              00010000
*                                                        *              00020000
*  SUBROUTINE STNINDX                                    *              00030000
*                                                        *              00040000
*  TO SEARCH INDEX FOR REQUIRED STATION DETAILS          *              00050000
*                                                        *              00060000
*Y2K  16.06.1997  STNINDXU IS YEAR 2000 COMPLIANT.                      00060110
*                                                        *              00060210
*  CHANGE                                                               00061003
*  19/10/92     ALLOW ICAO REGION RETRIEVAL                             00062003
*  22/02/93     INITIALISE A VARIABLE (ITOT) WHICH HAD                  00063008
*               BEEN OVERLOOKED                                         00064008
**********************************************************              00070000
*234567                                                                 00080000
      SUBROUTINE STNINDX(WMOLST,ICAOLST,DCNNLST,RAINLST,                00090000
     *           IDTYPE,POS,ISTAT,INDREC)                               00100000
*                                                                       00150000
      INTEGER IDTYPE,IDLIST,WMOLST(50),KEPSTN,INDREC(1000),ISTAT,IREC   00160000
      INTEGER DCNNLST(50),RAINLST(50)                                   00170000
      INTEGER WMOID,DCNNID,RAINID                                       00170100
      INTEGER STNINTID(50),RECID(50),WMOINTID(2,50)                     00171000
      INTEGER KEPDCNN,KEPRAIN,ITOT,COUNT                                00180000
      CHARACTER*4 ICAOLST(50),STNCHRID(50),KEPICAO                      00193000
      CHARACTER*4 ICAOID                                                00193100
      CHARACTER*1 POS                                                   00194000
*                                                                       00371001
* INITIALISE VARIABLES                                                  00380000
*                                                                       00381000
******************************************************************      00381104
*                                                                       00381204
*                                                                       00381304
      ITOT = 0                                                          00381406
*                                                                       00381504
*                                                                       00381604
******************************************************************      00381704
      KEPSTN=0                                                          00381900
      IDLIST=0                                                          00382000
      JJJ=1                                                             00382100
*                                                                       00382200
      DO 2 I=1,1000                                                     00382300
        INDREC(I)=0                                                     00382400
   2  CONTINUE                                                          00382500
*                                                                       00382600
* LOOK AT LIST OF IDENTIFIERS                                           00383400
*                                                                       00383500
      COUNT=1                                                           00383600
*                                                                       00383700
 800  IF(COUNT.GT.50) GOTO 200                                          00383800
*                                                                       00384100
      IF(IDTYPE.EQ.1)THEN                                               00384200
       IF(WMOLST(COUNT).NE.0)THEN                                       00384400
         WMOID=WMOLST(COUNT)                                            00384500
         ICAOID='    '                                                  00384700
         DCNNID=-32768                                                  00384800
         RAINID=-32768                                                  00384900
         COUNT=COUNT+1                                                  00385000
       ENDIF                                                            00385100
      ELSEIF(IDTYPE.EQ.2)THEN                                           00385200
       IF(ICAOLST(COUNT).NE.'    ')THEN                                 00385300
         WMOID=-32768                                                   00385400
         ICAOID=ICAOLST(COUNT)(1:4)                                     00385500
         DCNNID=-32768                                                  00385600
         RAINID=-32768                                                  00385700
         COUNT=COUNT+1                                                  00385800
       ENDIF                                                            00385900
      ELSEIF(IDTYPE.EQ.3)THEN                                           00386000
       IF(DCNNLST(COUNT).NE.0)THEN                                      00386100
         WMOID=-32768                                                   00386200
         ICAOID='    '                                                  00386300
         DCNNID=DCNNLST(COUNT)                                          00386600
         RAINID=-32768                                                  00386700
         COUNT=COUNT+1                                                  00386800
       ENDIF                                                            00386900
      ELSEIF(IDTYPE.EQ.4)THEN                                           00387000
       IF(RAINLST(COUNT).NE.0)THEN                                      00387100
         WMOID=-32768                                                   00387200
         ICAOID='    '                                                  00387300
         DCNNID=-32768                                                  00387400
         RAINID=RAINLST(COUNT)                                          00387600
         COUNT=COUNT+1                                                  00387700
       ENDIF                                                            00387800
      ENDIF                                                             00387900
*                                                                       00388400
* CHECK TEMPORARY RECORDS FIRST                                         00388500
*                                                                       00388600
* IF WMO BLK AND STATION NUMBER                                         00388700
*                                                                       00388800
      IF(IDTYPE.EQ.1)THEN                                               00388900
        IREC=590                                                        00389000
        READ(85,4,REC=IREC)ITOTAL,((WMOINTID(II,JJ),II=1,2),JJ=1,50)    00389100
*                                                                       00389200
        DO 891 ILOOP=1,50                                               00389300
          IF(WMOID.EQ.WMOINTID(1,ILOOP))THEN                            00389400
            IF(POS.EQ.'L')THEN                                          00389500
              INDREC(JJJ)=WMOINTID(2,ILOOP)                             00389600
              JJJ=JJJ+1                                                 00389700
            ENDIF                                                       00389800
          ENDIF                                                         00389900
          IF(WMOINTID(1,ILOOP).EQ.0) GOTO 100                           00390000
 891    CONTINUE                                                        00390100
*                                                                       00390200
* IF ICAO ID                                                            00390300
*                                                                       00390400
      ELSEIF(IDTYPE.EQ.2)THEN                                           00390500
        IREC=591                                                        00390600
        READ(85,5,REC=IREC)ITOTAL,(STNCHRID(II),RECID(II),II=1,50)      00390700
*                                                                       00390800
        DO 892 ILOOP=1,50                                               00390900
          IF(ICAOID.EQ.STNCHRID(ILOOP))THEN                             00391000
            IF(POS.EQ.'L')THEN                                          00391100
              INDREC(JJJ)=RECID(ILOOP)                                  00391200
              JJJ=JJJ+1                                                 00391300
            ENDIF                                                       00391400
          ENDIF                                                         00391500
          IF(STNCHRID(ILOOP).EQ.'    ') GOTO 100                        00391600
  892   CONTINUE                                                        00391700
*                                                                       00391800
* IF DCNN ID                                                            00391900
*                                                                       00392000
      ELSEIF(IDTYPE.EQ.3)THEN                                           00392100
        IREC=592                                                        00392200
        READ(85,6,REC=IREC)ITOTAL,(STNINTID(II),RECID(II),II=1,50)      00392300
*                                                                       00392400
         DO 893 ILOOP=1,50                                              00392500
           IF(DCNNID.EQ.STNINTID(ILOOP))THEN                            00392600
             INDREC(JJJ)=RECID(ILOOP)                                   00392800
             JJJ=JJJ+1                                                  00392900
           ENDIF                                                        00393000
           IF(STNINTID(ILOOP).EQ.0) GOTO 100                            00393900
  893    CONTINUE                                                       00394600
*                                                                       00394700
* ID RAIN ID                                                            00394800
*                                                                       00394900
      ELSEIF(IDTYPE.EQ.4)THEN                                           00395000
         IREC=593                                                       00395100
         READ(85,7,REC=IREC)ITOTAL,(STNINTID(II),RECID(II),II=1,50)     00395300
*                                                                       00395500
          DO 894 ILOOP=1,50                                             00395600
            IF(RAINID.EQ.STNINTID(ILOOP))THEN                           00395700
              INDREC(JJJ)=RECID(ILOOP)                                  00396200
              JJJ=JJJ+1                                                 00396300
            ENDIF                                                       00397200
            IF(STNINTID(ILOOP).EQ.0) GOTO 100                           00397300
  894     CONTINUE                                                      00398000
      ENDIF                                                             00398100
*************************************************************           00398200
*************************************************************           00398300
*************************************************************           00398400
*                                                                       00398500
* NOW PROCESS MAIN PART OF INDEX IF APPROPRIATE                         00398600
*                                                                       00398700
*                                                                       00398800
* IF WMO BLK AND STN                                                    00398900
*                                                                       00399000
 100  IF(IDTYPE.EQ.1)THEN                                               00399100
*                                                                       00400000
* SPLIT WMOID INTO BLK AND STN                                          00410000
*                                                                       00420000
        IF(WMOID.LE.99)THEN                                             00421000
          IDLIST=1                                                      00422000
        ENDIF                                                           00423000
*                                                                       00424000
        IF(IDLIST.EQ.0)THEN                                             00425000
          IBLK=WMOID/1000                                               00430000
          IF((IBLK.LT.1).OR.(IBLK.GT.99)) GOTO 200                      00431000
*                                                                       00463200
* LOOK AT BLK INDEX FOR STATION                                         00463300
*                                                                       00464000
          IREC=1                                                        00470000
*                                                                       00480000
  10      READ(85,3,REC=IREC)((WMOINTID(II,JJ),II=1,2),JJ=1,50)         00490000
   3      FORMAT(5X,50(I2,1X,I5,1X))                                    00500000
*                                                                       00510000
          DO 20 ILOOP=1,50                                              00520000
            IF(IBLK.EQ.WMOINTID(1,ILOOP))THEN                           00530000
               IREC=WMOINTID(2,ILOOP)                                   00550000
               GOTO 123                                                 00560000
             ENDIF                                                      00570000
             IF(WMOINTID(1,ILOOP).EQ.0) GOTO 200                        00580000
   20     CONTINUE                                                      00610000
*                                                                       00620000
          IREC=IREC+1                                                   00630000
          GOTO 10                                                       00640000
*                                                                       00650000
  123     READ(85,4,REC=IREC)ITOTAL,((WMOINTID(II,JJ),II=1,2),JJ=1,50)  00660000
    4     FORMAT(5X,I2,1X,50(I5,1X,I5,1X))                              00670000
*                                                                       00680000
          DO 21 ILOOP=1,50                                              00691000
            IF(WMOID.EQ.WMOINTID(1,ILOOP))THEN                          00700000
              IF(POS.EQ.'L')THEN                                        00701000
                KEPSTN=WMOINTID(1,ILOOP)                                00710000
                DO 22 KKK=1,10                                          00711000
                  IF(KEPSTN.NE.WMOINTID(1,ILOOP+KKK))THEN               00712000
                    INDREC(JJJ)=WMOINTID(2,ILOOP+KKK-1)                 00713000
                    JJJ=JJJ+1                                           00713100
                    GOTO 200                                            00714000
                  ENDIF                                                 00715000
   22           CONTINUE                                                00716000
              ELSE                                                      00717000
                INDREC(JJJ)=WMOINTID(2,ILOOP)                           00718000
                JJJ=JJJ+1                                               00718100
                GOTO 21                                                 00719000
              ENDIF                                                     00731000
            ENDIF                                                       00742000
            IF(WMOINTID(1,ILOOP).EQ.0) GOTO 200                         00750000
  21      CONTINUE                                                      00780000
          IREC=IREC+1                                                   00800000
          GOTO 123                                                      00810000
        ELSEIF(IDLIST.EQ.1)THEN                                         00810100
          IBLK=WMOID                                                    00810200
          IF((IBLK.LT.1).OR.(IBLK.GT.99)) GOTO 200                      00810400
*                                                                       00813900
* LOOK AT BLK INDEX FOR STATION                                         00814000
*                                                                       00814100
 700      IREC=1                                                        00814200
*                                                                       00814300
  50      READ(85,53,REC=IREC)((WMOINTID(II,JJ),II=1,2),JJ=1,50)        00814400
  53      FORMAT(5X,50(I2,1X,I5,1X))                                    00814500
*                                                                       00814600
          DO 55 ILOOP=1,50                                              00814700
            IF(IBLK.EQ.WMOINTID(1,ILOOP))THEN                           00814800
               IREC=WMOINTID(2,ILOOP)                                   00814900
               GOTO 523                                                 00815000
             ENDIF                                                      00815100
             IF(WMOINTID(1,ILOOP).EQ.0) GOTO 200                        00815200
   55     CONTINUE                                                      00815500
*                                                                       00815600
          IREC=IREC+1                                                   00815700
          GOTO 50                                                       00815800
*                                                                       00815900
  523     READ(85,54,REC=IREC)ITOTAL,((WMOINTID(II,JJ),II=1,2),JJ=1,50) 00816000
   54     FORMAT(5X,I2,1X,50(I5,1X,I5,1X))                              00816100
*                                                                       00816200
          DO 51 ILOOP=1,50                                              00816300
            IF(WMOID.EQ.WMOINTID(1,ILOOP)/1000)THEN                     00816400
              IF(POS.EQ.'L')THEN                                        00816500
                IF(KEPSTN.EQ.WMOINTID(1,ILOOP)) GOTO 51                 00816600
                KEPSTN=WMOINTID(1,ILOOP)                                00816700
                DO 52 KKK=1,10                                          00816800
                  IF(KEPSTN.NE.WMOINTID(1,ILOOP+KKK))THEN               00816900
                    INDREC(JJJ)=WMOINTID(2,ILOOP+KKK-1)                 00817000
                    JJJ=JJJ+1                                           00817100
                    GOTO 51                                             00817200
                  ENDIF                                                 00817300
   52           CONTINUE                                                00817400
              ELSE                                                      00817500
                INDREC(JJJ)=WMOINTID(2,ILOOP)                           00817600
                JJJ=JJJ+1                                               00817700
                GOTO 51                                                 00817800
              ENDIF                                                     00817900
            ENDIF                                                       00818000
            IF(WMOINTID(1,ILOOP).EQ.0) GOTO 200                         00818100
  51      CONTINUE                                                      00818800
          IREC=IREC+1                                                   00818900
          GOTO 523                                                      00819000
        ENDIF                                                           00819100
*                                                                       00819200
* IF ICAOID                                                             00819300
*                                                                       00819400
      ELSEIF(IDTYPE.EQ.2)THEN                                           00820000
*                                                                       00820100
        IF(ICAOID(3:3).EQ.' ')THEN                                      00820300
          IDLIST=1                                                      00820400
        ENDIF                                                           00820500
*                                                                       00821000
        IF(IDLIST.EQ.0)THEN                                             00821100
          IF((ICAOID.LT.'AAAA').OR.(ICAOID.GT.'ZZZZ')) GOTO 200         00822000
*                                                                       00825000
          IREC=350                                                      00830000
*                                                                       00840000
  11      READ(85,5,REC=IREC)                                           00850000
     *              ITOTAL,(STNCHRID(II),RECID(II),II=1,50)             00860000
   5      FORMAT(5X,I2,1X,50(A4,1X,I5,1X))                              00870000
*                                                                       00880000
          DO 25 ILOOP=1,50                                              00981100
            IF(ICAOID.EQ.STNCHRID(ILOOP))THEN                           00981200
              IF(POS.EQ.'L')THEN                                        00981300
                KEPICAO=STNCHRID(ILOOP)                                 00981400
                  DO 61 KKK=1,10                                        00981500
                    IF(KEPICAO.NE.STNCHRID(ILOOP+KKK))THEN              00981600
                      INDREC(JJJ)=RECID(ILOOP+KKK-1)                    00981700
                      JJJ=JJJ+1                                         00981800
                      GOTO 200                                          00981900
                    ENDIF                                               00982000
  61              CONTINUE                                              00982100
              ELSE                                                      00982200
                INDREC(JJJ)=RECID(ILOOP)                                00982300
                JJJ=JJJ+1                                               00982400
                GOTO 25                                                 00982500
              ENDIF                                                     00982600
            ENDIF                                                       00982700
            IF(STNCHRID(ILOOP).EQ.'    ') GOTO 200                      00982800
  25      CONTINUE                                                      00983600
*                                                                       00984000
          IREC=IREC+1                                                   00990000
          GOTO 11                                                       01000000
        ELSEIF(IDLIST.EQ.1)THEN                                         01000100
          IF((ICAOID(1:2).LT.'AA').OR.(ICAOID(1:2).GT.'ZZ')) GOTO 200   01000300
*                                                                       01000600
          IREC=350                                                      01000700
*                                                                       01000800
  71      READ(85,75,REC=IREC)                                          01000900
     *              ITOTAL,(STNCHRID(II),RECID(II),II=1,50)             01001000
  75      FORMAT(5X,I2,1X,50(A4,1X,I5,1X))                              01001100
*                                                                       01001200
          DO 77 ILOOP=1,50                                              01001400
            IF(ICAOID(1:2).EQ.STNCHRID(ILOOP)(1:2))THEN                 01001500
              IF(POS.EQ.'L')THEN                                        01001600
                IF(KEPICAO.EQ.STNCHRID(ILOOP)) GOTO 77                  01001700
                KEPICAO=STNCHRID(ILOOP)                                 01001800
                  DO 76 KKK=1,10                                        01001900
                    IF(KEPICAO.NE.STNCHRID(ILOOP+KKK))THEN              01002000
                      INDREC(JJJ)=RECID(ILOOP)                          01002100
                      JJJ=JJJ+1                                         01002200
                      GOTO 77                                           01002300
                    ENDIF                                               01002400
  76              CONTINUE                                              01002500
              ELSE                                                      01002600
                INDREC(JJJ)=RECID(ILOOP)                                01002700
                JJJ=JJJ+1                                               01002800
                GOTO 77                                                 01002900
              ENDIF                                                     01003000
            ENDIF                                                       01003100
            IF(STNCHRID(ILOOP).EQ.'    ') GOTO 200                      01003200
  77      CONTINUE                                                      01003900
*                                                                       01004000
          IREC=IREC+1                                                   01004100
          GOTO 71                                                       01004200
        ENDIF                                                           01004300
*                                                                       01004400
* IF DCNNID                                                             01004500
*                                                                       01005000
      ELSEIF(IDTYPE.EQ.3)THEN                                           01010000
*                                                                       01010100
        IF(DCNNID.LE.99)THEN                                            01010200
          IDLIST=1                                                      01010300
        ENDIF                                                           01010400
*                                                                       01011000
        IF(IDLIST.EQ.0)THEN                                             01011100
          IF((DCNNID.LT.0).OR.(DCNNID.GT.9999)) GOTO 200                01012000
*                                                                       01015000
          IREC=500                                                      01020000
*                                                                       01030000
  12       READ(85,6,REC=IREC)                                          01040000
     *                  ITOTAL,(STNINTID(II),RECID(II),II=1,50)         01050000
   6       FORMAT(5X,I2,1X,50(I4,1X,I5,1X))                             01060000
*                                                                       01070000
           DO 23 ILOOP=1,50                                             01080000
             IF(DCNNID.EQ.STNINTID(ILOOP))THEN                          01090000
               IF(POS.EQ.'L')THEN                                       01091000
                 KEPDCNN=STNINTID(ILOOP)                                01092100
                 DO 68 KKK=1,10                                         01093000
                   IF(KEPDCNN.NE.STNINTID(ILOOP+KKK))THEN               01094000
                     INDREC(JJJ)=RECID(ILOOP+KKK-1)                     01100000
                     JJJ=JJJ+1                                          01101000
                     GOTO 200                                           01110000
                   ENDIF                                                01111000
  68             CONTINUE                                               01112000
               ELSE                                                     01113000
                 INDREC(JJJ)=RECID(ILOOP)                               01114000
                 JJJ=JJJ+1                                              01114100
                 GOTO 23                                                01115000
               ENDIF                                                    01116000
             ENDIF                                                      01120000
             IF(STNINTID(ILOOP).EQ.0) GOTO 200                          01130000
  23       CONTINUE                                                     01160000
*                                                                       01170000
           IREC=IREC+1                                                  01180000
           GOTO 12                                                      01190000
         ELSEIF(IDLIST.EQ.1)THEN                                        01191000
           IF((DCNNID.LT.0).OR.(DCNNID.GT.99)) GOTO 200                 01192000
*                                                                       01195000
           IREC=500                                                     01196000
*                                                                       01197000
  62       READ(85,66,REC=IREC)                                         01198000
     *                  ITOTAL,(STNINTID(II),RECID(II),II=1,50)         01199000
  66       FORMAT(5X,I2,1X,50(I4,1X,I5,1X))                             01199100
*                                                                       01199200
           DO 63 ILOOP=1,50                                             01199300
             IF(DCNNID.EQ.STNINTID(ILOOP)/100)THEN                      01199400
               IF(POS.EQ.'L')THEN                                       01199500
                 IF(KEPDCNN.EQ.STNINTID(ILOOP)) GOTO 63                 01199600
                 KEPDCNN=STNINTID(ILOOP)                                01199700
                   DO 69 KKK=1,10                                       01199800
                     IF(KEPDCNN.NE.STNINTID(ILOOP+KKK))THEN             01199900
                       INDREC(JJJ)=RECID(ILOOP+KKK-1)                   01200100
                       JJJ=JJJ+1                                        01200200
                       GOTO 63                                          01200300
                     ENDIF                                              01200400
  69               CONTINUE                                             01200500
               ELSE                                                     01200600
                 INDREC(JJJ)=RECID(ILOOP)                               01200700
                 JJJ=JJJ+1                                              01200800
                 GOTO 63                                                01200900
               ENDIF                                                    01201000
             ENDIF                                                      01201100
             IF(STNINTID(ILOOP).EQ.0) GOTO 200                          01201200
  63       CONTINUE                                                     01201900
*                                                                       01202000
           IREC=IREC+1                                                  01203000
           GOTO 62                                                      01204000
         ENDIF                                                          01210000
*                                                                       01320000
* IF RAINID                                                             01320100
*                                                                       01320200
      ELSEIF(IDTYPE.EQ.4)THEN                                           01320300
*                                                                       01320400
        IF(RAINID.LE.99)THEN                                            01320600
          IDLIST=1                                                      01320700
        ENDIF                                                           01320800
*                                                                       01320900
        IF(IDLIST.EQ.0)THEN                                             01321000
          IF((RAINID.LT.0).OR.(RAINID.GT.999999)) GOTO 200              01321100
*                                                                       01321400
          IREC=550                                                      01321500
*                                                                       01321600
  13      READ(85,7,REC=IREC)                                           01321700
     *               ITOTAL,(STNINTID(II),RECID(II),II=1,50)            01321800
   7      FORMAT(5X,I2,1X,50(I6,1X,I5,1X))                              01321900
*                                                                       01322000
          DO 24 ILOOP=1,50                                              01322100
            IF(RAINID.EQ.STNINTID(ILOOP))THEN                           01322200
              IF(POS.EQ.'L')THEN                                        01322300
                KEPRAIN=STNINTID(ILOOP)                                 01322400
                DO 29 KKK=1,10                                          01322500
                  IF(KEPRAIN.NE.STNINTID(ILOOP+KKK))THEN                01322600
                    INDREC(JJJ)=RECID(ILOOP+KKK-1)                      01322700
                    JJJ=JJJ+1                                           01322800
                    GOTO 200                                            01322900
                  ENDIF                                                 01323000
  29            CONTINUE                                                01323100
              ELSE                                                      01323200
                INDREC(JJJ)=RECID(ILOOP)                                01323300
                JJJ=JJJ+1                                               01323400
                GOTO 24                                                 01323500
              ENDIF                                                     01323600
            ENDIF                                                       01323700
            IF(STNINTID(ILOOP).EQ.0) GOTO 200                           01323800
  24      CONTINUE                                                      01324600
*                                                                       01324700
          IREC=IREC+1                                                   01324800
          GOTO 13                                                       01324900
        ELSEIF(IDLIST.EQ.1)THEN                                         01325000
          IF((RAINID.LT.0).OR.(RAINID.GT.99)) GOTO 200                  01325100
*                                                                       01325400
          IREC=550                                                      01325500
*                                                                       01325600
  83      READ(85,87,REC=IREC)                                          01325700
     *               ITOTAL,(STNINTID(II),RECID(II),II=1,50)            01325800
  87      FORMAT(5X,I2,1X,50(I6,1X,I5,1X))                              01325900
*                                                                       01326000
          DO 84 ILOOP=1,50                                              01326100
            IF(RAINID.EQ.STNINTID(ILOOP)/10000)THEN                     01326200
              IF(POS.EQ.'L')THEN                                        01326300
                IF(KEPRAIN.EQ.STNINTID(ILOOP)) GOTO 84                  01326400
                KEPRAIN=STNINTID(ILOOP)                                 01326500
                DO 89 KKK=1,10                                          01326600
                  IF(KEPRAIN.NE.STNINTID(ILOOP+KKK))THEN                01326700
                    INDREC(JJJ)=RECID(ILOOP+KKK-1)                      01326800
                    JJJ=JJJ+1                                           01326900
                    GOTO 84                                             01327000
                  ENDIF                                                 01327100
  89            CONTINUE                                                01327200
              ELSE                                                      01327300
                INDREC(JJJ)=RECID(ILOOP)                                01327400
                JJJ=JJJ+1                                               01327500
                GOTO 84                                                 01327600
              ENDIF                                                     01327700
            ENDIF                                                       01327800
            IF(STNINTID(ILOOP).EQ.0) GOTO 200                           01327900
  84      CONTINUE                                                      01328600
*                                                                       01328700
          IREC=IREC+1                                                   01328800
          GOTO 83                                                       01328900
        ENDIF                                                           01329000
*                                                                       01329100
      ENDIF                                                             01329200
*                                                                       01330000
 200  IF(IDTYPE.EQ.1)THEN                                               01331000
        IF(WMOLST(COUNT).NE.0)THEN                                      01332000
          GOTO 800                                                      01333000
        ENDIF                                                           01334000
      ELSEIF(IDTYPE.EQ.2)THEN                                           01335000
        IF(ICAOLST(COUNT).NE.'    ')THEN                                01336000
          GOTO 800                                                      01337000
        ENDIF                                                           01338000
      ELSEIF(IDTYPE.EQ.3)THEN                                           01339000
        IF(DCNNLST(COUNT).NE.0)THEN                                     01339100
          GOTO 800                                                      01339200
        ENDIF                                                           01339300
      ELSEIF(IDTYPE.EQ.4)THEN                                           01339400
        IF(RAINLST(COUNT).NE.0)THEN                                     01339500
          GOTO 800                                                      01339600
        ENDIF                                                           01339700
      ENDIF                                                             01339800
*                                                                       01340000
      DO 999 IJK=1,1000                                                 01340100
        IF(INDREC(IJK).GT.0)THEN                                        01340200
           ITOT=ITOT+1                                                  01340300
        ENDIF                                                           01340400
 999  CONTINUE                                                          01340500
*                                                                       01340600
      IF(ITOT.LE.0)THEN                                                 01341200
        ISTAT=8                                                         01341300
      ELSEIF(ITOT.EQ.1)THEN                                             01341400
        ISTAT=0                                                         01341500
      ELSEIF(ITOT.GT.1)THEN                                             01341700
        ISTAT=4                                                         01341800
      ENDIF                                                             01342000
*                                                                       01342100
      RETURN                                                            01343000
      END                                                               01480000
