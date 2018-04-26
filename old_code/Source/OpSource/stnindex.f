*********************************************************************** 00009500
*                                                                     * 00009600
* PROGRAM       : STNINDEX                                            * 00009700
*                                                                     * 00009800
* PURPOSE       : TO CREATE AN INDEX DATASET FOR THE MAIN             * 00009900
*                 STATION MASTER DATASET                              * 00010000
*                                                                     * 00010100
* DESCRIPTION   : LOOPS OVER ALL THE RECORDS IN THE MAIN STATION      * 00010200
*                 MASTER DATASET CREATING INDEX ENTRIES FOR ALL       * 00010300
*                 THE KEY FIELDS                                      * 00010400
*                                                                     * 00010500
* CALLED BY     : NONE (STAND ALONE...MUST RUN AFTER STNSYNM)         * 00010600
*                                                                     * 00010700
* CALLS         : NONE                                                * 00010800
*                                                                     * 00010900
* PARAMETERS    : NONE                                                * 00011000
*                                                                     * 00011100
*Y2K  16.06.1997  STNINDEX IS YEAR 2000 COMPLIANT.                      00011203
*                                                                     * 00011303
* CHANGE RECORD :                                                     * 00011403
*                                                                     * 00011503
*   DATE : MAY 92  PURPOSE: FIRST VERSION OF SOFTWARE                 * 00011603
*                                                                     * 00011703
*    !A  : 15-07-96 - S.COX - CORRECTED BUG WHICH MEANT THAT ONE MORE * 00011803
*                     ICAO WAS WRITTEN TO SDB.STNMAS.INDEXB THAN      * 00011903
*                     THERE ACTUALLY WAS. THE PROBLEM WAS ILOOP2      * 00012003
*                     THE SAME HAPPENS WITH RAIN ARRAYS (ILOOP3) AND  * 00012103
*                     DCNNS (ILOOP4). THESE HAVE ALSO BEEN CORRECTED  * 00012203
*                                                                     * 00012303
*    !B  : 10-04-97 - R HIRST - INCREASED SIZES OF ICAO ARRAYS        * 00012403
*                     ICAOAR1, ICAOAR2 AND IRANK FROM 4000 TO 6000    * 00012503
*                                                                     * 00012602
*    !C  : 16-11-98 - R HIRST - READ OPEN/CLOSE DATE/TIME FIELDS AS   * 00012704
*                     CHAR*2 (WERE INTEGER) TO COPE WITH UNKNOWNS (--)* 00012804
*                                                                     * 00012904
*********************************************************************** 00013002
!›$Log:
!? 1    Met_DB_Project 1.0         30/01/2006 20:24:36    Sheila Needham  
!?$
!›Revision 1.1  1998/11/12 08:45:33  usmdb
!›Initial revision
!›                                                                00014005
      INTEGER RECNO,BLK,STN,DCNN,RAIN,REGION,LAT1,LAT2                  02741400
      INTEGER LONG1,LONG2,HT1,HT2,CLASS,HAW00,HAW12                     02741500
      INTEGER MAP,RUN1,RUN2,OREC,NREC,NUM                               02741700
      INTEGER NCMTIM1(10),NCMTIM2(10),SQUARE                            02741800
      INTEGER IBLKSTN,BLKSTN                                            02742000
      CHARACTER*4 ICAO,UA                                               02742900
      CHARACTER*6 CHASER                                                02743000
      CHARACTER*24 FLAGS                                                02743100
      CHARACTER*24 SYNTIM(10)                                           02743200
      CHARACTER*13 SYN                                                  02743300
      CHARACTER*23 NCM                                                  02743400
      CHARACTER*9 DAY(10)                                               02743500
      CHARACTER*42 STATION                                              02743600
      CHARACTER*32 COUNTRY                                              02743700
      CHARACTER*1 LATP,LONGP                                            02743800
      CHARACTER*2 COYEAR,COMONTH,CODAY,COHOUR                        !C 02743904
      CHARACTER*2 CCYEAR,CCMONTH,CCDAY,CCHOUR                        !C 02744004
      CHARACTER*132 HEAD                                                02744105
*                                                                       02744205
      INTEGER BLKIND(2,50),STNIND(2,50)                                 02744305
      CHARACTER*4 ICAOAR1(6000)                                      !B 02744405
      INTEGER ILOOP2,ILOOP3                                             02744505
      INTEGER ICAOAR2(6000),IRANK(6000)                              !B 02744605
      INTEGER RAINAR1(4000),RAINAR2(4000),IRANK2(4000)                  02744705
      INTEGER DCNNAR1(4000),DCNNAR2(4000),IRANK3(4000)                  02744805
      INTEGER III,KEPBLK,KEPSTN,I,J,KK,L,TOTREC,ITOTAL                  02744905
*                                                                       02773000
* OPEN STATION MASTER AND INDEX                                         02773100
*                                                                       02773200
      OPEN(15,ACCESS='DIRECT',FORM='FORMATTED',RECL=692)                02774000
      OPEN(14,ACCESS='DIRECT',FORM='FORMATTED',RECL=692)                02775000
*                                                                       02775600
      HEAD='                                                            02775705
     &›$Source: /home/us0400/mdb/op/lib/source/RCS/stnindex.F,v $                                                        02775805
     &'//'›$ $Date: 30/01/2006 20:24:36$ $Revision: 1$                                       02775905
*                                                                       02776005
* READ FIRST RECORD                                                     02776100
*                                                                       02777000
      READ(15,14,REC=2)TOTREC                                           02777100
  14  FORMAT(12X,I5)                                                    02777200
*                                                                       02777600
* SET INITIAL VARIABLE VALUES                                           02777700
*                                                                       02777800
       KEPBLK=01                                                        02777900
       KEPSTN=001                                                       02778000
       I=1                                                              02778100
       J=10                                                             02778200
       KK=1                                                             02778300
       L=1                                                              02778400
       ITOTAL=0                                                         02778500
       ILOOP2=0         !A - NOW INITIALISED TO 0                       02778800
       ILOOP3=0         !A - NOW INITIALISED TO 0                       02778900
       ILOOP4=0         !A - NOW INITIALISED TO 0                       02779000
*                                                                       02779100
       BLKIND(1,KK)=01                                                  02779200
       BLKIND(2,KK)=J                                                   02779300
       KK=KK+1                                                          02779400
       DO ILOOP=1,6000                                               !B 02779501
         ICAOAR1(ILOOP)='    '                                          02779601
         ICAOAR2(ILOOP)=0                                               02779701
       ENDDO                                                            02780201
       DO ILOOP=1,4000                                                  02780301
         RAINAR1(ILOOP)=0                                               02780600
         RAINAR2(ILOOP)=0                                               02780700
         DCNNAR1(ILOOP)=0                                               02780800
         DCNNAR2(ILOOP)=0                                               02780900
       ENDDO                                                            02781001
*                                                                       02781100
* LOOP OVER ALL RECORDS IN DATASET                                      02781200
*                                                                       02781300
      DO 10 III=3,TOTREC                                                02781400
*                                                                       02781500
* READ DATASET                                                          02781600
*                                                                       02781700
 200    READ(15,15,REC=III)BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,LONG2,     02781800
     *    LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,               02781900
     *    COYEAR,COMONTH,CODAY,COHOUR,CCYEAR,CCMONTH,CCDAY,CCHOUR,   !C 02782004
     *    FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,              02782100
     *    UA,SYN,NCM,NUM,                                               02782200
     *    (DAY(K),SYNTIM(K),NCMTIM1(K),NCMTIM2(K),K=1,10),              02782300
     *    SQUARE,CHASER,RECNO                                           02782400
*                                                                       02782500
  15    FORMAT(1X,I2.2,I3.3,1X,A4,1X,I2.2,I2.2,A1,1X,I3.3,I2.2,A1,      02782600
     *    1X,I6,1X,I6,1X,A42,1X,A32,1X,I2,1X,I4.4,1X,I6,                02782700
     *    1X,A2,A2,A2,A2,1X,A2,A2,A2,A2,                             !C 02782804
     *    1X,A24,1X,I2,1X,I5,1X,I5,1X,I3,1X,I5.5,1X,I5.5,               02782900
     *    1X,I5.5,1X,I5.5,1X,A4,1X,A13,1X,A23,1X,I2,                    02783000
     *    1X,10(A9,1X,A24,1X,I2,1X,I2,1X),I4,1X,A6,1X,I5)               02783100
*                                                                       02783200
        BLKSTN=BLK*1000+STN                                             02783300
*                                                                       02783400
* LOOK FOR ICAO ID AND RAIN ID                                          02783500
*                                                                       02783600
        IF(ICAO.NE.'    ')THEN                                          02783700
           ILOOP2=ILOOP2+1             !A - MOVED TO TOP OF IF BLOCK    02783800
           ICAOAR1(ILOOP2)=ICAO                                         02783900
           ICAOAR2(ILOOP2)=III                                          02784000
        ENDIF                                                           02784100
        IF(RAIN.NE.0)THEN                                               02784200
           ILOOP3=ILOOP3+1             !A - MOVED TO TOP OF IF BLOCK    02784300
           RAINAR1(ILOOP3)=RAIN                                         02784400
           RAINAR2(ILOOP3)=III                                          02784500
        ENDIF                                                           02784600
        IF(DCNN.NE.0)THEN                                               02784700
           ILOOP4=ILOOP4+1             !A - MOVED TO TOP OF IF BLOCK    02784800
           DCNNAR1(ILOOP4)=DCNN                                         02784900
           DCNNAR2(ILOOP4)=III                                          02785000
        ENDIF                                                           02785100
        IF(BLK.EQ.KEPBLK)THEN                                           02785200
          IF(BLKSTN.NE.99999)THEN                                       02785300
            STNIND(1,L)=BLKSTN                                          02785400
            STNIND(2,L)=III                                             02785500
            L=L+1                                                       02785600
            ITOTAL=ITOTAL+1                                             02785700
          ENDIF                                                         02785800
*                                                                       02785900
* END OF RECORD PROCESSING (STN)                                        02786000
*                                                                       02786100
          IF(L.GE.51)THEN                                               02786200
            WRITE(14,12,REC=J)ITOTAL,((STNIND(II,JJ),II=1,2),JJ=1,50)   02786300
  12        FORMAT(5X,I2,1X,50(I5,1X,I5,1X))                            02786400
            J=J+1                                                       02786500
            L=1                                                         02786600
            ITOTAL=0                                                    02786700
*                                                                       02786800
            DO 5 ILOOP=1,50                                             02786900
              STNIND(1,ILOOP)=0                                         02787000
              STNIND(2,ILOOP)=0                                         02787100
   5        CONTINUE                                                    02787200
          ENDIF                                                         02787300
        ENDIF                                                           02787400
*                                                                       02787500
        IF(BLK.NE.KEPBLK)THEN                                           02787600
           PRINT*,'TOTAL FOR BLOCK ',KEPBLK,' IS ',ITOTAL               02787700
           WRITE(14,12,REC=J)ITOTAL,((STNIND(II,JJ),II=1,2),JJ=1,50)    02787800
           KEPBLK=BLK                                                   02787900
           ITOTAL=0                                                     02788000
           J=J+1                                                        02788100
           L=1                                                          02788200
           BLKIND(1,KK)=BLK                                             02788300
           BLKIND(2,KK)=J                                               02788400
           KK=KK+1                                                      02788500
*                                                                       02788600
             DO 6 ILOOP=1,50                                            02788700
               STNIND(1,ILOOP)=0                                        02788800
               STNIND(2,ILOOP)=0                                        02788900
   6         CONTINUE                                                   02789000
*                                                                       02789100
* END OF RECORD PROCESSING (BLK)                                        02789200
*                                                                       02789300
           IF(KK.GE.51)THEN                                             02789400
             WRITE(14,11,REC=I)((BLKIND(II,JJ),II=1,2),JJ=1,50)         02789500
  11         FORMAT(5X,50(I2,1X,I5,1X))                                 02789600
             I=I+1                                                      02789700
             KK=1                                                       02789800
             DO 7 ILOOP=1,50                                            02789900
               BLKIND(1,ILOOP)=0                                        02790000
               BLKIND(2,ILOOP)=0                                        02790100
   7         CONTINUE                                                   02790200
           ENDIF                                                        02790300
*                                                                       02790400
           STNIND(1,L)=BLKSTN                                           02790500
           STNIND(2,L)=III                                              02790600
           L=L+1                                                        02790700
           ITOTAL=ITOTAL+1                                              02790800
*                                                                       02790900
        ENDIF                                                           02791000
*                                                                       02791100
  10  CONTINUE                                                          02791200
*                                                                       02791300
* WRITE END OF BLK AND STN INDEX                                        02791400
*                                                                       02791500
 999  WRITE(14,11,REC=I)((BLKIND(II,JJ),II=1,2),JJ=1,50)                02792000
      WRITE(14,12,REC=J)ITOTAL,((STNIND(II,JJ),II=1,2),JJ=1,50)         02800000
*                                                                       03300000
* PROCESS AND STORE ICAO INDEX                                          03310000
*                                                                       03320000
      IFAIL=0                                                           03320100
      CALL M01DCE(ICAOAR1,1,ILOOP2,1,4,'A',IRANK,IFAIL)  !A - NOW ILOOP203321000
      IF(IFAIL.NE.0)THEN                                                03322000
        PRINT*,'RANKING HAS GONE WRONG'                                 03323000
      ENDIF                                                             03324000
*                                                                       03324100
      CALL M01ECE(ICAOAR1,1,ILOOP2,IRANK,IFAIL)          !A - NOW ILOOP203325000
      IF(IFAIL.NE.0)THEN                                                03326000
        PRINT*,'RANKING HAS GONE WRONG'                                 03327000
      ENDIF                                                             03328000
*                                                                       03329000
      CALL M01EBE(ICAOAR2,1,ILOOP2,IRANK,IFAIL)          !A - NOW ILOOP203329100
      IF(IFAIL.NE.0)THEN                                                03329200
        PRINT*,'RANKING HAS GONE WRONG'                                 03329300
      ENDIF                                                             03329400
*                                                                       03330000
* LOOP OVER ICAO ARRAYS PUTTING DATA IN INDEX DATASET                   03340000
*                                                                       03350000
      IEND=ILOOP2                                                       03350200
      I=350                                                             03351000
      II=1                                                              03351100
      ITOTAL=50                                                         03351200
      J=1                                                               03351300
      K=J+49                                                            03351400
*                                                                       03351600
 444  IF(K.GT.IEND)THEN                                                 03351700
        K=IEND                                                          03351900
        ITOTAL=K-J+1                                                    03352000
      ENDIF                                                             03352200
*                                                                       03352300
* STORE ICAO INDEX ENTRIES                                              03353000
*                                                                       03354000
        WRITE(14,13,REC=I)ITOTAL,((ICAOAR1(II),ICAOAR2(II)),II=J,K)     03360000
  13    FORMAT(5X,I2,1X,50(A4,1X,I5,1X))                                03370000
*                                                                       03370100
      IF(K.EQ.IEND) GOTO 111                                            03370200
      I=I+1                                                             03370300
      J=K+1                                                             03370400
      K=J+49                                                            03370500
      GOTO 444                                                          03370600
*                                                                       03370700
* PROCESS AND STORE RAIN INDEX                                          03370800
*                                                                       03370900
 111  IFAIL=0                                                           03371000
      PRINT*,'ILOOP3 IS ',ILOOP3                       !A - NOW ILOOP3  03371100
      CALL M01DBE(RAINAR1,1,ILOOP3,'A',IRANK2,IFAIL)   !A - NOW ILOOP3  03371200
      IF(IFAIL.NE.0)THEN                                                03371300
        PRINT*,'RANKING HAS GONE WRONG'                                 03371400
      ENDIF                                                             03371500
*                                                                       03371600
      CALL M01EBE(RAINAR1,1,ILOOP3,IRANK2,IFAIL)       !A - NOW ILOOP3  03371700
      IF(IFAIL.NE.0)THEN                                                03371800
        PRINT*,'RANKING HAS GONE WRONG'                                 03371900
      ENDIF                                                             03372000
*                                                                       03372100
      CALL M01EBE(RAINAR2,1,ILOOP3,IRANK2,IFAIL)       !A - NOW ILOOP3  03372200
      IF(IFAIL.NE.0)THEN                                                03372300
        PRINT*,'RANKING HAS GONE WRONG'                                 03372400
      ENDIF                                                             03372500
*                                                                       03372900
* LOOP OVER RAIN ARRAYS PUTTING DATA IN INDEX DATASET                   03373000
*                                                                       03373100
      IEND=ILOOP3                                                       03373300
      I=550                                                             03373400
      II=1                                                              03373500
      ITOTAL=50                                                         03373600
      J=1                                                               03373700
      K=J+49                                                            03373800
*                                                                       03373900
 445  IF(K.GT.IEND)THEN                                                 03374000
        K=IEND                                                          03374100
        ITOTAL=K-J+1                                                    03374200
      ENDIF                                                             03374300
*                                                                       03374400
* STORE RAIN INDEX ENTRIES                                              03374500
*                                                                       03374600
        WRITE(14,21,REC=I)ITOTAL,((RAINAR1(II),RAINAR2(II)),II=J,K)     03374700
  21    FORMAT(5X,I2,1X,50(I6,1X,I5,1X))                                03374800
*                                                                       03374900
      IF(K.EQ.IEND) GOTO 555                                            03375000
      I=I+1                                                             03375100
      J=K+1                                                             03375200
      K=J+49                                                            03375300
      GOTO 445                                                          03376000
*                                                                       03861100
* PROCESS AND STORE DCNN INDEX                                          03861200
*                                                                       03861300
 555  IFAIL=0                                                           03861400
      CALL M01DBE(DCNNAR1,1,ILOOP4,'A',IRANK3,IFAIL)   !A - NOW ILOOP4  03861500
      IF(IFAIL.NE.0)THEN                                                03861600
        PRINT*,'RANKING HAS GONE WRONG'                                 03861700
      ENDIF                                                             03861800
*                                                                       03861900
      CALL M01EBE(DCNNAR1,1,ILOOP4,IRANK3,IFAIL)       !A - NOW ILOOP4  03862000
      IF(IFAIL.NE.0)THEN                                                03862100
        PRINT*,'RANKING HAS GONE WRONG'                                 03862200
      ENDIF                                                             03862300
*                                                                       03862400
      CALL M01EBE(DCNNAR2,1,ILOOP4,IRANK3,IFAIL)       !A - NOW ILOOP4  03862500
      IF(IFAIL.NE.0)THEN                                                03862600
        PRINT*,'RANKING HAS GONE WRONG'                                 03862700
      ENDIF                                                             03862800
*                                                                       03862900
* LOOP OVER DCNN ARRAYS PUTTING DATA IN INDEX DATASET                   03863000
*                                                                       03863100
      IEND=ILOOP4                                                       03863300
      I=500                                                             03863400
      II=1                                                              03863500
      ITOTAL=50                                                         03863600
      J=1                                                               03863700
      K=J+49                                                            03863800
*                                                                       03863900
 446  IF(K.GT.IEND)THEN                                                 03864000
        K=IEND                                                          03864100
        ITOTAL=K-J+1                                                    03864200
      ENDIF                                                             03864300
*                                                                       03864400
* STORE DCNN INDEX ENTRIES                                              03864500
*                                                                       03864600
      WRITE(14,22,REC=I)ITOTAL,((DCNNAR1(II),DCNNAR2(II)),II=J,K)       03864700
  22  FORMAT(5X,I2,1X,50(I4,1X,I5,1X))                                  03864800
*                                                                       03864900
      IF(K.EQ.IEND) GOTO 666                                            03865000
      I=I+1                                                             03865100
      J=K+1                                                             03865200
      K=J+49                                                            03865300
      GOTO 446                                                          03865400
*                                                                       03865500
* CLEAR TEMPORARY RECORDS                                               03865600
*                                                                       03865700
  666 ITOTAL=0                                                          03865800
*                                                                       03865900
      DO 500 IJK=1,2                                                    03866000
        DO 501 IJK2=1,4000                                              03866100
           STNIND(IJK,IJK2)=0                                           03866200
 501    CONTINUE                                                        03866300
 500  CONTINUE                                                          03866400
*                                                                       03866500
      DO IJK=1,6000                                                  !B 03866601
         ICAOAR1(IJK)='    '                                            03866701
         ICAOAR2(IJK)=0                                                 03866801
      ENDDO                                                             03867301
      DO IJK=1,4000                                                     03867401
         RAINAR1(IJK)=0                                                 03867700
         RAINAR2(IJK)=0                                                 03867800
         DCNNAR1(IJK)=0                                                 03867900
         DCNNAR2(IJK)=0                                                 03868000
      ENDDO                                                             03868101
      WRITE(14,12,REC=590)ITOTAL,((STNIND(II,JJ),II=1,2),JJ=1,50)       03868200
      WRITE(14,13,REC=591)ITOTAL,((ICAOAR1(II),ICAOAR2(II)),II=1,50)    03868300
      WRITE(14,21,REC=592)ITOTAL,((RAINAR1(II),RAINAR2(II)),II=1,50)    03868400
      WRITE(14,22,REC=593)ITOTAL,((DCNNAR1(II),DCNNAR2(II)),II=1,50)    03868500
*                                                                       03868600
      STOP                                                              03869000
      END                                                               03870000
