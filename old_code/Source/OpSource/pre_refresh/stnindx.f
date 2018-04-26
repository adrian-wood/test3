      SUBROUTINE STNINDX(WMOLST,ICAOLST,DCNNLST,RAINLST,
     &                   IDTYPE,POS,ISTAT,INDREC,INDWMO)


!-----------------------------------------------------------------------
!
! PROGRAM       : STNINDX
!
! PURPOSE       : TO SEARCH FOR STATION IDENTIFIER IN INDEX
!
! DESCRIPTION   : TO RETURN BLOCK NUMBER TO READ FOR STATION DETAILS
!
! DATA TYPE(S)  : STATION NUMBERS, ICAO IDENTIFIERS, DCNN
!
! CALLED BY     : STNRET
!                 STNDISPA
!
! CALLS         : NONE
!
! PARAMETERS    : (1-4) LIST OF IDENTIFIERS OF ONE TYPE OR ANOTHER
!                 (5) TYPE (1: WMO  2: ICAO  3: DCNN  4: RAINFALL)
!                 (6) IF 'L', RETURN LATEST POSITION ONLY; OTHERWISE
!                      RETURN ALL PAST POSITIONS TOO.
!                 (7) RETURN CODE
!                 (8) RECORD NUMBERS (TO BE RETURNED)
!                 (9) BLOCK/STATION NUMBERS (TO BE RETURNED)
!
!Y2K  26.06.1997  STNINDX IS YEAR 2000 COMPLIANT.
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 15/05/2006 14:15:07$
! $Source: /data/us0400/mdb/op/lib/source/RCS/stnindx.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         15/05/2006 14:15:07    Stan Kellett
!       Increased size of two arrays from 5000 to 8000
!  1    Met_DB_Project 1.0         30/01/2006 20:24:38    Sheila Needham  
! $
! Revision 2.0  2001/01/08  11:59:15  11:59:15  usmdb (MetDB account c/o John C
! Ward)
! Separated variable declaration and initialisation for
! WMOBLK and NWMOBLK. Moved all variable declarations above
! DATA statements. Added copyright and modified header - S.Cox
!
! Revision 1.1  97/08/04  13:51:56  13:51:56  uspm (Pat McCormack)
! Initial revision
!
! SEP 95       ADDITION OF PARAMETER 9
! FEB 93       READ WHOLE INDEX AT START & KEEP IT FOR FURTHER USE
! 19/10/92     ALLOW ICAO REGION RETRIEVAL
!
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      INTEGER WMOLST(49),DCNNLST(49),RAINLST(49)
      INTEGER INDREC(1000),INDWMO(1000)
      CHARACTER*4   ICAOLST(49)

      CHARACTER*1   POS
      CHARACTER*132 HEAD

      INTEGER WMOID,DCNNID,RAINID, COUNT
*
      INTEGER WMOBLK(0:99)                                          !2.0
      INTEGER NWMOBLK(0:99)                                         !2.0
      INTEGER WMOSTN(15000),WMOREC(15000)
      INTEGER DCNNSTN(500),DCNNREC(500)
      INTEGER RAINSTN(500),RAINREC(500)
      CHARACTER*4 ICAOSTN(8000)                                    !2
      INTEGER ICAOREC(8000)                                        !2
      CHARACTER*4 ICAOID,KEPICAO
      LOGICAL REGION
*
      COMMON /STNRECNO/  WMOSTN,WMOREC,  DCNNSTN,DCNNREC,
     &                  RAINSTN,RAINREC, ICAOSTN,ICAOREC
*
      DATA LWMO/0/, LICAO/0/, LDCNN/0/, LRAIN/0/   ! TEMPORARY COUNTS
      DATA NWMO/0/, NICAO/0/, NDCNN/0/, NRAIN/0/   ! MAIN COUNTS
      DATA WMOBLK/100*0/                                            !2.0
      DATA NWMOBLK/100*0/                                           !2.0
*
* INITIALISE VARIABLES
*
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/stnindx.f,v $
     &'//'$ $Date: 15/05/2006 14:15:07$ $Revision: 2$'

      REGION=.FALSE.
      NFOUND=0
      COUNT=1
***********************************************************************
*
* TAKE NEXT IDENTIFIER FROM LIST
*
***********************************************************************
  100 IF (IDTYPE.EQ.1) THEN
        IF (WMOLST(COUNT).NE.0) THEN
          WMOID=WMOLST(COUNT)
          COUNT=COUNT+1
        ELSE
          GO TO 990
        ENDIF
*
      ELSE IF (IDTYPE.EQ.2) THEN
        IF (ICAOLST(COUNT).NE.'    ') THEN
          ICAOID=ICAOLST(COUNT)(1:4)
          COUNT=COUNT+1
        ELSE
          GO TO 990
        ENDIF
*
      ELSE IF (IDTYPE.EQ.3) THEN
        IF (DCNNLST(COUNT).NE.0) THEN
          DCNNID=DCNNLST(COUNT)
          COUNT=COUNT+1
        ELSE
          GO TO 990
        ENDIF
*
      ELSE IF (IDTYPE.EQ.4) THEN
        IF (RAINLST(COUNT).NE.0) THEN
          RAINID=RAINLST(COUNT)
          COUNT=COUNT+1
        ELSE
          GO TO 990
        ENDIF
      ENDIF
***********************************************************************
*
* FIRST LOOK FOR IDENTIFIER IN TEMPORARY RECORDS
*
* (READ RECORD ONLY IF FIRST TIME FOR THIS KIND OF IDENTIFIER)
*
***********************************************************************
*
* IF WMO BLK AND STATION NUMBER
*
      IF (IDTYPE.EQ.1) THEN
        IF (NWMO.EQ.0) THEN
          READ (89,4,REC=590) LWMO,(WMOSTN(I),WMOREC(I),I=1,LWMO)
        ENDIF
*
        DO 110 I=1,LWMO
          IF (WMOID.EQ.WMOSTN(I)) THEN
            IF (POS.EQ.'L') THEN
              NFOUND=NFOUND+1
              INDREC(NFOUND)=WMOREC(I)
              GO TO 900
            ENDIF
          ENDIF
  110   CONTINUE
*
* IF ICAO ID
*
      ELSE IF (IDTYPE.EQ.2) THEN
        IF (NICAO.EQ.0) THEN
          READ (89,5,REC=591) LICAO,(ICAOSTN(I),ICAOREC(I),I=1,LICAO)
        ENDIF
*
        DO 120 I=1,LICAO
          IF (ICAOID.EQ.ICAOSTN(I)) THEN
            IF (POS.EQ.'L') THEN
              NFOUND=NFOUND+1
              INDREC(NFOUND)=ICAOREC(I)
              GO TO 900
            ENDIF
          ENDIF
  120   CONTINUE
*
* IF DCNN ID
*
      ELSE IF (IDTYPE.EQ.3) THEN
        READ (89,6,REC=592) LDCNN,(DCNNSTN(I),DCNNREC(I),I=1,LDCNN)
*
        DO 130 I=1,LDCNN
          IF (DCNNID.EQ.DCNNSTN(I)) THEN
            NFOUND=NFOUND+1
            INDREC(NFOUND)=DCNNREC(I)
            GO TO 900
          ENDIF
  130   CONTINUE
*
* IF RAIN ID
*
      ELSE IF (IDTYPE.EQ.4) THEN
        READ (89,7,REC=593) LRAIN,(RAINSTN(I),RAINREC(I),I=1,LRAIN)
*
        DO 140 I=1,LRAIN
          IF (RAINID.EQ.RAINSTN(I)) THEN
            NFOUND=NFOUND+1
            INDREC(NFOUND)=RAINREC(I)
            GO TO 900
          ENDIF
  140   CONTINUE
      ENDIF
***********************************************************************
*
* IF IDENTIFIER NOT FOUND IN TEMPORARY RECORDS, LOOK IN MAIN INDEX
*
***********************************************************************
*
* IF WMO BLK AND STN
*
***********************************************************************
      IF (IDTYPE.EQ.1) THEN
        IF (WMOID.LE.99) REGION=.TRUE.
*
* READ BLOCK INDEX IF IT'S NOT YET IN CORE.  WMOBLK(N) IS THE RECORD
* WHERE ENTRIES FOR WMO BLOCK N START, NWMOBLK(N) WILL BE THE NUMBER
* OF STATIONS IN BLOCK N.
*
        IF (WMOBLK(1).EQ.0) THEN
          READ (89,3,REC=1) (N,WMOBLK(N),I=1,50)
          READ (89,3,REC=2) (N,WMOBLK(N),I=1,50)
    3     FORMAT (5X,50(I2,1X,I5,1X))
        ENDIF
*
* GET WMO BLOCK NUMBER.  INDEXES FOR WMO BLOCKS WILL BE READ AS NEEDED.
*
        IF (REGION) THEN
          IBLOCK=WMOID
        ELSE
          IBLOCK=WMOID/1000
        ENDIF
        IF (IBLOCK.LT.1 .OR. IBLOCK.GT.99) GOTO 900
        IF (WMOBLK(IBLOCK).EQ.0) GOTO 900
*
* READ IN THE INDEX FOR THIS BLOCK (50 STATIONS/BLOCK) IF NOT IN CORE,
* PUTTING IT IN THE SLOTS IT WOULD FILL IF THE WHOLE INDEX WERE READ.
*
        IF (NWMOBLK(IBLOCK).EQ.0) THEN
          IREC=WMOBLK(IBLOCK)
  210     J=LWMO+(IREC-WMOBLK(1))*50
          READ(89,4,REC=IREC)
     &              N,(WMOSTN(I),WMOREC(I),I=J+1,J+N)
    4     FORMAT(5X,I2,1X,50(I5,1X,I5,1X))
          NWMOBLK(IBLOCK)=NWMOBLK(IBLOCK)+N
          IF (N.EQ.50) THEN
            IREC=IREC+1
            GO TO 210
          ENDIF
          NWMO=NWMO+NWMOBLK(IBLOCK)
        ENDIF
*
* FIND STATION IN INDEX ENTRIES FOR ITS WMO BLOCK.
*
        IF (.NOT.REGION) THEN
          I0=LWMO+(WMOBLK(IBLOCK)-WMOBLK(1))*50
          DO 220 I=I0+1,I0+NWMOBLK(IBLOCK)
            IF (WMOID.EQ.WMOSTN(I)) THEN
              IF (POS.EQ.'L') THEN
                KEPSTN=WMOSTN(I)
                DO 221 K=1,10
                  IF (KEPSTN.NE.WMOSTN(I+K)) THEN
                    NFOUND=NFOUND+1
                    INDREC(NFOUND)=WMOREC(I+K-1)
                    INDWMO(NFOUND)=WMOSTN(I+K-1)
                    GOTO 900
                  ENDIF
  221           CONTINUE
              ELSE
                NFOUND=NFOUND+1
                INDREC(NFOUND)=WMOREC(I)
                INDWMO(NFOUND)=WMOSTN(I)
              ENDIF
            ENDIF
  220     CONTINUE
*
* OR, IF WHOLE WMO BLOCK REQUESTED, RETURN ALL STATIONS.
*
        ELSE IF (REGION) THEN
          I0=LWMO+(WMOBLK(IBLOCK)-WMOBLK(1))*50
          DO 230 I=I0+1,I0+NWMOBLK(IBLOCK)
            IF (WMOID.EQ.WMOSTN(I)/1000) THEN
              IF (POS.EQ.'L') THEN
                IF (KEPSTN.EQ.WMOSTN(I)) GOTO 230
                KEPSTN=WMOSTN(I)
                DO 231 K=1,10
                  IF (KEPSTN.NE.WMOSTN(I+K)) THEN
                    NFOUND=NFOUND+1
                    INDREC(NFOUND)=WMOREC(I+K-1)
                    INDWMO(NFOUND)=WMOSTN(I+K-1)
                    GOTO 230
                  ENDIF
  231           CONTINUE
              ELSE
                NFOUND=NFOUND+1
                INDREC(NFOUND)=WMOREC(I)
                INDWMO(NFOUND)=WMOSTN(I)
              ENDIF
            ENDIF
  230     CONTINUE
        ENDIF
***********************************************************************
*
* IF ICAOID
*
***********************************************************************
      ELSE IF (IDTYPE.EQ.2) THEN
        IF (ICAOID(3:3).EQ.' ') REGION=.TRUE.
*
* FIRST READ IN THE WHOLE INDEX (50 AIRFIELDS/BLOCK) IF NOT IN CORE
*
        IF (NICAO.EQ.0) THEN
          IREC=350                   ! START READING AT BLOCK 350
  310     J=LICAO+(IREC-350)*50
          READ(89,5,REC=IREC)
     &              N,(ICAOSTN(I),ICAOREC(I),I=J+1,J+N)
   5      FORMAT(5X,I2,1X,50(A4,1X,I5,1X))
          NICAO=NICAO+N
          IF (N.EQ.50) THEN
            IREC=IREC+1
            GO TO 310
          ENDIF
        ENDIF
*
* IF FULL AIRFIELD IDENTIFIER (4 LETTERS)
*
        IF (.NOT.REGION) THEN
          IF (ICAOID.LT.'AAAA' .OR. ICAOID.GT.'ZZZZ') GOTO 900
*
          DO 320 I=LICAO+1,LICAO+NICAO
            IF (ICAOID.EQ.ICAOSTN(I)) THEN
              IF (POS.EQ.'L') THEN
                KEPICAO=ICAOSTN(I)
                DO 321 K=1,10
                  IF (KEPICAO.NE.ICAOSTN(I+K)) THEN
                    NFOUND=NFOUND+1
                    INDREC(NFOUND)=ICAOREC(I+K-1)
                    GOTO 900
                  ENDIF
  321           CONTINUE
              ELSE
                NFOUND=NFOUND+1
                INDREC(NFOUND)=ICAOREC(I)
              ENDIF
            ENDIF
  320     CONTINUE
*
* IF ICAO REGION (ONLY FIRST 2 LETTERS)
*
        ELSE IF (REGION) THEN
          IF (ICAOID(1:2).LT.'AA' .OR. ICAOID(1:2).GT.'ZZ') GOTO 900
*
          DO 330 I=LICAO+1,LICAO+NICAO
            IF (ICAOID(1:2).EQ.ICAOSTN(I)(1:2)) THEN
              IF (POS.EQ.'L') THEN
                IF (KEPICAO.EQ.ICAOSTN(I)) GOTO 330
                KEPICAO=ICAOSTN(I)
                DO 331 K=1,10
                  IF(KEPICAO.NE.ICAOSTN(I+K))THEN
                    NFOUND=NFOUND+1
                    INDREC(NFOUND)=ICAOREC(I)
                    GOTO 330
                  ENDIF
  331           CONTINUE
              ELSE
                NFOUND=NFOUND+1
                INDREC(NFOUND)=ICAOREC(I)
              ENDIF
            ENDIF
  330     CONTINUE
        ENDIF
***********************************************************************
*
* IF DCNNID
*
***********************************************************************
      ELSE IF (IDTYPE.EQ.3) THEN
        IF (DCNNID.LE.99) REGION=.TRUE.
*
* FIRST READ IN THE WHOLE INDEX (50 STATIONS/BLOCK) IF NOT IN CORE
*
        IF (NDCNN.EQ.0) THEN
          IREC=500                   ! START READING AT BLOCK 500
  410     J=LDCNN+(IREC-500)*50
          READ(89,6,REC=IREC)
     &              N,(DCNNSTN(I),DCNNREC(I),I=J+1,J+N)
   6       FORMAT(5X,I2,1X,50(I4,1X,I5,1X))
          NDCNN=NDCNN+N
          IF (N.EQ.50) THEN
            IREC=IREC+1
            GO TO 410
          ENDIF
        ENDIF
*
        IF (.NOT.REGION) THEN
          IF (DCNNID.LT.0 .OR. DCNNID.GT.9999) GOTO 900
*
           DO 420 I=LDCNN+1,LDCNN+NDCNN
             IF (DCNNID.EQ.DCNNSTN(I)) THEN
               IF (POS.EQ.'L') THEN
                 KEPDCNN=DCNNSTN(I)
                 DO 421 K=1,10
                   IF(KEPDCNN.NE.DCNNSTN(I+K))THEN
                     NFOUND=NFOUND+1
                     INDREC(NFOUND)=DCNNREC(I+K-1)
                     GOTO 900
                   ENDIF
  421            CONTINUE
               ELSE
                 NFOUND=NFOUND+1
                 INDREC(NFOUND)=DCNNREC(I)
               ENDIF
             ENDIF
  420      CONTINUE
*
         ELSE IF (REGION) THEN
           IF (DCNNID.LT.0 .OR. DCNNID.GT.99) GOTO 900
*
           DO 430 I=LDCNN+1,LDCNN+NDCNN
             IF (DCNNID.EQ.DCNNSTN(I)/100) THEN
               IF (POS.EQ.'L') THEN
                 KEPDCNN=DCNNSTN(I)
                 DO 431 K=1,10
                   IF(KEPDCNN.NE.DCNNSTN(I+K))THEN
                     NFOUND=NFOUND+1
                     INDREC(NFOUND)=DCNNREC(I+K-1)
                     GOTO 430
                   ENDIF
  431            CONTINUE
               ELSE
                 NFOUND=NFOUND+1
                 INDREC(NFOUND)=DCNNREC(I)
               ENDIF
             ENDIF
  430      CONTINUE
         ENDIF
***********************************************************************
*
* IF RAINID
*
***********************************************************************
      ELSE IF (IDTYPE.EQ.4) THEN
        IF (RAINID.LE.99) REGION=.TRUE.
*
* FIRST READ IN THE WHOLE INDEX (50 STATIONS/BLOCK) IF NOT IN CORE
*
        IF (NRAIN.EQ.0) THEN
          IREC=550                   ! START READING AT BLOCK 550
  510     J=LRAIN+(IREC-550)*50
          READ(89,7,REC=IREC)
     &              N,(RAINSTN(I),RAINREC(I),I=J+1,J+N)
   7      FORMAT(5X,I2,1X,50(I6,1X,I5,1X))
          NRAIN=NRAIN+N
          IF (N.EQ.50) THEN
            IREC=IREC+1
            GO TO 510
          ENDIF
        ENDIF
*
        IF (.NOT.REGION) THEN
          IF (RAINID.LT.0 .OR. RAINID.GT.999999) GOTO 900
*
          DO 520 I=LRAIN+1,LRAIN+NRAIN
            IF (RAINID.EQ.RAINSTN(I)) THEN
              IF (POS.EQ.'L') THEN
                KEPRAIN=RAINSTN(I)
                DO 521 K=1,10
                  IF (KEPRAIN.NE.RAINSTN(I+K)) THEN
                    NFOUND=NFOUND+1
                    INDREC(NFOUND)=RAINREC(I+K-1)
                    GOTO 900
                  ENDIF
  521           CONTINUE
              ELSE
                NFOUND=NFOUND+1
                INDREC(NFOUND)=RAINREC(I)
              ENDIF
            ENDIF
  520     CONTINUE
*
        ELSE IF (REGION) THEN
          IF (RAINID.LT.0 .OR. RAINID.GT.99) GOTO 900
*
          DO 530 I=LRAIN+1,LRAIN+NRAIN
            IF (RAINID.EQ.RAINSTN(I)/10000) THEN
              IF (POS.EQ.'L') THEN
                IF (KEPRAIN.EQ.RAINSTN(I)) GOTO 530
                KEPRAIN=RAINSTN(I)
                DO 531 K=1,10
                  IF (KEPRAIN.NE.RAINSTN(I+K)) THEN
                    NFOUND=NFOUND+1
                    INDREC(NFOUND)=RAINREC(I+K-1)
                    GOTO 530
                  ENDIF
  531           CONTINUE
              ELSE
                NFOUND=NFOUND+1
                INDREC(NFOUND)=RAINREC(I)
              ENDIF
            ENDIF
  530     CONTINUE
        ENDIF
      ENDIF
***********************************************************************
*
* SEE IF THERE'S ANOTHER IDENTIFIER.  IF NOT, SET RETURN CODE
* & PUT ZERO IN ARRAY OF RECORD NUMBERS AFTER LAST NUMBER FOUND.
*
***********************************************************************
 900  IF (COUNT.LT.50) GO TO 100
*
 990  IF (NFOUND.EQ.0) THEN
        ISTAT=8
      ELSE IF (NFOUND.EQ.1) THEN
        ISTAT=0
      ELSE IF (NFOUND.GT.1) THEN
        ISTAT=4
      ENDIF
      INDREC(NFOUND+1)=0
*
      RETURN
      END
