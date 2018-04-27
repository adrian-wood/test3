**********************************************************              00010026
*                                                        *              00020026
*  SUBROUTINE WRITINDX                                   *              00030099
*                                                        *              00040083
*  TO WRITE DATA FROM STATION MASTER                     *              00050099
*                                                        *              00060026
*Y2K  16.06.1997  WRITINDX IS YEAR 2000 COMPLIANT.                      00060199
*                                                        *              00061099
**********************************************************              00070026
*                                                                       00080083
      SUBROUTINE WRITINDX                                               00090099
*                                                                       00090499
      INCLUDE(STNINIT)                                                  00090599
      INTEGER RECID(4000),STNINTID(4000)                                00090699
      CHARACTER*4 STNCHRID(4000)                                        00090799
*                                                                       01790099
* IF WMO BLK AND STN                                                    01800099
*                                                                       01810099
      IF(IDTYPE.EQ.1)THEN                                               01820099
*                                                                       01830099
        IREC=590                                                        01980099
*                                                                       01990099
        READ(85,3,REC=IREC)ITOTAL,((STNINTID(II),RECID(II),II=1,50)     02000099
   3    FORMAT(5X,I2,1X,50(I5,1X,I5,1X))                                02010099
*                                                                       02020099
        DO 20 ILOOP=1,50                                                02030099
          IF(STNINTID(ILOOP).EQ.0)THEN                                  02040099
            GOTO 10                                                     02050099
          ENDIF                                                         02060099
   20   CONTINUE                                                        02110099
*                                                                       02120099
        PRINT*,'NO ROOM IN TEMPORARY AREA OF INDEX'                     02121099
        GOTO 999                                                        02122099
*                                                                       02130099
   10   STNINTID(ILOOP)=BLKSTN                                          02140099
        RECID(ILOOP)=END                                                02150099
        WRITE(85,3,REC=IREC)ITOTAL,(STNINTID(II),RECID(II),II=1,50)     02560099
*                                                                       02580099
* IF ICAOID                                                             03050099
*                                                                       03060099
      ELSEIF(IDTYPE.EQ.2)THEN                                           03070099
*                                                                       03080099
        IREC=591                                                        03180099
*                                                                       03190099
        READ(85,5,REC=IREC)ITOTAL,(STNCHRID(II),RECID(II),II=1,50)      03210099
    5   FORMAT(5X,I2,1X,50(A4,1X,I5,1X))                                03220099
*                                                                       03220199
        DO 30 ILOOP=1,50                                                03221099
          IF(STNCHRID(ILOOP).EQ.'    ')THEN                             03222099
            GOTO 11                                                     03223099
          ENDIF                                                         03224099
   30   CONTINUE                                                        03225099
*                                                                       03226099
        PRINT*,'NO ROOM IN TEMPORARY AREA OF INDEX'                     03227099
        GOTO 999                                                        03228099
*                                                                       03228199
   11   STNCHRID(ILOOP)=ICAO                                            03228299
        RECID(ILOOP)=END                                                03228399
        WRITE(85,5,REC=IREC)ITOTAL,(STNCHRID(II),RECID(II),II=1,50)     03228499
*                                                                       03930099
* IF DCNNID                                                             03940099
*                                                                       03950099
      ELSEIF(IDTYPE.EQ.3)THEN                                           03960099
*                                                                       03970099
        IREC=592                                                        04070099
*                                                                       04080099
        READ(85,6,REC=IREC)ITOTAL,(STNINTID(II),RECID(II),II=1,50)      04100099
   6    FORMAT(5X,I2,1X,50(I4,1X,I5,1X))                                04110099
*                                                                       04110199
        DO 40 ILOOP=1,50                                                04111099
          IF(STNINTID(ILOOP).EQ.0)THEN                                  04112099
            GOTO 12                                                     04113099
          ENDIF                                                         04114099
   40   CONTINUE                                                        04115099
*                                                                       04116099
        PRINT*,'NO ROOM IN TEMPORARY AREA OF INDEX'                     04117099
        GOTO 999                                                        04118099
*                                                                       04118199
   12   STNINTID(ILOOP)=DCNN                                            04118299
        RECID(ILOOP)=END                                                04118399
        WRITE(85,6,REC=IREC)ITOTAL,(STNINTID(II),RECID(II),II=1,50)     04118499
*                                                                       04119099
* IF RAINID                                                             04830099
*                                                                       04840099
      ELSEIF(IDTYPE.EQ.4)THEN                                           04850099
*                                                                       04860099
        IREC=593                                                        04960099
*                                                                       04970099
        READ(85,7,REC=IREC)ITOTAL,(STNINTID(II),RECID(II),II=1,50)      04990099
   7    FORMAT(5X,I2,1X,50(I6,1X,I5,1X))                                05000099
*                                                                       05000199
        DO 60 ILOOP=1,50                                                05001099
          IF(STNINTID(ILOOP).EQ.0)THEN                                  05002099
            GOTO 13                                                     05003099
          ENDIF                                                         05004099
   60   CONTINUE                                                        05005099
*                                                                       05006099
        PRINT*,'NO ROOM IN TEMPORARY AREA OF INDEX'                     05007099
        GOTO 999                                                        05008099
*                                                                       05009099
   13   STNINTID(ILOOP)=RAIN                                            05009199
        RECID(ILOOP)=END                                                05009299
        WRITE(85,7,REC=IREC)ITOTAL,(STNINTID(II),RECID(II),II=1,50)     05009399
*                                                                       05010099
      ENDIF                                                             05011099
*                                                                       05020099
 999  RETURN                                                            05810099
*                                                                       05820099
      END                                                               05830099
