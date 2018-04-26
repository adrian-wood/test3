*********************************************************************** 00010000
*                                                                     * 00020000
* PROGRAM       : STNRITE                                             * 00030009
*                                                                     * 00040000
* PURPOSE       : TO WRITE A RECORD TO THE MAIN STATIONMASTER D/S     * 00050006
*                                                                     * 00065000
* CALLED BY     : STNUPDT                                             * 00066008
*                                                                     * 00067000
* CALLS         : NOTHING                                             * 00068000
*                                                                     * 00069000
* PARAMETERS    : (1) RECORD NUMBER                                   * 00069100
*                 (2- 46) STATION DESCRIPTION WRITTEN                 * 00069203
*                                                                     * 00069400
*Y2K  16.06.1997  STNRITE IS YEAR 2000 COMPLIANT.                       00069512
*                                                                     * 00069612
* CHANGE RECORD :                                                     * 00069712
*   DATE :-        PURPOSE:-                                          * 00069812
*  10/6/93      ROUTINE FIRST WRITTEN                                 * 00069912
*                                                                     * 00070012
*********************************************************************** 00071012
*                                                                       00080012
      SUBROUTINE STNRITE (MAINREC,BLK,STN,ICAO,LAT1,LAT2,LATP,          00091009
     *  LONG1,LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,     00100000
     *  COYEAR,COMONTH,CODAY,COHOUR,CCYEAR,CCMONTH,CCDAY,CCHOUR,        00110013
     *  FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,                00120000
     *  UA,SYN,NCM,NUM,                                                 00130000
     *  DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,RECNO)                 00140000
*                                                                       00150000
      INTEGER MAINREC                                                   00170008
*                                                                       00200000
      INTEGER BLK,STN,DCNN,RAIN,REGION,LAT1,LAT2                        00210000
      INTEGER LONG1,LONG2,HT1,HT2,CLASS,HAW00,HAW12                     00220000
      INTEGER MAP,RUN1,RUN2,OLDB,OREC,NREC                              00240000
      INTEGER NCMTIM1(10),NCMTIM2(10),RECNO,SQUARE                      00250000
      CHARACTER*4 ICAO,UA                                               00260000
      CHARACTER*6 CHASER                                                00270000
      CHARACTER*24 FLAGS                                                00280000
      CHARACTER*24 SYNTIM(10)                                           00290000
      CHARACTER*13 SYN                                                  00300000
      CHARACTER*23 NCM                                                  00310000
      CHARACTER*9 DAY(10)                                               00320000
      CHARACTER*42 STATION                                              00330000
      CHARACTER*32 COUNTRY                                              00340000
      CHARACTER*1 LATP,LONGP                                            00350000
      CHARACTER*2 COYEAR,COMONTH,CODAY,COHOUR                           00360013
      CHARACTER*2 CCYEAR,CCMONTH,CCDAY,CCHOUR                           00370013
*                                                                       00821000
*                                                                       00821200
*     WRITING TO STATION MASTER DATASET                                 00822004
*                                                                       00824100
      WRITE (88,15,REC=MAINREC)BLK,STN,ICAO,LAT1,LAT2,LATP,             00824408
     *  LONG1,LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,     00825004
     *  COYEAR,COMONTH,CODAY,COHOUR,CCYEAR,CCMONTH,CCDAY,CCHOUR,        00826013
     *  FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,                00827000
     *  UA,SYN,NCM,NUM,                                                 00828000
     *  (DAY(K),SYNTIM(K),NCMTIM1(K),NCMTIM2(K),K=1,10),                00829000
     *  SQUARE,CHASER,RECNO                                             00829100
*                                                                       00829200
  15  FORMAT(1X,I2.2,I3.3,1X,A4,1X,I2.2,I2.2,A1,1X,I3.3,I2.2,A1,        00829300
     *  1X,I6,1X,I6,1X,A42,1X,A32,1X,I2,1X,I4.4,1X,I6,                  00829400
     *  1X,A2,A2,A2,A2,1X,A2,A2,A2,A2,                                  00829513
     *  1X,A24,1X,I2,1X,I5,1X,I5,1X,I3,1X,I5.5,1X,I5.5,                 00829600
     *  1X,I5.5,1X,I5.5,1X,A4,1X,A13,1X,A23,1X,I2,                      00829700
     *  1X,10(A9,1X,A24,1X,I2,1X,I2,1X),I4,1X,A6,1X,I5)                 00829800
*                                                                       00832100
      RETURN                                                            01420000
      END                                                               01480000
