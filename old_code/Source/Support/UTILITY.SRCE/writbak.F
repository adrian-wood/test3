**********************************************************              00010026
*                                                        *              00020026
*  SUBROUTINE WRITBAK                                    *              00030099
*                                                        *              00040083
*  TO WRITE DATA FROM STATION MASTER                     *              00050099
*                                                        *              00060026
**********************************************************              00070026
!$Log:
! 1    Met_DB_Project 1.0         28/02/2006 12:07:58    Sheila Needham  
!$
!Revision 1.2  1999/06/18 09:38:55  usmdb
!Fix for 1999 problem
!
!$Revision: 1$
!$Date: 28/02/2006 12:07:58$                                                              
!
!
*                                                                       00080083
      SUBROUTINE WRITBAK                                                00090099
      INTEGER INDREC2                                                   00090199
      INTEGER AYEAR,ISUM,IWIN,HDAY(13),DATE,END,IZERO                   00090299
      CHARACTER*3 HCONT(13)                                             00090399
*                                                                       00090499
      INCLUDE(STNINIT)                                                  00090599
*                                                                       00821099
* WRITING BACK TO STATION MASTER DATASET                                00822099
*                                                                       00824199
      INDREC2=INDREC(1)                                                 00824299
*                                                                       00824499
      WRITE(88,15,REC=INDREC2)BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,LONG2,  00824599
     *  LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,                 00825099
     *  COYEAR,COMONTH,CODAY,COHOUR,CCYEAR,CCMONTH,CCDAY,CCHOUR,    !1.200826199
     *  FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,                00827099
     *  UA,SYN,NCM,NUM,                                                 00828099
     *  (DAY(K),SYNTIM(K),NCMTIM1(K),NCMTIM2(K),K=1,10),                00829099
     *  SQUARE,CHASER,RECNO                                             00829199
*                                                                       00829299
  15  FORMAT(1X,I2.2,I3.3,1X,A4,1X,I2.2,I2.2,A1,1X,I3.3,I2.2,A1,        00829399
     *  1X,I6,1X,I6,1X,A42,1X,A32,1X,I2,1X,I4.4,1X,I6,                  00829499
     *  1X,A2,A2,A2,A2,1X,A2,A2,A2,A2,                              !1.200829699
     *  1X,A24,1X,I2,1X,I5,1X,I5,1X,I3,1X,I5.5,1X,I5.5,                 00829799
     *  1X,I5.5,1X,I5.5,1X,A4,1X,A13,1X,A23,1X,I2,                      00829899
     *  1X,10(A9,1X,A24,1X,I2,1X,I2,1X),I4,1X,A6,1X,I5)                 00829999
*                                                                       00832199
* WRITE UPDATE DATE BACK TO DATASET                                     00832299
*                                                                       00832399
      READ(88,28,REC=2)IZERO,END,DATE,AYEAR,ISUM,IWIN,                  00832999
     * (HDAY(II),HCONT(II),II=1,13)                                     00833099
  28  FORMAT(1X,I5.5,6X,I5,1X,I6,1X,I4,1X,I3,1X,I3,1X,13(I3,1X,A3,1X))  00833199
*                                                                       00833299
      DATE=IYEAR*10000+IMONTH*100+IDAY                                  00833399
*                                                                       00833499
      IZERO=0                                                           00833599
*                                                                       00833899
      WRITE(88,28,REC=2)IZERO,END,DATE,AYEAR,ISUM,IWIN,                 00833999
     * (HDAY(II),HCONT(II),II=1,13)                                     00834099
*                                                                       00835099
      RETURN                                                            01420090
      END                                                               01480000
