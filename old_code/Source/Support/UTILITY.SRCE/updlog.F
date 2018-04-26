**********************************************************              00010026
*                                                        *              00020026
*  SUBROUTINE UPDLOG                                     *              00030099
*                                                        *              00040083
*  TO UPDATE LOG DATASET WHEN THERE HAS BEEN A CHANGE    *              00050099
*                                                        *              00060026
*Y2K  16.06.1997  UPDLOG IS YEAR 2000 COMPLIANT.                        00060199
*                                                        *              00061099
**********************************************************              00070026
*                                                                       00080083
      SUBROUTINE UPDLOG                                                 00090099
*                                                                       00090699
      INTEGER DIAFLAG                                                   00090799
      CHARACTER*26 ATEXT                                                00090899
      INCLUDE(STNINIT)                                                  00090999
*                                                                       00091099
* READ FIRST LOG RECORD                                                 00091199
*                                                                       00091299
      READ(13,26,REC=1)ATEXT,DIAFLAG,LOGREC                             00092099
   26 FORMAT(A26,I1,1X,I5)                                              00093099
*                                                                       00824899
* WRITE TO LOG DATASET                                                  00824999
*                                                                       00825099
      WRITE(13,28,REC=LOGREC)RECNO,SOURCE,AUSER,IYEAR,IMONTH,IDAY,IHOUR 00833599
   28 FORMAT(1X,I5,1X,A20,1X,A20,1X,I2,1X,I2,1X,I2,1X,I2)               00833699
*                                                                       00834099
      LOGREC=LOGREC+1                                                   00834299
      WRITE(13,26,REC=1)ATEXT,DIAFLAG,LOGREC                            00834399
*                                                                       00835099
      RETURN                                                            01420090
      END                                                               01480000
