*********************************************************************** 00010099
*                                                                     * 00020099
* SUBROUTINE    : STNFRPAN                                            * 00030099
*                                                                     * 00040099
* PURPOSE       : TO COPY PANEL VARIABLES TO FORTRAN PGM VARIABLES    * 00050099
*                                                                     * 00060099
* CALLED BY     : STNDISP (FORTRAN)                                   * 00070099
*                 STNADMIN                                            * 00080099
*                                                                     * 00090099
* PARAMETERS    : NONE (COOMON BLOCK)                                 * 00100099
*                                                                     * 00110099
* CHANGE RECORD :                                                     * 00120099
*                                                                     * 00130099
*   DATE : JUL 92  PURPOSE: FIRST VERSION OF SOFTWARE                 * 00140099
*          FEB 93         : TO CORRECT AN ERROR IN THE 5 DEGREE SQUARE* 00150099
*                           CALCULATION                               * 00160099
*                                                                     * 00170099
*********************************************************************** 00180099
*234567                                                                 00190099
      SUBROUTINE STNFRPAN                                               00200099
*                                                                       00210099
* INCLUDE ALL THE VARIABLE STATEMENTS                                   00220099
*                                                                       00230099
      REAL LATTEMP, LONGTEMP                                            00240099
      INTEGER LAT,LONG,SQ1,SQ2                                          00250099
      INCLUDE(STNINIT)                                                  00260099
*                                                                       00270099
      DAY(1)=DAY1                                                       00280099
      DAY(2)=DAY2                                                       00290099
      DAY(3)=DAY3                                                       00300099
      DAY(4)=DAY4                                                       00310099
      DAY(5)=DAY5                                                       00320099
      DAY(6)=DAY6                                                       00330099
      DAY(7)=DAY7                                                       00340099
      DAY(8)=DAY8                                                       00350099
      DAY(9)=DAY9                                                       00360099
      DAY(10)=DAY10                                                     00370099
      DO 44 IJ=1,10                                                     00380099
        DO 94 JJ=1,9                                                    00390099
          IF(DAY(IJ)(JJ:JJ).LT.'A')THEN                                 00400099
            DAY(IJ)(JJ:JJ)='-'                                          00410099
          ENDIF                                                         00420099
  94    CONTINUE                                                        00430099
  44  CONTINUE                                                          00440099
*                                                                       00450099
      DO 64 IJK=1,13                                                    00460099
        IF(SYN(IJK:IJK).EQ.'Y')THEN                                     00470099
           SYN(IJK:IJK)='X'                                             00480099
        ELSE                                                            00490099
           SYN(IJK:IJK)='-'                                             00500099
        ENDIF                                                           00510099
   64 CONTINUE                                                          00520099
      DO 63 IJK=1,23                                                    00530099
        IF(NCM(IJK:IJK).EQ.'Y')THEN                                     00540099
           NCM(IJK:IJK)='X'                                             00550099
        ELSE                                                            00560099
           NCM(IJK:IJK)='-'                                             00570099
        ENDIF                                                           00580099
   63 CONTINUE                                                          00590099
      SYNTIM(1)=SYNTIM1                                                 00600099
      SYNTIM(2)=SYNTIM2                                                 00610099
      SYNTIM(3)=SYNTIM3                                                 00620099
      SYNTIM(4)=SYNTIM4                                                 00630099
      SYNTIM(5)=SYNTIM5                                                 00640099
      SYNTIM(6)=SYNTIM6                                                 00650099
      SYNTIM(7)=SYNTIM7                                                 00660099
      SYNTIM(8)=SYNTIM8                                                 00670099
      SYNTIM(9)=SYNTIM9                                                 00680099
      SYNTIM(10)=SYNTIM10                                               00690099
      DO 54 II=1,10                                                     00700099
        DO 74 JJ=1,24                                                   00710099
          IF(SYNTIM(II)(JJ:JJ).LT.'0')THEN                              00720099
             SYNTIM(II)(JJ:JJ)='-'                                      00730099
          ENDIF                                                         00740099
   74   CONTINUE                                                        00750099
   54 CONTINUE                                                          00760099
*                                                                       00770099
      IF(NCMTIM1A.EQ.'YES')THEN                                         00780099
        NCMTIM1(1)=9                                                    00790099
      ELSE                                                              00800099
        NCMTIM1(1)=0                                                    00810099
      ENDIF                                                             00820099
      IF(NCMTIM1B.EQ.'YES')THEN                                         00830099
        NCMTIM1(2)=9                                                    00840099
      ELSE                                                              00850099
        NCMTIM1(2)=0                                                    00860099
      ENDIF                                                             00870099
      IF(NCMTIM1C.EQ.'YES')THEN                                         00880099
        NCMTIM1(3)=9                                                    00890099
      ELSE                                                              00900099
        NCMTIM1(3)=0                                                    00910099
      ENDIF                                                             00920099
      IF(NCMTIM1D.EQ.'YES')THEN                                         00930099
        NCMTIM1(4)=9                                                    00940099
      ELSE                                                              00950099
        NCMTIM1(4)=0                                                    00960099
      ENDIF                                                             00970099
      IF(NCMTIM1E.EQ.'YES')THEN                                         00980099
        NCMTIM1(5)=9                                                    00990099
      ELSE                                                              01000099
        NCMTIM1(5)=0                                                    01010099
      ENDIF                                                             01020099
      IF(NCMTIM1F.EQ.'YES')THEN                                         01030099
        NCMTIM1(6)=9                                                    01040099
      ELSE                                                              01050099
        NCMTIM1(6)=0                                                    01060099
      ENDIF                                                             01070099
      IF(NCMTIM1G.EQ.'YES')THEN                                         01080099
        NCMTIM1(7)=9                                                    01090099
      ELSE                                                              01100099
        NCMTIM1(7)=0                                                    01110099
      ENDIF                                                             01120099
      IF(NCMTIM1H.EQ.'YES')THEN                                         01130099
        NCMTIM1(8)=9                                                    01140099
      ELSE                                                              01150099
        NCMTIM1(8)=0                                                    01160099
      ENDIF                                                             01170099
      IF(NCMTIM1I.EQ.'YES')THEN                                         01180099
        NCMTIM1(9)=9                                                    01190099
      ELSE                                                              01200099
        NCMTIM1(9)=0                                                    01210099
      ENDIF                                                             01220099
      IF(NCMTIM1J.EQ.'YES')THEN                                         01230099
        NCMTIM1(10)=9                                                   01240099
      ELSE                                                              01250099
        NCMTIM1(10)=0                                                   01260099
      ENDIF                                                             01270099
*                                                                       01280099
      IF(NCMTIM2A.EQ.'YES')THEN                                         01290099
        NCMTIM2(1)=21                                                   01300099
      ELSE                                                              01310099
        NCMTIM2(1)=0                                                    01320099
      ENDIF                                                             01330099
      IF(NCMTIM2B.EQ.'YES')THEN                                         01340099
        NCMTIM2(2)=21                                                   01350099
      ELSE                                                              01360099
        NCMTIM2(2)=0                                                    01370099
      ENDIF                                                             01380099
      IF(NCMTIM2C.EQ.'YES')THEN                                         01390099
        NCMTIM2(3)=21                                                   01400099
      ELSE                                                              01410099
        NCMTIM2(3)=0                                                    01420099
      ENDIF                                                             01430099
      IF(NCMTIM2D.EQ.'YES')THEN                                         01440099
        NCMTIM2(4)=21                                                   01450099
      ELSE                                                              01460099
        NCMTIM2(4)=0                                                    01470099
      ENDIF                                                             01480099
      IF(NCMTIM2E.EQ.'YES')THEN                                         01490099
        NCMTIM2(5)=21                                                   01500099
      ELSE                                                              01510099
        NCMTIM2(5)=0                                                    01520099
      ENDIF                                                             01530099
      IF(NCMTIM2F.EQ.'YES')THEN                                         01540099
        NCMTIM2(6)=21                                                   01550099
      ELSE                                                              01560099
        NCMTIM2(6)=0                                                    01570099
      ENDIF                                                             01580099
      IF(NCMTIM2G.EQ.'YES')THEN                                         01590099
        NCMTIM2(7)=21                                                   01600099
      ELSE                                                              01610099
        NCMTIM2(7)=0                                                    01620099
      ENDIF                                                             01630099
      IF(NCMTIM2H.EQ.'YES')THEN                                         01640099
        NCMTIM2(8)=21                                                   01650099
      ELSE                                                              01660099
        NCMTIM2(8)=0                                                    01670099
      ENDIF                                                             01680099
      IF(NCMTIM2I.EQ.'YES')THEN                                         01690099
        NCMTIM2(9)=21                                                   01700099
      ELSE                                                              01710099
        NCMTIM2(9)=0                                                    01720099
      ENDIF                                                             01730099
      IF(NCMTIM2J.EQ.'YES')THEN                                         01740099
        NCMTIM2(10)=21                                                  01750099
      ELSE                                                              01760099
        NCMTIM2(10)=0                                                   01770099
      ENDIF                                                             01780099
      FLAGS(1:1)=F1                                                     01790099
      FLAGS(2:2)=F2                                                     01800099
      FLAGS(3:3)=F3                                                     01810099
      FLAGS(4:4)=F4                                                     01820099
      FLAGS(5:5)=F5                                                     01830099
      FLAGS(6:6)=F6                                                     01840099
      FLAGS(7:7)=F7                                                     01850099
      FLAGS(8:8)=F8                                                     01860099
      FLAGS(9:9)=F9                                                     01870099
      FLAGS(10:10)=F10                                                  01880099
      FLAGS(11:11)=F11                                                  01890099
      FLAGS(12:12)=F12                                                  01900099
      FLAGS(13:13)=F13                                                  01910099
      FLAGS(14:14)=F14                                                  01920099
      FLAGS(15:15)=F15                                                  01930099
      FLAGS(16:16)=F16                                                  01940099
      FLAGS(17:17)=F17                                                  01950099
      FLAGS(18:18)=F18                                                  01960099
      FLAGS(19:19)=F19                                                  01970099
      FLAGS(20:20)=F20                                                  01980099
      FLAGS(21:21)=F21                                                  01990099
      FLAGS(22:22)=F22                                                  02000099
      FLAGS(23:23)=F23                                                  02010099
      FLAGS(24:24)=F24                                                  02020099
*                                                                       02030099
      DO 34 IJ=1,24                                                     02040099
        IF(FLAGS(IJ:IJ).LT.'1')THEN                                     02050099
          FLAGS(IJ:IJ)='0'                                              02060099
        ENDIF                                                           02070099
  34  CONTINUE                                                          02080099
*                                                                       02090078
*******************************************************************     02100099
* 5 DEGREE SQUARE CALCULATION                                           02110099
*                                                                       02120078
      IF((LAT1.NE.99).AND.(LONG1.NE.999))THEN                           02130099
*                                                                       02140099
*******************************************************************     02150099
* CONVERT TO DEGREES AND 10THS                                          02160099
*******************************************************************     02170099
*                                                                       02180099
*                                                                       02190099
        LATTEMP=(LAT2/6)                                                02200099
        LONGTEMP=(LONG2/6)                                              02210099
        LAT5=(LAT1*10)+LATTEMP                                          02220099
        LONG5=(LONG1*10)+LONGTEMP                                       02230099
*                                                                       02240099
*******************************************************************     02250099
* LATTITUDE IS POSITIVE NORTH                                           02260099
* LONGNITUDE IS POSITVE WEST                                            02270099
*******************************************************************     02280099
*                                                                       02290099
*                                                                       02300099
        IF(LATP.EQ.'S')THEN                                             02310099
          LAT5=LAT5*(-1)                                                02320099
        ENDIF                                                           02330099
        IF(LONGP.EQ.'E')THEN                                            02340099
          LONG5=LONG5*(-1)                                              02350099
        ENDIF                                                           02360099
*                                                                       02370099
*******************************************************************     02380099
* APPLY 5 DEGREE SQUARE ALGORITHM                                       02390099
*******************************************************************     02400099
*                                                                       02410099
*                                                                       02420099
        SQ1=900-LAT5-1                                                  02430099
        SQ1=SQ1/50                                                      02440099
        SQ1=SQ1*72                                                      02450099
        SQ2=1800-LONG5-1                                                02460099
        SQ2=SQ2/50                                                      02470099
        SQUARE=SQ1+SQ2                                                  02480099
      ELSE                                                              02490099
        SQUARE=9999                                                     02500099
      ENDIF                                                             02510099
*                                                                       02520099
*                                                                       02530099
*******************************************************************     02540099
*                                                                       02550099
      BLK=BLKSTN/1000                                                   02560099
      STN=BLKSTN-BLK*1000                                               02570099
*                                                                       02580099
      IF(COYEAR.EQ.'--')THEN                                            02590099
        OYEAR=99                                                        02600099
      ELSE                                                              02610099
        READ(COYEAR,'(I2.2)')OYEAR                                      02620099
      ENDIF                                                             02630099
*                                                                       02640099
      IF(COMONTH.EQ.'--')THEN                                           02650099
        OMONTH=99                                                       02660099
      ELSE                                                              02670099
        READ(COMONTH,'(I2.2)')OMONTH                                    02680099
      ENDIF                                                             02690099
*                                                                       02700099
      IF(CODAY.EQ.'--')THEN                                             02710099
        ODAY=99                                                         02720099
      ELSE                                                              02730099
        READ(CODAY,'(I2.2)')ODAY                                        02740099
      ENDIF                                                             02750099
*                                                                       02760099
      IF(COHOUR.EQ.'--')THEN                                            02770099
        OHOUR=99                                                        02780099
      ELSE                                                              02790099
        READ(COHOUR,'(I2.2)')OHOUR                                      02800099
      ENDIF                                                             02810099
*                                                                       02820099
      IF(CCYEAR.EQ.'--')THEN                                            02830099
        CYEAR=99                                                        02840099
      ELSE                                                              02850099
        READ(CCYEAR,'(I2.2)')CYEAR                                      02860099
      ENDIF                                                             02870099
*                                                                       02880099
      IF(CCMONTH.EQ.'--')THEN                                           02890099
        CMONTH=99                                                       02900099
      ELSE                                                              02910099
        READ(CCMONTH,'(I2.2)')CMONTH                                    02920099
      ENDIF                                                             02930099
*                                                                       02940099
      IF(CCDAY.EQ.'--')THEN                                             02950099
        CDAY=99                                                         02960099
      ELSE                                                              02970099
        READ(CCDAY,'(I2.2)')CDAY                                        02980099
      ENDIF                                                             02990099
*                                                                       03000099
      IF(CCHOUR.EQ.'--')THEN                                            03010099
        CHOUR=99                                                        03020099
      ELSE                                                              03030099
        READ(CCHOUR,'(I2.2)')CHOUR                                      03040099
      ENDIF                                                             03050099
*                                                                       03060099
      RETURN                                                            03070099
      END                                                               03080099
