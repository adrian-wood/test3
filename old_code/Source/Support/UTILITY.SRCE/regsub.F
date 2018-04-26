*********************************************************************** 00010000
*                                                                     * 00020000
* PROGRAM       : REGSUB   IN SECRET                                  * 00030005
*                                                                     * 00040000
* PURPOSE       : TO RETURN WMO REGION GIVEN WMO BLOCK AND STATION    * 00050000
*                 NUMBER.                                             * 00060000
*                                                                     * 00070000
* DESCRIPTION   :                                                     * 00080000
*                                                                     * 00090000
*                                                                     * 00100000
* CALLED BY     : ........ IN YLOAD.                                  * 00110000
*                 STNADMIN                                            * 00120011
*                                                                     * 00121011
* CALLS         : NONE                                                * 00130000
*                                                                     * 00140000
* PARAMETERS    : (1)  WMO BLOCK                                      * 00150000
*                 (2)  WMO STATION NUMBER                             * 00160000
*                 (3)  WMO REGION                                     * 00170000
*                           ANTARCTICA = REGION 0                     * 00180002
*                           TOO HIGH   = -32768                       * 00181002
*                           TOO LOW    = -32768                       * 00182002
*                                                                     * 00183002
*Y2K  16.06.1997  REGSUB IS YEAR 2000 COMPLIANT.                        00183112
*                                                                     * 00184012
* CHANGE RECORD :                                                     * 00190000
*   DATE :- FEB 92 PURPOSE:- ROUTINE ADAPTED TO ENABLE IT TO BE CALLED* 00200010
*                            BY STNADMIN                              * 00210010
*                                                                     * 00211010
* OCT 89         WRITTEN BY JOHN NORTON                               * 00220000
*                                                                     * 00230000
*********************************************************************** 00240000
*234567                                                                 00250000
      SUBROUTINE REGSUB(BLOCK,STNNUM,WMOREG)                            00260009
      INTEGER   BLOCK,STNNUM,WMOREG                                     00270006
*                                                                       00280000
      ISTN=BLOCK*1000+STNNUM                                            00290000
*                                                                       00300000
      IF(ISTN.LT.00000)THEN                                             00310001
         WMOREG = -32768                                                00310102
      ELSEIF(ISTN.LT.20000)THEN                                         00311001
         WMOREG = 6                                                     00320000
      ELSEIF(ISTN.LT.33000)THEN                                         00340101
        IF(ISTN.LT.20100)THEN                                           00341001
           WMOREG = 2                                                   00350001
        ELSEIF(ISTN.LT.20200)THEN                                       00370001
           WMOREG = 6                                                   00380001
        ELSEIF(ISTN.LT.22000)THEN                                       00400001
           WMOREG = 2                                                   00410001
        ELSEIF(ISTN.LT.23000)THEN                                       00430001
           WMOREG = 6                                                   00440001
        ELSEIF(ISTN.LT.26000)THEN                                       00460001
           WMOREG = 2                                                   00470001
        ELSEIF(ISTN.LT.28000)THEN                                       00490001
           WMOREG = 6                                                   00500001
        ELSE                                                            00520001
           WMOREG = 2                                                   00530001
        ENDIF                                                           00540101
      ELSEIF(ISTN.LT.60000)THEN                                         00541001
        IF(ISTN.LT.35000)THEN                                           00550003
           WMOREG = 6                                                   00560001
        ELSEIF(ISTN.LT.37000)THEN                                       00561003
           WMOREG = 2                                                   00562003
        ELSEIF(ISTN.LT.38000)THEN                                       00570003
           WMOREG = 6                                                   00571003
        ELSEIF(ISTN.LT.40000)THEN                                       00580003
           WMOREG = 2                                                   00590001
        ELSEIF(ISTN.LT.40350)THEN                                       00600003
           WMOREG = 6                                                   00601003
        ELSEIF(ISTN.LT.48600)THEN                                       00610003
           WMOREG = 2                                                   00620003
        ELSEIF(ISTN.LT.48800)THEN                                       00630003
           WMOREG = 5                                                   00630103
        ELSE                                                            00630201
           WMOREG = 2                                                   00630301
        ENDIF                                                           00631001
      ELSEIF(ISTN.LT.70000)THEN                                         00670000
         WMOREG = 1                                                     00680000
      ELSEIF(ISTN.LT.80000)THEN                                         00700000
         WMOREG = 4                                                     00710000
      ELSEIF(ISTN.LT.89000)THEN                                         00730000
        IF(ISTN.EQ.88963.OR.ISTN.EQ.88968)THEN                          00731001
           WMOREG = 0                                                   00732002
        ELSE                                                            00734001
           WMOREG = 3                                                   00735001
        ENDIF                                                           00737001
      ELSEIF(ISTN.LT.90000)THEN                                         00760000
         WMOREG = 0                                                     00770002
      ELSEIF(ISTN.LT.99000)THEN                                         00790000
         WMOREG = 5                                                     00800000
      ELSE                                                              00810001
         WMOREG = -32768                                                00811002
      ENDIF                                                             00820000
*                                                                       00830000
      RETURN                                                            00860000
      END                                                               00870000
