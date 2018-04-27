BULLEDIT CSECT                                                          00008100
***********************************************************************
*                                                                     *
* PROGRAM       : BULLEDIT IN YLOAD00                                 *
*                                                                     *
* PURPOSE       : REMOVES NON-ESSENTIAL TELECOMMUNICATIONS CHARACTERS *
*                 FORM BULLETIN.                                      *
*                                                                     *
* DESCRIPTION   :                                                     *
*                                                                     *
*                                                                     *
* CALLED BY     : AEACCES  IN YLOAD00                                 *
*                                                                     *
* CALLS         : NOTHING                                             *
*                                                                     *
* PARAMETERS    : (1)                                                 *
*                 (2)                                                 *
*                 (3)                                                 *
*                                                                     *
* CHANGE RECORD : DATE 1979     PURPOSE : SPLIT OFF FROM AEACCES      *
*               : DATE 29/3/85  PURPOSE : COMMENTS                    *
*$Revision: 1$
*$Date: 30/01/2006 20:21:33$
*                                                                     *
***********************************************************************
         PRINT NOGEN                                                    00008200
         CHAIN ,,*                                                      00008300
         LR    9,1            R1 PARAM LIST ADRS INTO R9                00008400
         L     3,0(1)         BULLEN ADRS INTO R3                       00008500
         LH    3,0(3)         ACTUAL LEN INTO R3                        00008550
         LR    4,3            SAVE IN R4                                00008600
         L     2,4(1)         BULL ADRS INTO R2                         00008700
NXTBYTE  CLI   0(2),X'36'     FIG SHIFT ?                               00008800
         BE    SHIFTUP        YES - BR TO SHIFTUP                       00008900
         CLI   0(2),X'37'     NO - LETT SHIFT ?                         00009000
         BE    SHIFTUP        YES - BR TO SHIFTUP                       00009100
         CLC   0(2,2),=X'1515' NO - CR + CR ?                           00009200
         BE    CRANDLF        YES - BR TO CRANDLF                       00009300
         CLI   CRFLAG,X'33'   CRFLAG SET FOR LINE FEED SEARCH ?         00009400
         BNE   ADD2           NO - BR TO ADD2                           00009500
         CLI   0(2),X'25'     YES - NXT CHAR L/F ?                      00009600
         BE    SHIFTUP        YES - BR TO SHIFTUP                       00009700
         MVI   CRFLAG,X'00'   NO - SET CRFLAG 'OFF'                     00009800
ADD2     AH    2,=H'1'        POINT R2 TO NXT BYTE                      00009900
         BCT   3,NXTBYTE      BR TO NXTBYTE IF R3 COUNT ALLOWS          00010000
         B     EDITFIN        BR TO EDITFIN                             00010100
CRANDLF  CLC   1(2,2),=X'1525' CR + LF ?                                00010200
         BNE   ADD2           NO - BR TO ADD2                           00010300
         MVI   CRFLAG,X'FF'   CRFLAG SET TO 'FOUND'                     00010400
SHIFTUP  LA    5,1(2)         R2 + 1 ADRS INTO R5                       00010500
         SH    3,=H'1'        LENGTH TO BE MOVED NOW IN R3              00010600
         CH    3,=H'1'        R3 COUNT GT OR EQ ONE ?                   00010700
         BL    EDITFIN        NO - BR TO EDITFIN                        00010800
         COR2CORE (3),(2),(5)                                           00010900
         SH    4,=H'1'        R4 BULL LENGTH LESS ONE FOR CHAR REMOVED  00011000
         CLI   CRFLAG,X'FF'   CRFLAG SET 'ON'                           00011100
         BNE   NXTBYTE        NO - BR TO NXTBYTE                        00011200
         MVI   CRFLAG,X'33'   SET CRFLAG FOR LINE FEED ( L/F ) SEARCH   00011300
         AH    2,=H'2'        POINT R2 BEYOND CR+LF ( ALREADY CHECKED ) 00011400
         SH    3,=H'2'        ADJUST R3 COUNT                           00011500
         B     NXTBYTE        BR TO NXTBYTE                             00011600
EDITFIN  L     9,0(9)         BULLEN ADRS INTO R9                       00011700
         STH   4,0(9)         STORE EDITED LEN                          00011750
         RETURN13                                                       00011800
CRFLAG   DC    X'00'                                                    00011900
         LTORG                                                          00012000
UPDATED  DC    CL10' 22/04/80 '    NEW FRONT END INTRODUCTION           00012050
 DC CL30'$Revison: $'
 DC CL40'$Date: 30/01/2006 20:21:33$'
 DC CL60'$Source: /home/us0400/mdb/op/lib/source/RCS/bulledit.s,v $'
         END                                                            00012100
