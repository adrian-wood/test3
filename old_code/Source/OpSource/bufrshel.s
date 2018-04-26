***********************************************************************
*                                                                     *
* PROGRAM       : BUFRSHEL                                            *
*                                                                     *
* PURPOSE       : SHELL ROUTINE FOR BUFR GENERAL MODULES.             *
*                                                                     *
* DESCRIPTION   : ENTRY POINTS FOR ENCODING/DECODING AND REAL/INTEGER *
*                 AND OTHER CSECTS IN THE MODULE BUFR.                *
*                                                                     *
* CALLED BY     : USER                                                *
*                                                                     *
* LOADS         : BUFR                                                *
*                                                                     *
* PARAMETERS    : NONE (ENTRY ADDRESS IN R15 SAYS WHICH CALL TO MAKE) *
*                                                                     *
*Y2K  16.06.1997  BUFRSHEL IS YEAR 2000 COMPLIANT.
*                                                                     *
* CHANGE RECORD :                                                     *
*   JULY 93: ADD ENTRY POINTS FOR LOCALB & LOCALD                     *
*   FEB 97 : ADD ENTRY POINTS FOR BUFV2 & ENBUFV2                    !A
*                                                                     *
* Revision 1.1  1997/08/06 09:53:48  uspm
* Initial revision
*
*
***********************************************************************
BUFRSHEL CSECT
BUFRSHEL AMODE ANY                                                 !1.2
BUFRSHEL RMODE ANY                                                 !1.2
         PRINT NOGEN
         EQUREG R
         ENTRY DECODE,DECODI,DEBUFR,CODE,SCRIPT,TABLEB
         ENTRY TABLED,ENBUFR,ENCODE,ENCODI,DESFXY,DECORD,IDES
         ENTRY DBUFRI,NBUFRI,LOCALB,LOCALD,BUFV2,ENBUFV2             !A
* R15 CONTAINS DISPLACMENT DEPENDING ON ENTRY POINT.
DECODE   BC    0,0
DECODI   BC    0,12
DEBUFR   BC    0,24
CODE     BC    0,36
SCRIPT   BC    0,48
TABLEB   BC    0,60
TABLED   BC    0,72
ENBUFR   BC    0,84
ENCODE   BC    0,96
ENCODI   BC    0,108
DESFXY   BC    0,120
DECORD   BC    0,132
IDES     BC    0,144
DBUFRI   BC    0,156
NBUFRI   BC    0,168
LOCALB   BC    0,180
LOCALD   BC    0,192
BUFV2    BC    0,204                                                 !A
ENBUFV2  BC    0,216                                                 !A
*
* R0 IS USED IN FUNCTION CALLS.  REPLACE CHAIN WITH 2 STM TO SKIP R0
*        CHAIN
         DS    0H
         STM   R14,R15,12(R13)
         STM   R1,R12,24(R13)
         CNOP  4,8
         LR    R2,R13
         BALR  R13,0
         LA    R13,8(R13)
         B     72(R13)
         USING *,13
IMO0002P DC    9D'0'
         ST    R13,8(R2)
         ST    R2,4(R13)
*
         LR    R3,R1               SAVE POINTER TO ARGUMENT LIST
*
* Keep current addressing mode, switching to 31-bit if not already so.
*
         LA    R1,KEEPMODE      LA sets 1 in top bit if AMODE=31   !1.2
         LTR   R1,R1            One in top bit?                    !1.2
         BM    CLEARTWO         If so, still 31-bit mode after BSM !1.2
         A     R1,TOPBITON      If not, set top bit to switch      !1.2
CLEARTWO SR    R2,R2            To get top bit for current mode    !1.2
         BSM   R2,R1            Set R2 to input mode then AMODE=21 !1.2
KEEPMODE ST    R2,USERMODE      Keep current mode to reset at end  !1.2
*
         LH    R2,2(R15)           DISPLACEMENT IN ADDRESS LIST IN R2
         L     R15,ADDRESSS+8(R2)  ADDRESS OF MODULE
         LTR   R15,R15
         BNZ   CALLIT
         BAL   R8,LOADIT           LOAD ALL ADDRESSES
         L     R15,ADDRESSS+8(R2)
*
CALLIT   LR    R1,R3               RESTORE ARGUMENT LIST
         CALL  (15)
*
* Before returning to user reset addressing mode to 24-bit if necessary
*
         L     R1,USERMODE      Mode in which BUFR was called      !1.2
         LTR   R1,R1            31-bit mode?                       !1.2
         BM    RETURN13         If so, no need to switch           !1.2
         LA    R1,RETURN13      If not, load address for BSM       !1.2
         S     R1,USERMODE      & unset top bit to set AMODE=24    !1.2
         BSM   0,R1             Switch back to 24-bit mode         !1.2
*
* CODE RETURN 13 EXPLICITY LEAVING R0 FOR FUNCTION CALLS
*        RETURN13 RC=0
RETURN13 L     R13,4(R13)
         LM    R14,R15,12(R13)
         LM    R1,R12,24(R13)
         LA    R15,0
         BR    R14
*
LOADIT   LOAD  EP=BUFR          LOAD MODULE
         LR    R10,R0           ADDRESS OF START OF MODULE
         LA    R6,ADDRESSS      LIST OF ENTRY NAMES
         L     R4,NENTRIES      NUMBER IN BUFR LIST
         L     R5,NENTRIES      NUMBER IN NAMES LIST
* LOOP OVER NAMES IN 'BUFR' AND STORE CORRESPONDING ENTRY POINT ADRS
* IN THIS CSECT.
NEXTADR  CLC   0(6,R10),0(R6)   DOES THIS NAME MATCH?
         BE    STADR            YES, SO STORE THE ADDRESS
         LA    R6,12(R6)        TRY THE NEXT NAME
         BCT   R5,NEXTADR
         BR    R8
*
STADR    MVC   8(4,R6),8(R10)
         LA    R6,ADDRESSS      BACK TO START OF LIST
         L     R5,NENTRIES
         LA    R10,12(R10)      NEXT NAME IN 'BUFR'
         BCT   R4,NEXTADR
         BR    R8
*
USERMODE DS    F                To keep top bit for input mode     !1.2
TOPBITON DC    X'80000000'      Only top bit set (for AMODE=31)    !1.2
*
NENTRIES DC    F'19'            NUMBER OF NAMES/ADDRESSES TO FOLLOW  !A
ADDRESSS DC    CL8'DECODE'
         DC    F'0'             ADR OF ENTRY POINT WHEN LOADED
         DC    CL8'DECODI'
         DC    F'0'
         DC    CL8'DEBUFR'
         DC    F'0'
         DC    CL8'CODE  '
         DC    F'0'
         DC    CL8'SCRIPT'
         DC    F'0'
         DC    CL8'TABLEB'
         DC    F'0'
         DC    CL8'TABLED'
         DC    F'0'
         DC    CL8'ENBUFR'
         DC    F'0'
         DC    CL8'ENCODE'
         DC    F'0'
         DC    CL8'ENCODI'
         DC    F'0'
         DC    CL8'DESFXY'
         DC    F'0'
         DC    CL8'DECORD'
         DC    F'0'
         DC    CL8'IDES  '
         DC    F'0'
         DC    CL8'DBUFRI'
         DC    F'0'
         DC    CL8'NBUFRI'
         DC    F'0'
         DC    CL8'LOCALB'
         DC    F'0'
         DC    CL8'LOCALD'
         DC    F'0'
         DC    CL8'BUFV2'                                            !A
         DC    F'0'                                                  !A
         DC    CL8'ENBUFV2'                                          !A
         DC    F'0'                                                  !A
*
         DC    C'$Date: 30/01/2006 20:21:30$ $Revision: 1$'
         END
