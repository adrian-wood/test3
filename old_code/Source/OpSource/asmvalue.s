***********************************************************************
*                                                                     *
* PROGRAM       : VALUE                                               *
*                                                                     *
* PURPOSE       : GET VALUE IN WIDTH BITS AFTER IBEFOR BITS IN STRING.*
*                     ~~~~~    ~~~~~            ~~~~~~         ~~~~~~ *
* NOTE          : MUST ASSUME THAT OCTETS CORRESPOND TO CHARACTERS    *
*                                                                     *
* PARAMETERS    : (1) STRING OF DESCRIPTORS                           *
*                 (2) NUMBER OF BITS TO SKIP BEFORE TARGET FIELD      *
*                 (3) WIDTH OF TARGET FIELD                           *
*                                                                     *
*Y2K  16.06.1997  ASMVALUE IS YEAR 2000 COMPLIANT.                    *
*                                                                     *
* CHANGE RECORD :                                                     *
*   DATE :-        PURPOSE:-                                          *
*                                                                     *
***********************************************************************
* $Log:
*  1    Met_DB_Project 1.0         30/01/2006 20:20:57    Sheila Needham  
* $
* Revision 1.2  2003/03/06 09:03:32  usmdb
* Lines added to change addressing mode - C.Long
*
* Revision 1.1  1997/08/05 15:45:32  uspm
* Initial revision
*
*
***********************************************************************
VALUE    CSECT
VALUE    AMODE ANY                                                 !1.2
VALUE    RMODE ANY                                                 !1.2
         PRINT NOGEN
         EQUREG R
         USING *,R15
         STM   R2,R12,28(R13)  SAVE REGISTERS
         LM    R2,R4,0(R1)     STRING, BITS TO SKIP, WIDTH OF VALUE
*
         SR    R0,R0           ZERO ANSWER IN CASE WIDTH=ZERO
         L     R4,0(R4)        WIDTH OF NEW VALUE
         LTR   R4,R4           ZERO ?
         BZ    BACK            IF SO, NOTHING TO DO
*
         L     R6,0(R3)        BITS TO SKIP IN STRING
         LR    R10,R6          KEEP FOR UPDATE AT END
         SR    R7,R7           CLEAR 2ND REGISTER OF PAIR FOR DIVISION
         SRDL  R6,5            DIVIDE BY 32 TO GET FULLWORDS TO SKIP
         SLL   R6,2            QUOTIENT TIMES 4 IS DISPLACEMENT
*
         SRL   R7,27           BITS IN LAST FULLWORD (FROM REMAINDER)
         LA    R9,32           SHIFT LAST 2 WORDS LEFT THAT MANY BITS,
         SR    R9,R4           THEN RIGHT 32 MINUS LENGTH OF NEW VALUE
*
         AR    R2,R6           ADD DISPLACEMENT TO START OF STRING
         LM    R0,R1,0(R2)     LOAD LAST WHOLE FULLWORD & INCOMPLETE
         SLDL  R0,0(R7)        SHIFT VALUE REQUIRED TO LEFT-HAND END
         SRL   R0,0(R9)        & BACK TO RIGHT-HAND END OF REGISTER
*                               (FUNCTION, SO ANSWER RETURNED IN R0)
         AR    R10,R4          ADD WIDTH OF NEW VALUE TO BITS USED
         ST    R10,0(R3)       STORE UPDATED BITS USED
BACK     LM    R2,R12,28(R13)  RESTORE REGISTERS
         BR    R14             RETURN
         DC    C'$Date: 30/01/2006 20:20:57$ $Revision: 1$'
         END
