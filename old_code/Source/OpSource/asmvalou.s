***********************************************************************
*                                                                     *
* PROGRAM       : VALOUT   (ASSEMBLER VERSION FOR OPERATIONAL USE)    *
*                                                                     *
* PURPOSE       : PUT VALUE IN WIDTH BITS AFTER IBEFOR BITS IN STRING.*
*                     ~~~~~    ~~~~~            ~~~~~~         ~~~~~~ *
* NOTE          : MUST ASSUME THAT OCTETS CORRESPOND TO CHARACTERS.   *
*                 ASSUME ALSO THAT WE'RE ALWAYS MOVING FORWARD        *
*                 (EXCEPT AT THE START, TO WHICH WE RETURN AT THE END *
*                 TO STORE THE LENGTH), SO THAT BITS AHEAD OF THE     *
*                 VALUE BEING PUT IN THE STRING CAN SIMPLY BE ZEROED. *
*                                                                     *
* PARAMETERS    : (1) STRING OF DESCRIPTORS                           *
*                 (2) NUMBER OF BITS ALREADY IN STRING                *
*                 (3) WIDTH OF NEW FIELD                              *
*                 (4) VALUE TO BE INSERTED                            *
*                                                                     *
*Y2K  16.06.1997  ASMVALOU IS YEAR 2000 COMPLIANT.
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
* Revision 1.1  1997/08/05 15:44:39  uspm
* Initial revision
*
*
***********************************************************************
VALOUT   CSECT
VALOUT   AMODE ANY                                                 !1.2
VALOUT   RMODE ANY                                                 !1.2
         PRINT NOGEN
         EQUREG R
         USING *,R15
         STM   R2,R12,28(R13)  SAVE REGISTERS
         LM    R2,R5,0(R1)     STRING, BITS USED, WIDTH, NEW VALUE
*
         L     R4,0(R4)        WIDTH OF NEW VALUE
         LTR   R4,R4           ZERO ?
         BZ    BACK            IF SO, NOTHING TO DO
*
* R4 IS THE NUMBER OF BITS TO BE ADDED TO THE STRING
*
         L     R6,0(R3)        BITS ALREADY USED IN STRING
         LR    R0,R6           KEEP FOR UPDATE AT END
         SR    R7,R7           CLEAR 2ND REGISTER OF PAIR FOR DIVISION
         SRDL  R6,5            DIVIDE BY 32 TO GET FULLWORDS USED
         SLL   R6,2            QUOTIENT TIMES 4 IS DISPLACEMENT
         AR    R2,R6           ADD DISPLACEMENT TO START OF STRING
*
* R2 POINTS TO THE FULLWORD WHERE THE VALUE STARTS. R7: BITS USED IN IT
*
         SRL   R7,27           BITS IN LAST FULLWORD (FROM REMAINDER)
         LA    R9,64           NEW VALUE MUST BE SHIFTED (64-X-Y) BITS,
         SR    R9,R7           WHERE X IS BITS USED IN LAST FULLWORD
         SR    R9,R4           AND Y IS WIDTH OF NEW VALUE.
*
* R9: HOW MANY BITS VALUE MUST BE SHIFTED LEFT FROM RIGHT-HAND END OF
*  REGISTER PAIR TO FOLLOW ON FROM LAST VALUE IN STRING
*
         L     R11,0(R5)       VALUE TO BE PUT IN STRING
         SR    R10,R10         CLEAR 1ST REGISTER IN PAIR FOR SHIFT
         SLDL  R10,0(R9)       SHIFT INTO POSITION TO ADD IT TO STRING
*
* KEEP ANY BITS ALREADY SET IN THE WORD ON THE LEFT OF THE NEW VALUE
* (BITS ON THE RIGHT MUST BE ZEROED UNLESS IT'S THE LENGTH AT THE START
*
         LTR   R7,R7           ARE WE AT THE START OF A FULLWORD?
         BZ    LHWORDOK        IF SO, DON'T NEED ANY BITS FROM STRING
         A     R10,0(R2)       IF NOT, ADD IN BITS ALREADY SET
LHWORDOK LTR   R0,R0           ZERO BITS USED? (I.E. INSERTING LENGTH?)
         BNZ   TOSTRING        IF NOT, REGISTER PAIR READY TO BE STORED
         A     R11,4(R2)       IF LENGTH, ADD IN FIRST WORD OF DATA
TOSTRING STM   R10,R11,0(R2)   BOTH THESE REGS CAN HAVE BITS IN VALUE
*
* THE VALUE IS EITHER ALL IN R10 OR SPLIT BETWEEN R10 & R11;
* BUT QUICKER TO STORE BOTH THAN DO EXTRA TEST (?)
*
         AR    R0,R4           ADD WIDTH OF NEW VALUE TO BITS USED
         ST    R0,0(R3)        STORE UPDATED BITS USED
BACK     LM    R2,R12,28(R13)  RESTORE REGISTERS
         BR    R14             RETURN
         DC    C'$Date: 30/01/2006 20:20:57$ $Revision: 1$'
         END
