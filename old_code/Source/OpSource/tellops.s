*----------------------------------------------------------------------
*
* PROGRAM       : TELLOPS in MDBMAIN
*
* PURPOSE       : to put a message on the operator's screen (no reply)
*
* CALLED BY     : Fortran programs, so length of message can be found
*
* PARAMETERS    : (1) message to be displayed (up to 50 characters)
*
* CHANGE RECORD : OPERATIONAL FROM ........
*   DATE :-        PURPOSE:-
*
*----------------------------------------------------------------------
*$Revision: 1$
*$Date: 30/01/2006 20:25:17$
*
TELLOPS  CSECT
         EQUREG R
         CHAIN ,,*
         L     R2,0(R1)    address of message
         LR    R3,R1       now find corresponding length
         SH    R3,=H'4'    back 4 bytes from start of argument list
         A     R1,0(R3)    add length of argument list
         L     R1,0(R1)    load pointer to length of message
         L     R1,0(R1)    load length of message
*
         CH    R1,=H'50'   length>50?
         BNH   TEXTOWTO
         LH    R1,=H'50'   if so, only take first 50 characters
TEXTOWTO BCTR  R1,0        reduce length by 1 to go in MVC
         EX    R1,TOWTO    move message into 50 blanks in WTO
*                          (WTO starts BAL, length, flags - 8 bytes)
WTO      WTO   '                                                  ',   *
               ROUTCDE=(11)
         RETURN13
*
TOWTO    MVC   WTO+8(0),0(R2)
         DC    C'$Date: 30/01/2006 20:25:17$'
         END
