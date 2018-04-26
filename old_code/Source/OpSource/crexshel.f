***********************************************************************
*                                                                     *
* PROGRAM       : CREXSHEL                                            *
*                                                                     *
* PURPOSE       : shell to load CREX decode module                    *
*                                                                     *
* CALLED BY     : user                                                *
*                                                                     *
* LOADS         : CREX                                                *
*                                                                     *
* PARAMETERS    : arguments to be passed to CREXDEC                   *
*                                                                     *
* CHANGE RECORD :                                                     *
* Revision 1.1  2003/03/?? ??:??:??  uspm
* Initial revision
*
***********************************************************************
CREX     CSECT
CREX     AMODE ANY
CREX     RMODE ANY
         PRINT NOGEN
         EQUREG R               Let Rnn refer to registers
         CHAIN ,,*              Standard linkage
         LR    R2,R1            Keep address of argument list for call
*                                (R1 will be used by LOAD)
         L     R15,CREXLOAD     Address of loaded module (or zero)
         LTR   R15,R15          Has crex decode been loaded ?
         BNZ   CALLCREX         If so, just call it.
*
         LOAD  EP=CREX          Load the module,
         ST    R0,CREXLOAD      keep its entry point for next time
         LR    R15,R0           & put it in R15 ready for call.
*
CALLCREX LR    R1,R2            Restore the address of the arguments
         CALL  (15)             & call crexdec.
         RETURN13 RC=0          Standard return
*
CREXLOAD DC    F'0'             Address of CREX entry point from LOAD
         DC    C'$Date: 30/01/2006 20:21:55$ $Revision: 1$'
         END
