***********************************************************************
*                                                                     *
* PROGRAM       : REPLYT                          (MDB storage)       *
*                                                                     *
* PURPOSE       : To issue a WTOR & check for a reply from the operator
*                                                                     *
* CALLED BY     : ??????                                              *
*                                                                     *
* PARAMETERS    : (1) reply (or '    ' if no reply yet)    (output)   *
*                                                                     *
* CHANGE RECORD :                                                     *
*                                                                     *
***********************************************************************
REPLYT   CSECT
         PRINT NOGEN
         CHAIN
         L     2,0(1)               Keep return address for reply
         CLI   FIRSTIME,0           First time?
         BE    NOTFIRST             If not, check for reply.
*
* Find job name by jumping from control block to control block
*
         L     1,16
         L     1,0(1)
         L     1,4(1)
         L     1,12(1)
*
* Put job name in termination message & error message
* & characters 4-6 of job name after T in reply
*
         MVC   JOBNAME(8),0(1)
         MVC   ERROR+4(8),JOBNAME
         MVC   TJOB+1(3),JOBNAME+3
*
* Write To Operator
* (Test as WTO MF=(E,REPLY),ROUTCDE=(11) to see text in job output)
*
MESSAGE  WTOR  ,T,4,ECB,MF=(E,REPLY)
         MVI   FIRSTIME,0           Unset flag so as not to reissue
         B     BLANK                and return.
*
* Come straight here once the message has been issued
*
NOTFIRST TM    ECB,X'40'            Has the operator replied?
         BNO   BLANK                If not, return.
         CLC   T+1(3),TJOB+1        If so, is reply for right job?
         BNE   BADREPLY             If wrong, reissue message.
*
         MVC   0(4,2),T             Return reply to caller
         B     BACK
*
BADREPLY WTO   MF=(E,ERROR)
         XC    ECB,ECB              Clear ECB
         XC    T,T                  Clear REPLY
         B     MESSAGE              Reissue message
*
BLANK    MVC   0(4,2),NOREPLY       Return blank reply
BACK     RETURN13 RC=0
*
FIRSTIME DC    X'01'
ECB      DC    F'0'
T        DC    F'0'
NOREPLY  DC    CL4' '               Return spaces if no reply yet.
REPLY    WTOR  'To end MDB..... on its own, reply T...',MF=L
JOBNAME  EQU   REPLY+19
TJOB     EQU   JOBNAME+27
ERROR    WTO   'MDB..... not terminated - wrong reply',MF=L
         DC    C'$Revision: 1$ $Date: 30/01/2006 20:24:00$'
         END
