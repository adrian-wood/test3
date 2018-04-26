***********************************************************************
*                                                                     *
* PROGRAM       : REPLYTU                          (MDB storage)      *
*                                                                     *
* PURPOSE       : To issue a WTOR & check for a reply from the operator
*                                                                     *
* CALLED BY     : MDBFTP                                              *
*                                                                     *
* PARAMETERS    : (1) reply (or ' ' if no reply yet)    (output)      *
*                                                                     *
* CHANGE RECORD :                                                     *
*                                                                     *
***********************************************************************
REPLYTU  CSECT
         PRINT NOGEN
         CHAIN
         L     2,0(1)               Keep return address for reply
         CLI   FIRSTIME,0           First time?
         BE    NOTFIRST             If not, check for reply.
*
* Write To Operator
* (Test as WTO MF=(E,REPLY),ROUTCDE=(11) to see text in job output)
*
MESSAGE  WTOR  ,T,1,ECB,MF=(E,REPLY)
         MVI   FIRSTIME,0           Unset flag so as not to reissue
         B     BLANK                and return.
*
* Come straight here once the message has been issued
*
NOTFIRST TM    ECB,X'40'            Has the operator replied?
         BNO   BLANK                If not, return.
         CLC   T,=C'T'              If so, is reply T or U?
         BL    BADREPLY             If not, reissue message.
         CLC   T,=C'U'              If so, is reply T or U?
         BH    BADREPLY             If not, reissue message.
*
         MVC   0(1,2),T             Return reply to caller
         B     BACK
*
BADREPLY WTO   MF=(E,ERROR)
         XC    ECB,ECB              Clear ECB
         XC    T,T                  Clear REPLY
         B     MESSAGE              Reissue message
*
BLANK    MVI   0(2),C' '            Return blank reply
BACK     RETURN13 RC=0
*
FIRSTIME DC    X'01'
T        DC    CL1' '
ECB      DC    F'0'
REPLY    EQU   *
 WTOR 'To end MDBFTP tidily reply T (or U if urgent: U may lose data)',-
               MF=L
ERROR    WTO   'MDBFTP not terminated - wrong reply',MF=L
         DC    C'$Revision: 1$ $Date: 30/01/2006 20:24:01$'
         END
