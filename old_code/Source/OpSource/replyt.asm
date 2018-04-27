*---------------------------------------------------------------------
*
* PROGRAM    : REPLYT
*
* PURPOSE    : To issue a WTOR & check for a reply from the operator
*
* CALLED BY  : Fortran programs, so length of message can be found
*
* ARGUMENTS  : (1) reply (or '    ' if no reply yet)    (output)
*
* REVISION INFO :
*
* $Revision: 2$
* $Date: 05/05/2011 14:51:13$
* $Source: $
*
* CHANGE RECORD :
*
* $Log:
*  2    MetDB_Refresh 1.1         05/05/2011 14:51:13    Sheila Needham
*       Corrected continuations
*  1    MetDB_Refresh 1.0         05/05/2011 14:19:10    Sheila Needham
*       Initial check-in
* $
* --------------------------------------------------------------------
* (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
*
* Met Office, United Kingdom
*
* The use, duplication and disclosure of this code is strictly
* prohibited without the permission of The Meteorological Database
* Team at the above address.
*  -------------------------------------------------------------------
REPLYT   CEEENTRY PPA=PARMPPA,MAIN=NO,AUTO=WORKSIZE
         L     2,0(1)               Keep return address for reply
         CLI   FIRSTIME,0           First time?
         BE    NOTFIRST             If not, check for reply.
*
* Findhjob name by jumping from control block to control block
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
BACK     CEETERM  RC=0
*
FIRSTIME DC    X'01'
ECB      DC    F'0'
T        DC    F'0'
NOREPLY  DC    CL4' '               Return spaces if no reply yet.
REPLY    WTOR  'To end MDB..... on its own, reply T...',MF=L
JOBNAME  EQU   REPLY+19
TJOB     EQU   JOBNAME+27
ERROR    WTO   'MDB..... not terminated - wrong reply',MF=L
         DC    C'$Date: 05/05/2011 14:51:13$'
PARMPPA  CEEPPA ,                 Constants describing the code block
WORKAREA DSECT
         ORG   *+CEEDSASZ         Leave space for the DSA fixed part
         DS    0D
WORKSIZE EQU   *-WORKAREA
         CEEDSA  ,                Mapping of the dynamic save area
         CEECAA  ,                Mapping of the common anchor area
         END
