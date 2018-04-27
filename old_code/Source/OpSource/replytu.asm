*---------------------------------------------------------------------
*
* PROGRAM     : REPLYTU
*
* PURPOSE     : To issue a WTOR & check for a reply from the operator
*
* CALLED BY   : Fortran programs, so length of message can be found
*
* ARGUMENTS   : (1) reply (or '    ' if no reply yet)    (output)
*
* REVISION INFO :
*
* $Revision: 1$
* $Date: 05/05/2011 14:51:28$
* $Source: $
*
* CHANGE RECORD :
*
* $Log:
*  1    MetDB_Refresh 1.0         05/05/2011 14:51:28    Sheila Needham
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
REPLYTU  CEEENTRY PPA=PARMPPA,MAIN=NO,AUTO=WORKSIZE
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
BACK     CEETERM  RC=0
*
FIRSTIME DC    X'01'
T        DC    CL1' '
ECB      DC    F'0'
REPLY    EQU   *
 WTOR 'To end MDBFTP tidily reply T (or U if urgent: U may lose data)',-
               MF=L
ERROR    WTO   'MDBFTP not terminated - wrong reply',MF=L
         DC    C'$Date: 05/05/2011 14:51:28$'
PARMPPA  CEEPPA ,                 Constants describing the code block
WORKAREA DSECT
         ORG   *+CEEDSASZ         Leave space for the DSA fixed part
         DS    0D
WORKSIZE EQU   *-WORKAREA
         CEEDSA  ,                Mapping of the dynamic save area
         CEECAA  ,                Mapping of the common anchor area
         END
