*---------------------------------------------------------------------
*
* PROGRAM       : TELLOPS
*
* PURPOSE       : to put a message on the operator's screen (no reply)
*
* CALLED BY     : Fortran programs, so length of message can be found
*
* ARGUMENTS     : (1) message to be displayed (up to 50 characters)
*
* REVISION INFO :
*
* $Revision: 2$
* $Date: 05/05/2011 14:50:45$
* $Source: $
*
* CHANGE RECORD :
*
* $Log:
*  2    MetDB_Refresh 1.1         05/05/2011 14:50:45    Sheila Needham
*       Corrected continuations
*       
*  1    MetDB_Refresh 1.0         05/05/2011 14:14:44    Sheila Needham
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
TELLOPS  CEEENTRY PPA=PARMPPA,MAIN=NO,AUTO=WORKSIZE
         EQUREG R
         LM    R2,R3,0(R1)    addr(message),length
*
         CH    R3,=H'50'   length>50?
         BNH   TEXTOWTO
         LH    R3,=H'50'   if so, only take first 50 characters
TEXTOWTO BCTR  R3,0        reduce length by 1 to go in MVC
         EX    R3,TOWTO    move message into 50 blanks in WTO
*                          (WTO starts BAL, length, flags - 8 bytes)
WTO      WTO   '                                                  ',   *
               ROUTCDE=(11)
GO_HOME  DS    0H
         CEETERM  RC=0
*
TOWTO    MVC   WTO+8(0),0(R2)
         DC    C'$Date: 05/05/2011 14:50:45$'
PARMPPA  CEEPPA ,                 Constants describing the code block
WORKAREA DSECT
         ORG   *+CEEDSASZ         Leave space for the DSA fixed part
         DS    0D
WORKSIZE EQU   *-WORKAREA
         CEEDSA  ,                Mapping of the dynamic save area
         CEECAA  ,                Mapping of the common anchor area
         END
