*---------------------------------------------------------------------
*
* PROGRAM    : SECSOUT
*
* PURPOSE    : To wait the given number of seconds
*
* CALLED BY  : Fortran programs
*
* ARGUMENTS  : (1) nu,ber of seconds (input)
*
* REVISION INFO :
*
* $Revision: 1$
* $Date: 05/05/2011 14:58:22$
* $Source: $
*
* CHANGE RECORD :
*
* $Log:
*  1    MetDB_Refresh 1.0         05/05/2011 14:58:22    Sheila Needham
*       Initial check-in
*       
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
SECSOUT  CEEENTRY PPA=PARMPPA,AUTO=WORKSIZE,MAIN=NO
         EQUREG R
*        CHAIN ,,*
         L     R1,0(R1)    address of seconds
         L     R1,0(R1)    number of seconds
         MH    R1,=H'100'  convert to hundredths for STIMER
         ST    R1,SECHUNDS
         STIMER WAIT,BINTVL=SECHUNDS
         CEETERM RC=0
SECHUNDS DS    F
         DC    C'$Date: 05/05/2011 14:58:22$'
PARMPPA  CEEPPA ,                 Constants describing the code block
WORKAREA DSECT
         ORG   *+CEEDSASZ         Leave space for the DSA fixed part
         DS    0D
WORKSIZE EQU   *-WORKAREA
         CEEDSA  ,                Mapping of the dynamic save area
         CEECAA  ,                Mapping of the common anchor area
         END
