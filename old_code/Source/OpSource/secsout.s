*----------------------------------------------------------------------
*
* PROGRAM       : SECSOUT in MDBMAIN
*
* PURPOSE       : to wait a given number of seconds
*
* CALLED BY     : Fortran programs
*
* PARAMETERS    : (1) number of seconds to wait
*
* CHANGE RECORD : OPERATIONAL FROM ........
*   DATE :-        PURPOSE:-
*
*----------------------------------------------------------------------
*$Revision: 1$
*$Date: 30/01/2006 20:24:10$
*
SECSOUT  CSECT
         EQUREG R
         CHAIN ,,*
         L     R1,0(R1)    address of seconds
         L     R1,0(R1)    number of seconds
         MH    R1,=H'100'  convert to hundredths for STIMER
         ST    R1,SECHUNDS
         STIMER WAIT,BINTVL=SECHUNDS
         RETURN13
SECHUNDS DS    F
         DC    C'$Date: 30/01/2006 20:24:10$'
         END
