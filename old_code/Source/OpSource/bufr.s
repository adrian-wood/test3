***********************************************************************
*
* PROGRAM       : BUFR      (ENTRY POINT OF LOAD MODULE BUFR)
*
* PURPOSE       : TO PROVIDE ADDRESSES OF ENTRY POINTS FOR USER CALLS
*
* CALLED BY     : SHELL WHICH LOADS BUFR LOAD MODULE
*
* CHANGE RECORD :
*
* $Log:
*  1    Met_DB_Project 1.0         30/01/2006 20:21:18    Sheila Needham  
* $
* Revision 1.3  2003/05/06 07:55:55  usmdb
* Renamed DECODE to BUFDATA - S.Cox.
*
* Revision 1.2  2003/03/06 09:03:32  usmdb
* Lines added to change addressing mode - C.Long
*
* Revision 1.1  1997/08/05 15:46:00  uspm
* Initial revision
*
* FEB 97 : ENTRY POINTS FOR BUFV2 & ENBUFV2 ADDED                  !A
* JUL 93 : ENTRY POINTS FOR LOCALB & LOCALD ADDED
*
***********************************************************************
*
*
***********************************************************************
BUFR     CSECT
BUFR     AMODE ANY                                                 !1.2
BUFR     RMODE ANY                                                 !1.2
         DC    CL8'DECODE'  DECODE to match name in BUFRSHEL       !1.3
         DC    V(BUFDATA)   but links in BUFDATA.                  !1.3
         DC    CL8'DECODI'
         DC    V(DECODI)
         DC    CL8'DEBUFR'               REAL DECODE
         DC    V(DEBUFR)
         DC    CL8'CODE'
         DC    V(CODE)
         DC    CL8'SCRIPT'
         DC    V(SCRIPT)
         DC    CL8'TABLEB'
         DC    V(TABLEB)
         DC    CL8'TABLED'
         DC    V(TABLED)
         DC    CL8'ENBUFR'               REAL ENCODE
         DC    V(ENBUFR)
         DC    CL8'ENCODE'
         DC    V(ENCODE)
         DC    CL8'ENCODI'
         DC    V(ENCODI)
         DC    CL8'DESFXY'
         DC    V(DESFXY)
         DC    CL8'DECORD'
         DC    V(DECORD)
         DC    CL8'IDES'
         DC    V(IDES)
         DC    CL8'DBUFRI'               INTEGER DECODE
         DC    V(DBUFRI)
         DC    CL8'NBUFRI'               INTEGER ENCODE
         DC    V(NBUFRI)
         DC    CL8'LOCALB'               LOCAL TABLE B
         DC    V(LOCALB)
         DC    CL8'LOCALD'               LOCAL TABLE D
         DC    V(LOCALD)
         DC    CL8'BUFV2'                TO CONVERT TO VERSION 2     !A
         DC    V(BUFV2)                                              !A
         DC    CL8'ENBUFV2'              TO MAKE A VERSION 2 MESSAGE !A
         DC    V(ENBUFV2)                                            !A
         DC    C'$Date: 30/01/2006 20:21:18$ $Revision: 1$'
         END
