//T12SKCHG JOB (OP1,SK,TCDS1B),STAN.TEAM.6994,PRTY=8,
//         MSGCLASS=Q,MSGLEVEL=(1,1),NOTIFY=T12SK
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//* TITLE         : MDBMON
//*
//* PURPOSE       : TO REBUILD MDBMON APPLICATION FROM SOURCE.
//*                  (MDB CHASER MODULE)
//*
//* INTRODUCED    : SEPTEMBER 1998 BY JOHN NORTON (CC3)
//* COPIED HERE   : MARCH 2000 BY STAN KELLETT
//*
//* CHANGE RECORD :
//*
//* 06 AUG 1999     UAMFP CHANGED TO UAMFP31 IN MODULES LINKED FROM
//*                 MET.PROGLIB.                                    JN
//* 03 MAR 2000     CHANGED BUILD FILES TO PICK UP CHANGES FILES
//*                                                                 SAK
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//*
//S1 EXEC FORT2CL,FPARMS='CHARLEN(28000),NOFIPS,DC(*)',
//        LPARMS='MAP,XREF,AMODE=31'
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(BINCNV),DISP=SHR
//           DD DSN=MDB.SOURCE(CHSBUL),DISP=SHR
//           DD DSN=MDB.SOURCE(CHSDAY),DISP=SHR
//           DD DSN=MDB.SOURCE(CHSDBL),DISP=SHR
//           DD DSN=MDB.SOURCE(CHSDRP),DISP=SHR
//           DD DSN=MDB.SOURCE(CHSDST),DISP=SHR
//           DD DSN=MDB.SOURCE(CHSMAN),DISP=SHR
//           DD DSN=MDB.SOURCE(CHSRPS),DISP=SHR
//           DD DSN=MDB.SOURCE(CHSSTN),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(MDBMON),DISP=SHR,SPACE=,UNIT=         
//*                                                                     
//LKED.SDBLOAD DD DSN=SDB.LOADLIB,DISP=SHR                              
//LKED.METPROG DD DSN=MET.PROGLIB,DISP=SHR                              
//LKED.OLD     DD DSN=SDB.LOADLIB,DISP=SHR                              
//LKED.SYSIN   DD *                                                     
         INCLUDE SDBLOAD(MDB)                                           
         INCLUDE METPROG(PARMCH,UAMFP31,ZPDATE)                         
         INCLUDE OLD(MDBMON)                                            
         ENTRY CHSMAN                                                   
/*                                                                      
//
