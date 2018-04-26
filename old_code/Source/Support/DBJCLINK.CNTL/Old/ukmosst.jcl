//M12DBSST JOB (M12,DB,TCDS1B),MDB.TEAM.6951,MSGCLASS=Q,PRTY=??
//*
//*---------------------------------------------------------------------
//*            BUILDS MET.D.B. STORAGE MODULE ("UKMOSST")
//*            ------------------------------------------
//*    Makes load module for job to read, process and store AUTOSAT-3
//*  sea surface temperature data in the MetDB. (UKMOSST & FOAMSST).
//*
//* REVISION INFO :
//*
//* $Revision: 2$
//* $Date: 22/06/2007 15:51:48$
//*
//*  19/07/99: ORIGINAL OPERATIONAL VERSION.
//*  MAR 2000: INCLUDE MDB.SOURCE(INDLALO).
//*  JAN 2001: INCLUDE MET.PROGLIB(CLPARM).
//*  APR 2001: DON'T INCLUDE MET.SRCELIB(IBMISO8).
//*  10/12/01: DON'T INCLUDE MDB.SOURCE(LIMAREA).
//*  19/05/03: CHANGE JOB TO USE 'BUFREP' INSTEAD OF 'SATREP'.     !1.8
//*---------------------------------------------------------------------
//*
//*INFORM PRINTDATA
//*
//UKMOSST EXEC FORT2CL,FPARMS='NOFIPS,OPT(3),DC(*),CHARLEN(27998)',
//        REGION=3000K,OUTPUT=Q
//FORT.SYSIN DD DSN=MDB.SOURCE(UKMOSST),DISP=SHR
//           DD DSN=MDB.SOURCE(BUFREP),DISP=SHR                    !1.8
//           DD DSN=MDB.SOURCE(BUFSEQ),DISP=SHR
//           DD DSN=MDB.SOURCE(CHAR2),DISP=SHR                     !1.8
//           DD DSN=MDB.SOURCE(CHAR3),DISP=SHR                     !1.8
//           DD DSN=MDB.SOURCE(CLRMAP),DISP=SHR                    !1.8
//           DD DSN=MDB.SOURCE(DUPCHK),DISP=SHR                    !1.8
//           DD DSN=MDB.SOURCE(FREEBLK),DISP=SHR                   !1.8
//           DD DSN=MDB.SOURCE(GETVALS),DISP=SHR                   !1.8
//           DD DSN=MDB.SOURCE(ICHAR2),DISP=SHR                    !1.8
//           DD DSN=MDB.SOURCE(ICHAR3),DISP=SHR
//           DD DSN=MDB.SOURCE(INDEX1),DISP=SHR                    !1.8
//           DD DSN=MDB.SOURCE(INDEX2),DISP=SHR                    !1.8
//           DD DSN=MDB.SOURCE(IOBLKS),DISP=SHR                    !1.8
//           DD DSN=MDB.SOURCE(LATBOX),DISP=SHR
//           DD DSN=MDB.SOURCE(ONLAND),DISP=SHR                    !1.8
//           DD DSN=MDB.SOURCE(TRANGE),DISP=SHR                    !1.8
//*
//LKED.SYSLMOD DD UNIT=,SPACE=,DCB=,DISP=SHR,
//             DSN=MCC3.DBLOAD.D??????(UKMOSST)
//*
//LKED.PROGLIB DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SDBLOAD DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.SYSIN   DD *
 INCLUDE PROGLIB(CLPARM,ZPDATE)
 INCLUDE SDBLOAD(BUFRSHEL)
/*
//
