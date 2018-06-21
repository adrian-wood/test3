//T12SCIN1 JOB (OP1,SC,WDS0BF),MDBTEAM.6955,PRTY=??,MSGCLASS=Q
//*
//* ---------------------------------------------------------------------
//* TO INITIALISE AN MDB DATA SET - SET PARAMETERS ON EXEC CARD AT END
//* (NO SEQUENCE IN SECOND BLOCK - USE MDBINITS IF ONE IS WANTED)
//* ---------------------------------------------------------------------
//*
//* --- compile & link mdbinit source
//*
//CL EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(28000)'
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(MDBINIT),DISP=SHR
//           DD DSN=MDB.STORAGE.SRCE(CHAR2),DISP=SHR
//LKED.MP DD DSN=MET.PROGLIB,DISP=SHR
 INCLUDE MP(CLPARM)
/*
//* --- execute mdbinit
//*
//INIT PROC CYLS=
// EXEC FORT2G,
// PARM='&CYLS,&BLOCK,&ENTRYLEN,&INDBLOKS,&HRSINBLK,&OFFSET,&OVERFLOS/'
//GO.FT01F001 DD DSN=&DSN,DISP=(,CATLG),VOL=SER=&DISK,
//  DCB=(RECFM=F,BLKSIZE=&BLOCK),UNIT=DISK,SPACE=(CYL,&CYLS)
// PEND
// EXEC INIT,DSN='MDB.??????',CYLS=12,BLOCK=27998,
//    ENTRYLEN=12,INDBLOKS=20,HRSINBLK=6,OFFSET=3,OVERFLOS=0,
//    DISK=OPR103