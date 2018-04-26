//M12CLBOG JOB (M12,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q           00001000
//*                                                                     00002000
//* JOB TO MERGE BOGUS (OPERATIONAL 2004?)                              00003000
//*                                                                     00016000
//MERBOGUS PROC                                                         00017000
// EXEC PGM=MERSOUP,REGION=6M                                           00017100
//STEPLIB  DD DSN=SYS1.SDBLOAD,DISP=SHR                                 00017200
//         DD DSN=SDB.LOADLIB,DISP=SHR                                  00017300
//FT01F001 DD DSN=MDB.MERGE.TABLES(BOG&LEVEL),DISP=SHR                  00019000
//FT02F001 DD DSN=MDB.BOGUS.OPER.GAIR,DISP=SHR                          00020000
//FT11F001 DD DSN=MS12.EEADDAT.&TYPE..&LEVEL.A,DISP=SHR,LABEL=(,,,IN)   00030000
//FT12F001 DD DSN=MS12.EEADDAT.&TYPE..&LEVEL.B,DISP=SHR,LABEL=(,,,IN)   00040000
//TABLEB   DD DSN=SDB.BUFR.TABLEB,DISP=SHR,LABEL=(,,,IN)                00050000
//TABLED   DD DSN=SDB.BUFR.TABLED,DISP=SHR,LABEL=(,,,IN)                00060000
//CODEFIG  DD DSN=SDB.BUFR.CODEFIGS,DISP=SHR,LABEL=(,,,IN)              00070000
// PEND                                                                 00080000
//*                                                                     00090000
// EXEC MERBOGUS,TYPE=BOGUS,LEVEL=SURF                                  00100000
// EXEC MERBOGUS,TYPE=BOGUS,LEVEL=UAIR                                  00110000
// EXEC MERBOGUS,TYPE=TCBOGUS,LEVEL=SURF                                00120000
// EXEC MERBOGUS,TYPE=TCBOGUS,LEVEL=UAIR                                00130000
