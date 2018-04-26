//M12CLSST JOB (M12,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q           00001000
//*                                                                     00002000
//* JOB TO STORE SST SUPEROBS (OPERATIONAL OCT 2003?)                   00003000
//*                                                                     00004000
// EXEC PGM=MERSOUP,REGION=6M                                           00008000
//STEPLIB DD DSN=SYS1.SDBLOAD,DISP=SHR                                  00010600
//        DD DSN=SDB.LOADLIB,DISP=SHR                                   00010700
//FT01F001 DD DSN=MDB.MERGE.TABLES(SSTSUPOB),DISP=SHR                   00011001
//FT02F001 DD DSN=MDB.SSTSUPOB.OPER.GSST,DISP=SHR                       00020000
//FT11F001 DD DSN=COP.ADDAT.BUOYA.QH06SST,DISP=SHR,LABEL=(,,,IN)        00040000
//FT12F001 DD DSN=COP.ADDAT.BUOYB.QH06SST,DISP=SHR,LABEL=(,,,IN)        00041000
//TABLEB   DD DSN=SDB.BUFR.TABLEB,DISP=SHR,LABEL=(,,,IN)                00070000
//TABLED   DD DSN=SDB.BUFR.TABLED,DISP=SHR,LABEL=(,,,IN)                00080000
//CODEFIG  DD DSN=SDB.BUFR.CODEFIGS,DISP=SHR,LABEL=(,,,IN)              00090000
