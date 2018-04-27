//M12JASUB JOB (M12,JA,WDS0BF),METDB.JIMA.6953,PRTY=??,
//     MSGCLASS=Q,MSGLEVEL=(1,1),NOTIFY=T12JA
//*
//*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
//*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
//*
//* Description:
//*     Execute clist in batch to complete skeleton JCL for automatic
//*   check on data in MetDB for Yesterday.
//*
//* Method:
//*     Set up ISPF environment using a current CC3 userId.
//*
//*     Refer to documentation of the "MDBCHECK" system.  At present the
//*     Fortran program handles only on-line requests; it has to be
//*     changed to select sub-types for listing according to "AUTO"
//*     setting in the first line in control dataset.
//*
//*     This system has not been tested !  If it is, eventually, devel-
//*     oped, remember to test it using a UserId other that your own as
//*     this job will not run while you are still logged on !  It sets
//*     up a pukka TSO session and you can't have two sessions !
//*
//* Code owner:  Jim Arnott.
//*
//* History:
//* Version     Date     Comment
//*
//*  1.0     05/08/1997  Original
//*
//*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
//*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *
//*
//ISPFTSO EXEC PGM=IKJEFT01,REGION=5M,TIME=1,DYNAMNBR=25
//*
//ISPPLIB  DD DSN=SYS1.ISPPLIB,DISP=SHR        System Panel lib
//ISPMLIB  DD DSN=SYS1.ISPMLIB,DISP=SHR        System Messages lib
//ISPSLIB  DD DSN=SYS1.ISPSLIB,DISP=SHR        System Skeleton lib
//         DD DSN=MCC3.DB.SKELS,DISP=SHR       User Skeleton lib
//*                                           (Also defined in Clist)
//ISPTLIB  DD DSN=SYS1.ISPTLIB,DISP=SHR        System Input Table lib
//ISPLLIB  DD DSN=SDB.LOADLIB,DISP=SHR         ISPF Load lib
//*
//SYSPROC  DD DSN=MCC3.DB.CLIST,DISP=SHR       Branch Clist lib
//*
//ISPLOG   DD SYSOUT=Q,DCB=(RECFM=FB,LRECL=133,BLKSIZE=1330)
//SYSTSPRT DD SYSOUT=Q,DCB=(RECFM=FB,LRECL=133,BLKSIZE=1330)
//*
//FT06F001 DD SYSOUT=Q
//FT05F001 DD DSN=&&DATEIN,UNIT=SYSDA,SPACE=(TRK,1),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//FT07F001 DD DSN=&&DATEUT,UNIT=SYSDA,SPACE=(TRK,1),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//*
//* Define user-specific environment and execute clist.
//*
//ISPPROF DD DSN=TSO.T12JA.PROF42,DISP=OLD    TSO UserId profile
//*
//* ISPSTART command has default Prty (5) Mode and batch UserId.
//*
//SYSTSIN  DD *
PROFILE PREFIX(T01MF)
ISPSTART CMD(MDBCHECK 5 MODE(BATCH) USER(T12JA))
/*
//
