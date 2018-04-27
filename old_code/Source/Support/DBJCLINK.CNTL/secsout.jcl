//T12SNRP1 JOB (M12,SN,WDS0BF),NEEDHAM,PRTY=8,
//  NOTIFY=T12SN,MSGCLASS=Q,TIME=(,5)
//*
//*  Builds secsout load module from assembler using LE version of
//*  of libraries on TSTMDB.PROCLIB
//*  source code is in start team as secsout.asm
//*
//JCL JCLLIB ORDER=MCC3.DB.PROCLIB
//ASM   EXEC ASMACL
//ASM.SYSIN DD DSN=MCC3.DBSRCLIB(SECSOUT),DISP=SHR
//LKED.SYSLMOD DD DSN=MCC3.SNLIB.LOADE(SECSOUT),DISP=SHR,UNIT=DISK,
//        SPACE=
