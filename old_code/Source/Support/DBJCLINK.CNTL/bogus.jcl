//T12SCBOG JOB (M12,SC,WDS0BF),METDB.TEAM.6955,PRTY=8,MSGCLASS=Q
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//* TITLE         : BOGUS
//*
//* PURPOSE       : To rebuild BOGUS storage application from source.
//*                  (MDB storage)
//*
//* INTRODUCED    : July 1997 by Jim Arnott (CC3)
//*
//* CHANGE RECORD :
//* 13/03/00: include INDLALO (C Long)
//* 17/04/00: rename all BOG* programs (C Long)
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//A1 EXEC ASMAC
//ASM.SYSIN DD DSN=MDB.SOURCE(ABEND),DISP=SHR
//*
//STEP1 EXEC FORT2C
      SUBROUTINE ICOBRV     ! called by TAFREP, but only for METARs,
      RETURN                ! so dummy it here to save space.
      END
//*
//S1 EXEC FORT2CL,FPARMS='CHARLEN(28000),NOFIPS,DC(*)',
//        LMAP=MAP,LXREF=XREF
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(BOGUS),DISP=SHR
//           DD DSN=MDB.SOURCE(BOGEXP),DISP=SHR
//           DD DSN=MDB.SOURCE(BOGIND),DISP=SHR
//           DD DSN=MDB.SOURCE(TAFREP),DISP=SHR
//           DD DSN=MDB.SOURCE(UADUPS),DISP=SHR
//           DD DSN=MDB.SOURCE(INDLALO),DISP=SHR
//           DD DSN=MDB.SOURCE(CENTURY),DISP=SHR
//           DD DSN=MDB.SOURCE(SORTCH),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(BOGUS),DISP=OLD,SPACE=,UNIT=
//*
//SDBLD DD DSN=SDB.LOADLIB,DISP=SHR
//METPL DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN DD *
   INCLUDE SDBLD(BUFRSHEL,MDB)
   INCLUDE METPL(ZPDATE)
   ENTRY BOGUS
/*
//
