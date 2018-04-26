//T12AMMDX JOB (M12,AM,WDS0BF),MONTGOMERY.6953,PRTY=8,MSGLEVEL=(1,1),
//         MSGCLASS=N,REGION=4000K,TIME=(0,10)
//*
//*INFORM PRINTDATA
//*FORMAT PR,DDNAME=,DEST=P741B
//*
//STEPASM EXEC ASMHC
//SYSIN DD DSN=SDB.DBOPSRCE(EFLAGCH),DISP=SHR
//STEPASM EXEC ASMHC
//SYSIN DD DSN=SDB.DBOPSRCE(READCHSR),DISP=SHR
//STEPASM EXEC ASMHC
//SYSIN DD DSN=SDB.DBOPSRCE(DSNALLOC),DISP=SHR
//STEPASM EXEC ASMHC
//SYSIN DD DSN=SDB.DBOPSRCE(DYNALLOC),DISP=SHR
//*
//STEPCL EXEC FORT2CL,
//     FPARMS='NOFIPS,CHARLEN(27998),DC(BLKA,BLKB,INDEX,FORMAT)',
//     LPARMS='MAP,XREF'
//FORT.SYSIN DD DSN=MCC3.DBSRCLIB(NCMISSAM),DISP=SHR
//           DD DSN=MCC3.DBSRCLIB(SYNFLGAM),DISP=SHR
//           DD DSN=MCC3.DBSRCLIB(MSTNUKAM),DISP=SHR
//           DD DSN=MCC3.DBSRCLIB(MDATEAM),DISP=SHR
//           DD DSN=MCC3.DBSRCLIB(MDAILYAM),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(ADDSUB),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(L2SDAT),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(S2LDAT),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(LGOUT),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(SYNABR),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(OBSERV),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(IDENTS),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(CMPDAT),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(FNDKEY),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(HSKPG),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(SUMWIN),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(PUBHOL),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(SPACES),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(HRS24),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(OPNCLM),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(NCM),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(NCMFLG),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(NCMSLH),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(NCMABR),DISP=SHR
//           DD DSN=SDB.DBOPSRCE(SWPSUB),DISP=SHR
//LKED.SYSLMOD DD DSN=MCC3.AMLOAD.TEST(DMQCMSTN),
//     DISP=SHR,SPACE=,UNIT=
//LKED.INC1 DD DSN=MET.PROGLIB,DISP=SHR
//LKED.INC2 DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.INC3 DD DSN=MCC3.GFXNACT.OBJ,DISP=SHR
//LKED.SYSIN DD *
      INCLUDE INC1(VDATES,YEARDY,VNEWMARK,ZPDATE,DATIM,UAMFP)
      INCLUDE INC2(SDB)
      INCLUDE INC3(UTIME)
      ENTRY MDATE
/*
//
