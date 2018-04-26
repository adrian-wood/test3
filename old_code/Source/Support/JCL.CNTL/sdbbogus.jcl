//SDBBOGUS JOB (OP1,DB,WDS0BF),METDB.TEAM.X6954,PRTY=8,MSGCLASS=Q,
//  REGION=4M,TIME=(0,15)
//*********************************************************************
//*                                                                   *
//* PROGRAM     : BOGUS                                               *
//*                                                                   *
//* PURPOSE     : TO RUN MDB STORAGE ROUTINES FOR BOGUS DATA TYPE     *
//*                                                                   *
//* DESCRIPTION : JOB SUBMITTED BY HORACE WHEN BOGUS DATA PASSED TO   *
//*               TO IBM. STANDARD MDB STORAGE ROUTINES AND ENBUFR.   *
//*                                                                   *
//* CONTACT     : METDB TEAM (J. LEWTHWAITE) X6954                    *
//*                                                                   *
//* VERSION     : TEST VERSION 26/12/96                               *
//*                                                                   *
//* CHANGES     : --REQUIRED CHANGES FOR OPERATIONAL STATUS INCLUDE   *
//*               STEPLIB DD DSN=SDB.LOADLIB,DISP=SHR                 *
//*               FT09 TO POINT TO TEMP BOGUS DATASET                 *
//*                                                                   *
//* OPERATIONAL : EXPECTED 09/12/96 10:30Z                            *
//*                                                                   *
//* OTHER       :FT10 - MDB STORAGE DATASET DO NOT CHANGE WITHOUT     *
//*              CONSULTING METDB TEAM.                               *
//*                                                                   *
//* 02/12/96    :GO.TABLEB CHANGED TO POINT AT NEW TABLE. THIS        *
//*              NOW INCLUDES THE CORRECT BOGUS TYPE DESCRIPTOR.      *
//*
//*********************************************************************
// EXEC PGM=BOGUS
//STEPLIB DD DSN=SDB.LOADLIB,DISP=SHR
//GO.FT06F001 DD SYSOUT=*
//GO.LOCALSEQ DD DSN=SDB.BUFR.LOCALSEQ(BOGUS),DISP=SHR
//GO.TABLEB DD DSN=SDB.BUFR.TABLEB,DISP=SHR
//GO.TABLED DD DSN=SDB.BUFR.TABLED,DISP=SHR
//GO.CODEFIGS DD DSN=SDB.BUFR.CODEFIGS,DISP=SHR
//GO.FT09F001 DD DSN=TOP.INTVTN.CNEW,DISP=SHR
//GO.FT10F001 DD DSN=SDB.BOGUS,DISP=SHR
//
