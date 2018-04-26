//MDBSWEEP JOB (OP1,DB,WDS0BF),DBTEAM.6953,CLASS=I,MSGCLASS=Q,USER=T12DB00001000
//*                                                                     00001500
//*********** To rerun sweep for a previous day *********************** 00001500
//*                                                                     00001500
//* The 24 hours covered by the sweep depend on the current time.       00001500
//* The job is submitted after 23Z on day N to sweep data for day N-1.  00001500
//* This emergency job resets the "current" time to sweep the right day.00001500
//* Set year/month/day in the replacement DATIM below.                  00001500
//* (The data types are as in July 2000 - they may get out of date!)    00001500
//* N.B. Only T12DB can submit the job with this name.                  00001500
//* And MWI & GLOSS/SAT120 have only 60 hours on line, so it may be too 00001500
//* late! If so, delete those steps to avoid restoring empty data sets. 00001500
//*                                                                     00001500
// EXEC FORT2CL,FPARMS='CHARLEN(28000),NOFIPS'
      SUBROUTINE DATIM(NOW) ! to override current date/time
      INTEGER NOW(8)
      NOW(8)=2000           ! YEAR
      NOW(7)=??             ! MONTH
      NOW(6)=?              ! DAY
      NOW(5)=23             ! HOUR
      NOW(4)=10             ! MINUTE
      RETURN
      END
//LKED.DB DD DSN=SDB.LOADLIB,DISP=SHR
 INCLUDE DB(MERGE)
 ENTRY MERGE
//*                                                                     00001500
//MDBSWEEP PROC TYPE=,DATASET= (DATASET=TYPE EXCEPT FOR U/A)            00001602
// EXEC FORT2G,TIME=30,REGION=8M,PARM='SWEEP'                           00002000
//FT01F001 DD DISP=SHR,DSN=MDB.MERGE.TABLES(&TYPE),LABEL=(,,,IN)        00004500
//FT10F001 DD DISP=OLD,DSN=MDB.&TYPE..OPER.GAIR.REQUESTS                00004603
//FT02F001 DD DISP=SHR,DSN=MDB.&DATASET..OPER.GAIR                      00004703
//TABLEB   DD DSN=SDB.BUFR.TABLEB,DISP=SHR                              00004900
//TABLED   DD DSN=SDB.BUFR.TABLED,DISP=SHR                              00005000
//CODEFIG  DD DSN=SDB.BUFR.CODEFIGS,DISP=SHR                            00005100
//SYSUDUMP DD SYSOUT=Q
// PEND                                                                 00006000
//*                                                                     00007000
//AMDARS   EXEC MDBSWEEP,DATASET=AMDARS,TYPE=AMDARS                     00007102
//LNDSYN   EXEC MDBSWEEP,DATASET=LNDSYN,TYPE=LNDSYN                     00008002
//SHPSYN   EXEC MDBSWEEP,DATASET=SHPSYN,TYPE=SHPSYN                     00008102
//TEMP     EXEC MDBSWEEP,DATASET=UPRAIR,TYPE=TEMP                       00010002
//PILOT    EXEC MDBSWEEP,DATASET=UPRAIR,TYPE=PILOT                      00020002
//DROPSOND EXEC MDBSWEEP,DATASET=UPRAIR,TYPE=DROPSOND                   00030002
//AIREPS   EXEC MDBSWEEP,DATASET=AIREPS,TYPE=AIREPS                     00040002
//SATOB    EXEC MDBSWEEP,DATASET=SATOB,TYPE=SATOB                       00090002
//BUOY     EXEC MDBSWEEP,DATASET=BUOY,TYPE=BUOY                         00100002
//GOESAMW  EXEC MDBSWEEP,DATASET=GOESAMW,TYPE=GOESAMW                   00080002
//ESAHRVW  EXEC MDBSWEEP,DATASET=ESAHRVW,TYPE=ESAHRVW                   00080002
//PAOBS    EXEC MDBSWEEP,DATASET=PAOBS,TYPE=PAOBS                       00080002
//WINPRO   EXEC MDBSWEEP,DATASET=WINPRO,TYPE=WINPRO                     00080002
//ATOVSG   EXEC MDBSWEEP,DATASET=ATOVSG,TYPE=ATOVSG                     00080002
//ESACMW   EXEC MDBSWEEP,DATASET=ESACMW,TYPE=ESACMW                     00080002
//ESACSWVW EXEC MDBSWEEP,DATASET=ESACSWVW,TYPE=ESACSWVW                 00080002
//ESAHRWVW EXEC MDBSWEEP,DATASET=ESAHRWVW,TYPE=ESAHRWVW                 00080002
//SSMI     EXEC MDBSWEEP,DATASET=SSMI,TYPE=SSMI                         00080002
//UKMMWI   EXEC MDBSWEEP,DATASET=UKMMWI,TYPE=UKMMWI                     00007202
//GLOSS    EXEC MDBSWEEP,DATASET=GLOSS,TYPE=GLOSS                       00007302
//SAT120   EXEC MDBSWEEP,DATASET=SAT120,TYPE=SAT120                     00080002
