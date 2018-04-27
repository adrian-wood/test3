//MDBDBRAC JOB (M00,DB,WDS0BF),METDB.TEAM.X6954,PRTY=10,
//  MSGCLASS=Q,TIME=(0,30),REGION=6M,MSGLEVEL=(1,1)
//JCL JCLLIB ORDER=(DSM.STORAGE.JCL)
//*
//*This job re-runs the actual archive step. If FDR has failed say,
//*because of a bad cart. but the dataset is on disk then run this job.
//*Be warned that the disk dataset will be scratched after a day or two
//*and if its gone you'll have to re-run the whole archive process for
//*whatever datatype.
//*
//*Enter the correct management class (given below) and the dataset
//*name in the format MDB.DAdatatype.Dyymmdd
//*
//*MGMTCLASS
//*For SYNOPTIC type data use MCMDBOB5 (5 Year retention)
//*For SAT. Data use          MCMDBST3 (3 Year retention)
//*For  TAFS&METARS use       MCMDBTF5 (5 year retention TAFS/METARS)
//*For SFLOC & SPHERIC use    MCMDB10Y (10 Year retention)
//*Perm                       MCMDB99  (Perm retention)
//DBRARC EXEC BACKUP,MC=MCMDBST3,SCRATCH=S
//BACKUP DD *
  SELECT CATDSN=MDB.DA?????.Dyymmdd
//
