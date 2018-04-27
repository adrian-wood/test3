//*This job accepts as input the name of a dataset and a tapenum.
//*The dataset name is where the output will be written to. You do not
//*have to allocate the dataset before-hand. The tapenum is the vol=ser
//*of the tape that you would like to copy. So if you have 4 files on
//*tape 009900 then the program will generate 4 IEBGENERs and write them
//* to your specified output dataset.
//*
//T12DHCP? JOB (M12,DH,WDS0BF),DICK.METDB.X6955,PRTY=??,MSGCLASS=Q
//*
//GO EXEC COPYJCL,OUTDSN='MCC3.DH36TO36.SATYY.MMM',TAPENUM=9?????
//
