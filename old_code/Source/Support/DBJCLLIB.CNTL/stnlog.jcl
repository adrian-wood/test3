//T12DBNLG JOB (M12,DB,WDS0BF),METDB.X6675,NOTIFY=T12DB,PRTY=8, 
//   MSGCLASS=Q,TIME=1                                                 
//*                                                                    
//*  JOB TO CREATE NEW STATIONMASTER LOG AND                         
//*  COPY SELECTED RECS FROM OLD LOG TO NEW LOG                       
//*                                                                 
//*  Vsn 1.0  Written when old log became full.             (RJ Lavery)
//*                                                                    
//*---------------------------------------------                       
//* Revision info:                                                    
//* $Revision: 1$                                                       
//* $Date: 30/10/2006 08:48:30$                                                       
//* $Author: Stan Kellett$                                                       
//* $Folder: DBJCLLIB.CNTL$                                                     
//* $Workfile: stnlog.jcl$                                                    
//*                                                                 
//* Change Record:                                                  
//* $Log:
//*  1    Met_DB_Project 1.0         30/10/2006 08:48:30    Stan Kellett    
//* $                                                         
//*                                                         
//*---------------------------------------------                 
//* (C) CROWN COPYRIGHT 2006 - MET OFFICE.                       
//* All Rights Reserved.                                      
//*                                                              
//* Met Office, United Kingdom                                  
//*                                                               
//* The use, duplication and disclosure of this                  
//* code is strictly prohibited without the                       
//* permission of the Meteorological Database                  
//* Team at the above address.                                    
//*                                                               
//*---------------------------------------------                   
//*                                                               
//*                                                                
//* Create direct access data set and fill with blank records      
//*                                                                 
//*                                                               
//CRE EXEC PGM=IEBDG                                             
//SYSPRINT DD SYSOUT=N                                            
//DA DD DSN=MCC3.RLSTNMAS.LOG,DISP=(NEW,CATLG),SPACE=(CYL,13),   
//  STORCLAS=SCUSER,MGMTCLAS=MCMDA30,                          
//  DCB=(RECFM=F,LRECL=80,BLKSIZE=80,DSORG=PS)                     
//SYSIN DD *                                                     
         DSD OUTPUT=(DA)                                          
         CREATE QUANTITY=15210,FILL=X'40'                          
         END                                                  
/*                                                                
//*                                                               
//*Copy selected records from old log into new log               
//CPR EXEC PGM=NEWLOG                                            
//STEPLIB DD DSN=MCC3.RLLIB.LOAD,DISP=SHR                         
//FT06F001 DD SYSOUT=Q,DCB=(RECFM=FB,LRECL=151,BLKSIZE=1510)         
//FT13F001 DD DSN=SDB.STNMAS.LOG,DISP=SHR                            
//FT14F001 DD DSN=MCC3.RLSTNMAS.LOG,DISP=OLD                          
//*                                                                   
//* One instruction per card:                                    
//* cols  2- 6 First record to be copied           
//* cols  8-12 Last record to be copied (or blank if copying only one)
//* cols 14-18 Target record to copy to (or blank for same as old log)
//FT05F001 DD *                                                       
     1                                                                 
     2                                                                 
 13902 14452     3                                                      
/*                                                                      
//                                                                      
