//T12DBLLG JOB (M12,DB,WDS0BF),METDB.X4431,NOTIFY=T12DB,PRTY=8,        
//          MSGCLASS=Q                                                
//*INFORM PRINTDATA                                                  
//*                                                                   
//*  REMAKE LOAD MODULE STNLOG                                        
//*                                                                    
//****************************************************************      
//* Revision Info:                                                     
//* Original version written by Rosemary Lavery.                        
//*                                                                    
//* $Revision: 1$ $Date: 30/10/2006 11:45:29$                                                 
//* $Author: Stan Kellett$                                                         
//* $Workfile: stnlog.jcl$    
//* $Folder: DBJCLINK.CNTL$
//* 
//* Change details
//* $Log:
//*  1    Met_DB_Project 1.0         30/10/2006 11:45:29    Stan Kellett    
//* $                                                    
//*                                                                     
//****************************************************************      
//*  (C) CROWN COPYRIGHT 2006 - MET OFFICE.                           
//*  All Rights Reserved. n by Rosemary Lavery.                        
//*                                                                  
//*  Met Office, United Kingdom                                      
//*  The use, duplication and disclosure of this                       
//*  code is strictly prohibited without the                        
//*  permission of the Met Office.                                      
//*                                                                     
//****************************************************************      
//*                                                                     
//LOAD EXEC FORT2CL,FPARMS='NOFIPS,OPT(2)'                             
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(STNLOG),DISP=SHR                   
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D######(STNLOG),DISP=OLD,             
// SPACE=,UNIT=                                                        
//                                                                     
