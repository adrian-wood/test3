*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
*Y2K  26.06.1997  DMLINK IS YEAR 2000 COMPLIANT.
*                                
*$Log:
* 1    Met_DB_Project 1.0         30/01/2006 20:22:05    Sheila Needham  
*$
*Revision 1.1  1997/08/20 08:29:48  uspm
*Initial revision
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
      SUBROUTINE DMLINK                                                         
*                                                                               
      INCLUDE(STNINIT)
      CHARACTER*132 HEAD
      
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/dmlink.F,v $
     &'//'$ $Date: 30/01/2006 20:22:05$ $Revision: 1$'
*                                                                               
* LINK ISPF PANEL VARIABLES TO FORTRAN VARIABLES                        00087000
*                                                                               
      LASTRC=ISPLNK('VDEFINE','ACTION ',ACTION,'CHAR ',1)               00088 00
      LASTRC=ISPLNK('VDEFINE','A ',A,'CHAR ',1)                         00088100
      LASTRC=ISPLNK('VDEFINE','CDENT ',CDENT,'CHAR ',6)                 00088100
      LASTRC=ISPLNK('VDEFINE','CURR ',CURR,'CHAR ',1)                   00088100
      LASTRC=ISPLNK('VDEFINE','BLKSTN ',BLKSTN,'FIXED ',4)              00088100
      LASTRC=ISPLNK('VDEFINE','ICAO ',ICAO,'CHAR ',4)                   00088100
      LASTRC=ISPLNK('VDEFINE','DCNN ',DCNN,'FIXED ',4)                  00088100
      LASTRC=ISPLNK('VDEFINE','RAIN ',RAIN,'FIXED',4)                   00088100
      LASTRC=ISPLNK('VDEFINE','LAT1 ',LAT1,'FIXED',4)                   00088100
      LASTRC=ISPLNK('VDEFINE','LAT2 ',LAT2,'FIXED',4)                   00088100
      LASTRC=ISPLNK('VDEFINE','LATP ',LATP,'CHAR',1)                    00088100
      LASTRC=ISPLNK('VDEFINE','LONG1 ',LONG1,'FIXED',4)                 00088100
      LASTRC=ISPLNK('VDEFINE','LONG2 ',LONG2,'FIXED',4)                 00088100
      LASTRC=ISPLNK('VDEFINE','LONGP ',LONGP,'CHAR',1)                  00088100
      LASTRC=ISPLNK('VDEFINE','HT1 ',HT1,'FIXED',4)                     00088100
      LASTRC=ISPLNK('VDEFINE','HT2 ',HT2,'FIXED',4)                     00088100
      LASTRC=ISPLNK('VDEFINE','STATION ',STATION,'CHAR',42)             00088100
      LASTRC=ISPLNK('VDEFINE','COUNTRY ',COUNTRY,'CHAR',32)             00088100
      LASTRC=ISPLNK('VDEFINE','REGION ',REGION,'FIXED',4)               00088100
      LASTRC=ISPLNK('VDEFINE','OYEAR ',OYEAR,'FIXED',4)                 00088100
      LASTRC=ISPLNK('VDEFINE','OMONTH ',OMONTH,'FIXED',4)               00088100
      LASTRC=ISPLNK('VDEFINE','ODAY ',ODAY,'FIXED',4)                   00088100
      LASTRC=ISPLNK('VDEFINE','OHOUR ',OHOUR,'FIXED',4)                 00088100
      LASTRC=ISPLNK('VDEFINE','CYEAR ',CYEAR,'FIXED',4)                 00088100
      LASTRC=ISPLNK('VDEFINE','CMONTH ',CMONTH,'FIXED',4)               00088100
      LASTRC=ISPLNK('VDEFINE','CDAY ',CDAY,'FIXED',4)                   00088100
      LASTRC=ISPLNK('VDEFINE','CHOUR ',CHOUR,'FIXED',4)                 00088100
*                                                                               
      LASTRC=ISPLNK('VDEFINE','F1 ',F1,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','F2 ',F2,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','F3 ',F3,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','F4 ',F4,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','F5 ',F5,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','F6 ',F6,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','F7 ',F7,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','F8 ',F8,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','F9 ',F9,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','F10 ',F10,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F11 ',F11,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F12 ',F12,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F13 ',F13,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F14 ',F14,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F15 ',F15,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F16 ',F16,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F17 ',F17,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F18 ',F18,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F19 ',F19,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F20 ',F20,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F21 ',F21,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F22 ',F22,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F23 ',F23,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','F24 ',F24,'CHAR',1)                      00088100
      LASTRC=ISPLNK('VDEFINE','CLASS ',CLASS,'FIXED',4)                 00088100
      LASTRC=ISPLNK('VDEFINE','HAW00 ',HAW00,'FIXED',4)                 00088100
      LASTRC=ISPLNK('VDEFINE','HAW12 ',HAW12,'FIXED',4)                 00088100
      LASTRC=ISPLNK('VDEFINE','MAP ',MAP,'FIXED',4)                     00088100
      LASTRC=ISPLNK('VDEFINE','RUN1 ',RUN1,'FIXED',4)                   00088100
      LASTRC=ISPLNK('VDEFINE','RUN2 ',RUN2,'FIXED',4)                   00088100
      LASTRC=ISPLNK('VDEFINE','OREC ',OREC,'FIXED',4)                   00088100
      LASTRC=ISPLNK('VDEFINE','NREC ',NREC,'FIXED',4)                   00088100
*                                                                               
      LASTRC=ISPLNK('VDEFINE','UA ',UA,'CHAR',4)                        00088100
      LASTRC=ISPLNK('VDEFINE','SYN ',SYN,'CHAR',13)                     00088100
      LASTRC=ISPLNK('VDEFINE','NCM ',NCM,'CHAR',23)                     00088100
      LASTRC=ISPLNK('VDEFINE','NUM ',NUM,'FIXED',4)                     00088100
      LASTRC=ISPLNK('VDEFINE','DAY1 ',DAY1,'CHAR',9)                    00088100
      LASTRC=ISPLNK('VDEFINE','DAY2 ',DAY2,'CHAR',9)                    00088100
      LASTRC=ISPLNK('VDEFINE','DAY3 ',DAY3,'CHAR',9)                    00088100
      LASTRC=ISPLNK('VDEFINE','DAY4 ',DAY4,'CHAR',9)                    00088100
      LASTRC=ISPLNK('VDEFINE','DAY5 ',DAY5,'CHAR',9)                    00088100
      LASTRC=ISPLNK('VDEFINE','DAY6 ',DAY6,'CHAR',9)                    00088100
      LASTRC=ISPLNK('VDEFINE','DAY7 ',DAY7,'CHAR',9)                    00088100
      LASTRC=ISPLNK('VDEFINE','DAY8 ',DAY8,'CHAR',9)                    00088100
      LASTRC=ISPLNK('VDEFINE','DAY9 ',DAY9,'CHAR',9)                    00088100
      LASTRC=ISPLNK('VDEFINE','DAY10 ',DAY10,'CHAR',9)                  00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM1 ',SYNTIM1,'CHAR',24)             00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM2 ',SYNTIM2,'CHAR',24)             00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM3 ',SYNTIM3,'CHAR',24)             00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM4 ',SYNTIM4,'CHAR',24)             00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM5 ',SYNTIM5,'CHAR',24)             00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM6 ',SYNTIM6,'CHAR',24)             00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM7 ',SYNTIM7,'CHAR',24)             00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM8 ',SYNTIM8,'CHAR',24)             00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM9 ',SYNTIM9,'CHAR',24)             00088100
      LASTRC=ISPLNK('VDEFINE','SYNTIM10 ',SYNTIM10,'CHAR',24)           00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1A ',NCMTIM1A,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1B ',NCMTIM1B,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1C ',NCMTIM1C,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1D ',NCMTIM1D,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1E ',NCMTIM1E,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1F ',NCMTIM1F,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1G ',NCMTIM1G,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1H ',NCMTIM1H,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1I ',NCMTIM1I,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM1J ',NCMTIM1J,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2A ',NCMTIM2A,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2B ',NCMTIM2B,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2C ',NCMTIM2C,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2D ',NCMTIM2D,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2E ',NCMTIM2E,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2F ',NCMTIM2F,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2G ',NCMTIM2G,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2H ',NCMTIM2H,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2I ',NCMTIM2I,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','NCMTIM2J ',NCMTIM2J,'CHAR',3)            00088100
      LASTRC=ISPLNK('VDEFINE','SQUARE ',SQUARE,'FIXED',4)               00088100
      LASTRC=ISPLNK('VDEFINE','CHASER ',CHASER,'CHAR',6)                00088100
      LASTRC=ISPLNK('VDEFINE','RECNO ',RECNO,'FIXED',4)                 00088100
      LASTRC=ISPLNK('VDEFINE','V ',V,'CHAR',1)                          00088100
      LASTRC=ISPLNK('VDEFINE','W ',W,'CHAR',1)                          00088100
      LASTRC=ISPLNK('VDEFINE','X ',X,'CHAR',1)                          00088100
      LASTRC=ISPLNK('VDEFINE','Y ',Y,'CHAR',1)                          00088100
      LASTRC=ISPLNK('VDEFINE','UP ',UP,'CHAR',1)                        00088100
      LASTRC=ISPLNK('VDEFINE','SOURCE ',SOURCE,'CHAR',20)               00088100
      LASTRC=ISPLNK('VDEFINE','AUSER ',AUSER,'CHAR',20)                 00088100
      LASTRC=ISPLNK('VDEFINE','COYEAR ',COYEAR,'CHAR',2)                00088100
      LASTRC=ISPLNK('VDEFINE','COMONTH ',COMONTH,'CHAR',2)              00088100
      LASTRC=ISPLNK('VDEFINE','CODAY ',CODAY,'CHAR',2)                  00088100
      LASTRC=ISPLNK('VDEFINE','COHOUR ',COHOUR,'CHAR',2)                00088100
      LASTRC=ISPLNK('VDEFINE','CCYEAR ',CCYEAR,'CHAR',2)                00088100
      LASTRC=ISPLNK('VDEFINE','CCMONTH ',CCMONTH,'CHAR',2)              00088100
      LASTRC=ISPLNK('VDEFINE','CCDAY ',CCDAY,'CHAR',2)                  00088100
      LASTRC=ISPLNK('VDEFINE','CCHOUR ',CCHOUR,'CHAR',2)                00088100
      LASTRC=ISPLNK('VDEFINE','CPANEL ',CPANEL,'CHAR',8)                00088100
*                                                                               
      RETURN                                                                    
      END                                                               00110000
