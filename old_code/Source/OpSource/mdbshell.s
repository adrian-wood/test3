***********************************************************************         
*                                                                     *         
* PROGRAM       : MDB  (SHELL FOR MET. DATABASE RETRIEVAL CALL)       *         
*                                                                     *         
* PURPOSE       : TO LOAD & CALL THE GENERAL RETRIEVAL INTERFACE      *         
*                                                                     *         
* CALLED BY     : USER WITH RETRIEVAL REQUEST                         *         
*                                                                     *         
* CALLS         : MDB                                                 *         
*                                                                     *         
* PARAMETERS    : RETRIEVAL PARAMETERS, PASSED ON TO LOADED MODULE    *         
*                                                                     *         
*Y2K  26.06.1997  MDBSHELL IS YEAR 2000 COMPLIANT.                              
*                                                                     *         
* CHANGE RECORD :                                                     *         
* 12/12/90     LOAD AND CALL SEPARATE MODULE TO DELETE LOAD MODULES  A*         
* 08/01/96     LOAD MDB RATHER THAN MDBRT (JUST A CHANGE OF NAME!),  B*         
*              REMOVE MDBDEL CALL (NOTHING WAS DELETED ANYWAY!)      B*         
*              & REMOVE MODULE LIST AT END (LEFT BEHIND IN DEC 90!)  B*         
*                                                                     *
*$Log:
* 1    Met_DB_Project 1.0         28/02/2006 10:25:38    Sheila Needham  
*$
*Revision 1.1  1997/08/05 15:16:42  uspm
*Initial revision
*
*         
***********************************************************************         
MDB      CSECT                                                                  
         PRINT NOGEN                                                            
         EQUREG R                                                               
         CHAIN ,,*                                                              
         LR    R2,R1            SAVE ARGUMENT LIST                              
*                                                                               
         L     R15,MDBLOAD      ADDRESS OF LOADED MODULE (OR ZERO)              
         LTR   R15,R15          HAS RETRIEVAL MODULE BEEN LOADED ?              
         BNZ   CALLMDB          IF SO, JUST CALL IT.                            
*                                                                               
         LOAD  EP=MDB           LOAD THE MODULE,                      B         
         ST    R0,MDBLOAD       KEEP ITS ADDRESS FOR NEXT TIME                  
         LR    R15,R0           & PUT IT IN REGISTER USED BY CALL.              
*                                                                               
CALLMDB  LR    R1,R2            RESTORE THE ADDRESS OF THE ARGUMENTS            
         CALL  (15)             & CALL MDB                                      
         RETURN13 RC=0                                                          
*                                                                               
MDBLOAD  DC    F'0'             ADR OF ENTRY POINT WHEN LOADED (MDB)            
         DC    C'$Date: 28/02/2006 10:25:38$ $Revision: 1$'  
         END                                                                    
