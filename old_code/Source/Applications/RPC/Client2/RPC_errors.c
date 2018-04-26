/*----------------------------------------------------------------------------*
 * Program      : RPC_errors.c                                          
 *
 * Language     : C
 *
 * Description  : MetDB RPC error reporting routines
 *
 * Called by    : Various
 *
 * Calls        : clnt_pcreateerror  : RPC error connect routine
 *              : clnt_perror        : RPC error call routine
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
 * Revision 1.3  2000/09/05  11:02:04  11:02:04  usmdb (Generic MDB account)
 * Server name changed. for error reporting routines. Name
 * change for ErrorCantChangeTimeout - S.Cox
 * 
 * Revision 1.2  98/02/10  15:03:50  15:03:50  usmdb (Generic MDB account)
 * Addition of  routines ErrorNoUserTempFile1,
 * ErrorTempFile1Open and removal of routine
 * ErrorNoServerNumber
 * 
 * Revision 1.1  1998/02/02 16:00:23  usmdb
 * Initial revision
 *
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/RPC_errors.c,v $
 *----------------------------------------------------------------------------*/

#include <stdio.h>                  /* for standard C i/o */            
#include <rpc/rpc.h>                /* for all the RPC/XDR calls */
#include "constants.h"              /* RPC constants for MetDB */

ErrorNoUserServerNumber()
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorNoUserServerNumber\n");
  fprintf(stderr,"Could not get RPC program number from environment\n");
  fprintf(stderr,"variable METDB_SERVER_NUMBER.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorNoUserFreepnNumber()
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorNoUserFreepnNumber\n");
  fprintf(stderr,"Could not get RPC program number from environment\n");
  fprintf(stderr,"variable METDB_FREEPN_NUMBER.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorNoUserTicInfo()
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorNoUserTicInfo\n");
  fprintf(stderr,"Could not get RPC Tic Info from environment\n");
  fprintf(stderr,"variable METDB_SERVER_TICINFO.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorNoUserTempFile1()
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - WARNING!\n");
  fprintf(stdout,"info: ErrorNoUserTempFile1\n");
  fprintf(stderr,"Could not get MetDB temporary file information from\n");
  fprintf(stderr,"environment variable METDB_TEMPFILE1.\n");
  fprintf(stdout,"Contact MetDB team for advice\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorTicFileOpen(tic_file)

char *tic_file;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorTicFileOpen\n");
  fprintf(stdout,"Could not open user TIC file. Check that file exists:\n");
  fprintf(stdout,"%s\n",tic_file);
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorTempFile1Open(temp_file1)

char *temp_file1;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - WARNING!\n");
  fprintf(stdout,"info: ErrorTempFile1Open\n");
  fprintf(stdout,"Could not open user temporary file.\n");
  fprintf(stdout,"%s\n",temp_file1);
  fprintf(stdout,"Contact MetDB team for advice\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorBrokerConnect(ProgNum)

int ProgNum;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorBrokerConnect\n");
  fprintf(stdout,"info: Requested ProgNum = %d\n",ProgNum);
  fprintf(stdout,"Could not create client handle for Broker\n");
  fprintf(stdout,"Procedure probably not running on server!\n");
  fprintf(stdout,"Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_pcreateerror(server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorFreepnConnect(ProgNum)

int ProgNum;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErroFreepnConnect\n");
  fprintf(stdout,"info: Requested ProgNum = %d\n",ProgNum);
  fprintf(stdout,"Could not create client handle for Freepn\n");
  fprintf(stdout,"Procedure probably not running on server!\n");
  fprintf(stdout,"Contact MetDB team.\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_pcreateerror(server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorBrokerCall(cl)

CLIENT *cl;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorBrokerCall\n");
  fprintf(stdout,"Error occured calling Broker. There was a failure on\n");
  fprintf(stdout,"the server!\n");
  fprintf(stdout,"Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_perror(cl,server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorFreepnCall(cl)

CLIENT *cl;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorFreepnCall\n");
  fprintf(stdout,"Error occured calling Freepn. There was a failure on\n");
  fprintf(stdout,"the server!\n");
  fprintf(stdout,"Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_perror(cl,server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorFirstNoProgNum()
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stderr,"RPC CAN'T CONTINUE - FATAL!!\n");
  fprintf(stdout,"info: ErrorFirstNoProgNum\n");
  fprintf(stdout,"Program Number PortDetails->ProgNum returned from\n");
  fprintf(stdout,"Remote Procedure INITIAL (RPC broker) = 0. There\n");
  fprintf(stdout,"was a problem assigning a program number for\n");
  fprintf(stdout,"MSRPC_CALL_MDB. There were probably no available program\n");
  fprintf(stdout,"numbers. Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorFirstMainConnect(ProgNum)

int ProgNum;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stderr,"RPC CAN'T CONTINUE - FATAL!!\n");
  fprintf(stdout,"info: ErrorFirstMainConnect\n");
  fprintf(stdout,"info: Requested ProgNum = %d\n",ProgNum);
  fprintf(stdout,"Could not create client handle for Remote Procedure\n");
  fprintf(stdout,"MSRPC_CALL_MDB (RPC main)\n");
  fprintf(stdout,"Procedure probably not executing on mainframe!\n");
  fprintf(stdout,"Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_pcreateerror(server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorNotFirstMainConnectI4(SubCalls,ProgNum)

int SubCalls;
int ProgNum;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorNotFirstMainConnectI4\n");
  fprintf(stdout,"info: SubCalls = %d\n",SubCalls);
  fprintf(stdout,"Requested ProgNum = %d\n",ProgNum);
  fprintf(stdout,"Could not create client handle for Remote Procedure\n");
  fprintf(stdout,"MSRPC_CALL_MDB (RPC main). Procedure probably not executing\n");
  fprintf(stdout,"on mainframe! User calling RPC main with ISTAT=4,\n");
  fprintf(stdout,"and the server procedure has timed-out. Can't\n");
  fprintf(stdout,"continue. Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_pcreateerror(server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorNotFirstInitConnectI0(SubCalls)

int SubCalls;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorNotFirstInitConnectI0\n");
  fprintf(stdout,"info: SubCalls = %d\n",SubCalls);
  fprintf(stdout,"Could not create client handle for Remote Procedure\n");
  fprintf(stdout,"INITIAL (RPC broker). Procedure probably not executing\n");
  fprintf(stdout,"on mainframe! Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_pcreateerror(server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorNotFirstInitCalledI0(cl,SubCalls)

CLIENT *cl;
int    SubCalls;
{
  
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorNotFirstInitCalledI0\n");
  fprintf(stdout,"info: SubCalls = %d\n",SubCalls);
  fprintf(stdout,"Error occured while calling Remote Procedure INITIAL\n");
  fprintf(stdout,"(RPC broker). There was a failure on the mainframe!\n");
  fprintf(stdout,"Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_perror(cl,server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorNotFirstNoProgNum(SubCalls)

int SubCalls;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stderr,"RPC CAN'T CONTINUE - FATAL!!\n");
  fprintf(stdout,"info: ErrorNotFirstNoProgNumI0\n");
  fprintf(stdout,"info: SubCalls = %d\n",SubCalls);
  fprintf(stdout,"Program Number PortDetails->ProgNum returned from\n");
  fprintf(stdout,"Remote Procedure INITIAL (RPC broker) = 0. There\n");
  fprintf(stdout,"was a problem assigning a program number for CALL_MDB\n");
  fprintf(stdout,"(RPC Main). There were probably no available program\n");
  fprintf(stdout,"numbers. Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorNotFirstMainConnectI0(SubCalls,ProgNum)

int SubCalls;
int ProgNum;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stderr,"RPC CAN'T CONTINUE - FATAL!!\n");
  fprintf(stdout,"info: ErrorNotFirstMainConnectI0\n");
  fprintf(stdout,"info: SubCalls = %d\n",SubCalls);
  fprintf(stdout,"info: Requested ProgNum = %d\n",ProgNum);
  fprintf(stdout,"Could not create client handle for Remote Procedure\n");
  fprintf(stdout,"CALL_MDB (RPC main).\n");
  fprintf(stdout,"Procedure probably not executing on mainframe!\n");
  fprintf(stdout,"Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_pcreateerror(server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorCantChangeTimeout(SubCalls)  /* 1.3 */
int SubCalls;
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stderr,"RPC CAN'T CONTINUE - NON FATAL!!\n");
  fprintf(stdout,"info: ErrorCantChangeTimeout\n");
  fprintf(stdout,"info: SubCalls = %d\n",SubCalls);
  fprintf(stdout,"Problem occurred while attempting to changes TIMEOUT\n");
  fprintf(stdout,"from 25 secs to %d secs\n",metdb_timeout);  /* 1.3 */ 
  fprintf(stdout,"Contact MetDB team.\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorMainCall(cl,SubCalls)

CLIENT *cl;
int    SubCalls;
{
  
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - FATAL!\n");
  fprintf(stdout,"info: ErrorMainCall\n");
  fprintf(stdout,"info: SubCalls = %d\n",SubCalls);
  fprintf(stdout,"Error occured while calling Remote Procedure CALL_MDB\n");
  fprintf(stdout,"(RPC main). There was a failure on the mainframe!\n");
  fprintf(stdout,"Contact MetDB team.\n");
  fprintf(stdout,"Returning to user with ISTAT=99\n");
  fprintf(stdout,"------------------------------------------------------\n");
  clnt_perror(cl,server_ipname);  /* 1.3 */
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

ErrorNoClientContact()                      /* Function added with ST2 */
{
  fprintf(stdout,"\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"RPC ERROR OCCURRED - WARNING!\n");
  fprintf(stdout,"info: ErrorNoClientContact\n");
  fprintf(stderr,"Could not get client contact details from environnment\n");
  fprintf(stderr,"variable METDB_CLIENT_CONTACT. See Technote 10.\n");
  fprintf(stdout,"------------------------------------------------------\n");
  fprintf(stdout,"\n");
}

