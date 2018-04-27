/*-------------------------------------------------------------------*
 * Program      : kill_serverp_1  (kill_server_clnt.c)                                         
 *
 * Language     : C
 *
 * Description  : MetDB RPC freepn server client stub produced by
 *              : rpcgen of kill_server.x
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:20    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  09:59:26  09:59:26  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/kill_server_clnt.c,v $
 *-------------------------------------------------------------------*/

#include <rpc/rpc.h>
#include "kill_server.h"

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

void *
killserverp_1(argp, clnt)
	msrpc_killsv_ip_param *argp;
	CLIENT *clnt;
{
	static char res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, KILLSERVERP, xdr_msrpc_killsv_ip_param, argp, xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return ((void *)&res);
}

