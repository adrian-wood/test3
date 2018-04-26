/*----------------------------------------------------------------------------*
 * Program      : ssrpc_call_mdb_clnt.c                                          
 *
 * Language     : C
 *
 * Description  : MetDB single-user RPC client stub produced by rpcgen of
 *              : ssrpc_call_mdb.x
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:21    Kudsia Gwangwaa 
 * $
 * Revision 1.3  2003/03/14 12:48:16  usmdb
 * Corrected DEBUG pre-processor statements - S.Cox
 *
 * Revision 1.2  99/03/15  11:10:55  11:10:55  usmdb (Generic MDB account)
 * 15-03-1999 S.Cox
 * Addition of DEBUG directives. To enable debugging mode,
 * compile source with cc -DDEBUG
 * 
 * Revision 1.1  1998/02/02 16:02:53  usmdb
 * Initial revision
 *
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/ssrpc_call_mdb_clnt.c,v $
 *----------------------------------------------------------------------------*/

#include <rpc/rpc.h>
#include "ssrpc_call_mdb.h"
#include <time.h>

#ifdef hpux

#ifndef NULL
#define NULL  0
#endif  /* NULL */

#endif  /* hpux */

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

ssrpc_op_param *
ssrpc_call_mdbp_1(argp, clnt)
	ssrpc_ip_param *argp;
	CLIENT *clnt;
{
	static ssrpc_op_param res;

#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdbp_1: Start of routine\n");
#endif

	bzero((char *)&res, sizeof(res));

#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdbp_1: About to call clnt_call\n");
#endif

	if (clnt_call(clnt, SSRPC_CALL_MDBP, xdr_ssrpc_ip_param, argp, xdr_ssrpc_op_param, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}

#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdbp_1: Back from clnt_call ", 
                  "RPC_SUCCESS=TRUE\n");
    (void)fprintf(stdout,"ssrpc_call_mdbp_1: About to exit routine\n");
#endif

	return (&res);
}
