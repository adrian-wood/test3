/*----------------------------------------------------------------------------*
 * Program       : msrpc_call_mdb_clnt.c                                          
 *
 * Language      : C
 *
 * Description   : MetDB RPC main client code produced by rpcgen of
 *               : msrpc_call_mdb.x 
 *
 * Revision info :
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:27:19$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_mdb_clnt.c,v $
 *
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:19    Kudsia Gwangwaa 
 * $
 * Revision 1.4  2001/03/29 08:48:49  usmdb
 * Only print "call main server" diagnostics if
 * debug_level > 0 - S.Cox
 *
 * Revision 1.3  2000/06/16 11:46:27  usmdb
 * Addition of call to timtxt to output the time at each
 * fprintf - S.Cox
 *
 * Revision 1.2  99/07/28  12:15:17  12:15:17  usmdb (Generic MDB account)
 * Addition of debug output. Switched on by global variable debug_level in 
 * constants.h.
 * 
 * Revision 1.1  1998/02/02 16:02:35  usmdb
 * Initial revision
 *
 * 02-02-1998   : Written - S.Cox
 *----------------------------------------------------------------------------*/

#include <rpc/rpc.h>
#include <stdio.h>
#include "msrpc_call_mdb.h"
#include "constants.h"
#include <time.h>

#ifdef hpux

#ifndef NULL
#define NULL  0
#endif  /* NULL */

#endif  /* hpux */

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

msrpc_op_param *
msrpc_call_mdbp_1(argp, clnt)
	msrpc_ip_param *argp;
	CLIENT *clnt;
{
	static msrpc_op_param res;

	bzero((char *)&res, sizeof(res));

        if (debug_level > 0) {                                /* 1.4 */
          timtxt();
          (void)fprintf(stdout,"msrpc_call_mdbp_1: About to clnt_call " \
                               "main server\n");
        }                                                     /* 1.4 */

	if (clnt_call(clnt, MSRPC_CALL_MDBP, xdr_msrpc_ip_param, argp, xdr_msrpc_op_param, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}

        if (debug_level > 0) {                                /* 1.4 */
          timtxt();
          (void)fprintf(stdout,"msrpc_call_mdbp_1: Back from main server " \
                               "with result.\n");
        }                                                     /* 1.4 */

	return (&res);
}

void *
killserverp_1(argp, clnt)
	msrpc_killsv_ip_param *argp;
	CLIENT *clnt;
{
	static char res;

	bzero((char *)&res, sizeof(res));

        if (debug_level > 0) {
          timtxt();
          (void)fprintf(stdout,"killserverp_1: About to clnt_call " \
                               "main server\n");
        }

	if (clnt_call(clnt, KILLSERVERP, xdr_msrpc_killsv_ip_param, argp, xdr_void, &res, TIMEOUT) != RPC_SUCCESS) {

          if (debug_level > 0) {
            timtxt();
            (void)fprintf(stdout,"killserverp_1: Back from main server. " \
                                 "Return\n");
          }
	  return (NULL);
	}

	return ((void *)&res);
}
