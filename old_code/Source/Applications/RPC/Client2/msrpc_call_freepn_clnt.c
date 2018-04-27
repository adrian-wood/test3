/*----------------------------------------------------------------------------*
 * Program        : msrpc_call_freepn_clnt,c                                          
 *
 * Language       : C
 *
 * Description    : MetDB RPC client freepn stub produced by rpcgen of
 *                : msrpc_call_freepn.x
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 09/11/2009 10:44:12$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_freepn_clnt.c,v $
 *
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
 * Revision 1.3  2000/06/16  11:38:43  11:38:43  usmdb (Generic MDB account)
 * Addition of call to timtxt to output the time at each
 * fprintf - S.Cox
 * 
 * Revision 1.2  99/07/28  12:14:34  12:14:34  usmdb (Generic MDB account)
 * Addition of debug output. Switched on by global variable debug_level in 
 * constants.h - S.Cox
 * 
 * Revision 1.1  1998/02/02 16:02:16  usmdb
 * Initial revision
 *
 * 02-02-1998   : Written - S.Cox
 *----------------------------------------------------------------------------*/

#include <rpc/rpc.h>
#include <stdio.h>
#include "msrpc_call_freepn.h"
#include "constants.h"
#include <time.h>

#ifdef hpux

#ifndef NULL
#define NULL  0
#endif  /* NULL */

#endif  /* hpux */

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

freepn_op_param *
msrpc_call_freepnp_1(argp, clnt)
	freepn_ip_param *argp;
	CLIENT *clnt;
{
	static freepn_op_param res;

	bzero((char *)&res, sizeof(res));

        if (debug_level > 0) {
          timtxt();
          (void)fprintf(stdout,"msrpc_call_freepnp_1: About to clnt_call " \
                               "freepn server\n");
        }

	if (clnt_call(clnt, MSRPC_CALL_FREEPNP, xdr_freepn_ip_param, argp, xdr_freepn_op_param, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
        if (debug_level > 0) {
          timtxt();
          (void)fprintf(stdout,"msrpc_call_freepnp_1: Back from freepn " \
                               "server. Return\n");
        }
	return (&res);
}
