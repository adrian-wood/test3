/*----------------------------------------------------------------------------*
 * Program        : msrpc_call_brokerp_1  (msrpc_call_broker_clnt.c)                                         
 *
 * Language       : C
 *
 * Description    : MetDB multi-user RPC client broker client stub. Produced 
 *                : by rpcgen of msrpc_call_broker.x 
 *
 * Called by      : msrpc_call_broker
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 09/11/2009 10:44:12$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_broker_clnt.c,v $
 *
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
 * Revision 1.3  2000/06/16  11:37:45  11:37:45  usmdb (Generic MDB account)
 * Addition of call to timtxt to output the time at each
 * fprintf - S.Cox
 * 
 * Revision 1.2  99/07/28  12:13:54  12:13:54  usmdb (Generic MDB account)
 * Addition of debug output. Switched on by global variable debug_level in 
 * constants.h - S.Cox
 * 
 * Revision 1.1  1998/02/02 16:01:52  usmdb
 * Initial revision
 *
 * 02-02-1998   : Written - S.Cox
 *----------------------------------------------------------------------------*/

#include <rpc/rpc.h>
#include <stdio.h>
#include "msrpc_call_broker.h"
#include "constants.h"
#include <time.h>

#ifdef hpux

#ifndef NULL
#define NULL  0
#endif  /* NULL */

#endif  /* hpux */

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

msrpc_BrokerOut *
msrpc_call_brokerp_1(argp, clnt)
	msrpc_BrokerIn *argp;
	CLIENT *clnt;
{
	static msrpc_BrokerOut res;

	bzero((char *)&res, sizeof(res));

        if (debug_level > 0) {
          timtxt();
          (void)fprintf(stdout,"msrpc_call_brokerp_1: About to clnt_call " \
                               "broker server\n");
        }

	if (clnt_call(clnt, MSRPC_CALL_BROKERP, xdr_msrpc_BrokerIn, argp, xdr_msrpc_BrokerOut, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
        if (debug_level > 0) {
          timtxt();
          (void)fprintf(stdout,"msrpc_call_brokerp_1: Back from broker " \
                               "server with prognum. Return\n");
        }
	return (&res);
}
