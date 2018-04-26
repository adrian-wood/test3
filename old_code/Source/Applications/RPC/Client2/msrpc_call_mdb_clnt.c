#include <rpc/rpc.h>
#include "msrpc_call_mdb.h"

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

msrpc_op_param *
msrpc_call_mdbp_1(argp, clnt)
	msrpc_ip_param *argp;
	CLIENT *clnt;
{
	static msrpc_op_param res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, MSRPC_CALL_MDBP, xdr_msrpc_ip_param, argp, xdr_msrpc_op_param, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


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

