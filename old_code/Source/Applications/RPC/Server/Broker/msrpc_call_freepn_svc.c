/*-------------------------------------------------------------------*
 * Program      : msrpc_call_freepn_svc.c                                       
 *
 * Language     : C
 *
 * Description  : MetDB RPC freepn server stub produced by rpcgen
 *              : of msrpc_call_freepn.x
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:28    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  10:00:39  10:00:39  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/msrpc_call_freepn_svc.c,v $
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <rpc/rpc.h>
#include "msrpc_call_freepn.h"

static void msrpc_call_freepn_1();

main()
{
	SVCXPRT *transp;

	(void)pmap_unset(MSRPC_CALL_FREEPN, MSRPC_CALL_FREEPNV);

	transp = svcudp_create(RPC_ANYSOCK);
	if (transp == NULL) {
		(void)fprintf(stderr, "cannot create udp service.\n");
		exit(1);
	}
	if (!svc_register(transp, MSRPC_CALL_FREEPN, MSRPC_CALL_FREEPNV, msrpc_call_freepn_1, IPPROTO_UDP)) {
		(void)fprintf(stderr, "unable to register (MSRPC_CALL_FREEPN, MSRPC_CALL_FREEPNV, udp).\n");
		exit(1);
	}

	transp = svctcp_create(RPC_ANYSOCK, 0, 0);
	if (transp == NULL) {
		(void)fprintf(stderr, "cannot create tcp service.\n");
		exit(1);
	}
	if (!svc_register(transp, MSRPC_CALL_FREEPN, MSRPC_CALL_FREEPNV, msrpc_call_freepn_1, IPPROTO_TCP)) {
		(void)fprintf(stderr, "unable to register (MSRPC_CALL_FREEPN, MSRPC_CALL_FREEPNV, tcp).\n");
		exit(1);
	}
	svc_run();
	(void)fprintf(stderr, "svc_run returned\n");
	exit(1);
}

static void
msrpc_call_freepn_1(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	union {
		freepn_ip_param msrpc_call_freepnp_1_arg;
	} argument;
	char *result;
	bool_t (*xdr_argument)(), (*xdr_result)();
	char *(*local)();

	switch (rqstp->rq_proc) {
	case NULLPROC:
		(void)svc_sendreply(transp, xdr_void, (char *)NULL);
		return;

	case MSRPC_CALL_FREEPNP:
		xdr_argument = xdr_freepn_ip_param;
		xdr_result = xdr_freepn_op_param;
		local = (char *(*)()) msrpc_call_freepnp_1;
		break;

	default:
		svcerr_noproc(transp);
		return;
	}
	bzero((char *)&argument, sizeof(argument));
	if (!svc_getargs(transp, xdr_argument, &argument)) {
		svcerr_decode(transp);
		return;
	}
	result = (*local)(&argument, rqstp);
	if (result != NULL && !svc_sendreply(transp, xdr_result, result)) {
		svcerr_systemerr(transp);
	}
	if (!svc_freeargs(transp, xdr_argument, &argument)) {
		(void)fprintf(stderr, "unable to free arguments\n");
		exit(1);
	}
}

