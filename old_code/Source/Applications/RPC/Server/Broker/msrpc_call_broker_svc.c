/*-------------------------------------------------------------------*
 * Program      : msrpc_call_broker_svc.c                                    
 *
 * Language     : C
 *
 * Description  : MetDB RPC broker server stub produced by rpcgen of
 *              : msrpc_call_broker.x
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:27    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  10:00:16  10:00:16  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/msrpc_call_broker_svc.c,v $
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <rpc/rpc.h>
#include "msrpc_call_broker.h"

static void msrpc_call_broker_1();

main()
{
	SVCXPRT *transp;

	(void)pmap_unset(MSRPC_CALL_BROKER, MSRPC_CALL_BROKERV);

	transp = svcudp_create(RPC_ANYSOCK);
	if (transp == NULL) {
		(void)fprintf(stderr, "cannot create udp service.\n");
		exit(1);
	}
	if (!svc_register(transp, MSRPC_CALL_BROKER, MSRPC_CALL_BROKERV, msrpc_call_broker_1, IPPROTO_UDP)) {
		(void)fprintf(stderr, "unable to register (MSRPC_CALL_BROKER, MSRPC_CALL_BROKERV, udp).\n");
		exit(1);
	}

	transp = svctcp_create(RPC_ANYSOCK, 0, 0);
	if (transp == NULL) {
		(void)fprintf(stderr, "cannot create tcp service.\n");
		exit(1);
	}
	if (!svc_register(transp, MSRPC_CALL_BROKER, MSRPC_CALL_BROKERV, msrpc_call_broker_1, IPPROTO_TCP)) {
		(void)fprintf(stderr, "unable to register (MSRPC_CALL_BROKER, MSRPC_CALL_BROKERV, tcp).\n");
		exit(1);
	}
	svc_run();
	(void)fprintf(stderr, "svc_run returned\n");
	exit(1);
}

static void
msrpc_call_broker_1(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	union {
		msrpc_BrokerIn msrpc_call_brokerp_1_arg;
	} argument;
	char *result;
	bool_t (*xdr_argument)(), (*xdr_result)();
	char *(*local)();

	switch (rqstp->rq_proc) {
	case NULLPROC:
		(void)svc_sendreply(transp, xdr_void, (char *)NULL);
		return;

	case MSRPC_CALL_BROKERP:
		xdr_argument = xdr_msrpc_BrokerIn;
		xdr_result = xdr_msrpc_BrokerOut;
		local = (char *(*)()) msrpc_call_brokerp_1;
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

