/*-------------------------------------------------------------------*
 * Program      : msrpc_call_broker_xdr.c                                         
 *
 * Language     : C
 *
 * Description  : MetDB RPC broker server xdr routines produced by
 *              : rpcgen of msrpc_call_broker.x
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
 * Revision 1.1  98/02/03  10:00:20  10:00:20  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/msrpc_call_broker_xdr.c,v $
 *-------------------------------------------------------------------*/

#include <rpc/rpc.h>
#include "msrpc_call_broker.h"


bool_t
xdr_msrpc_BrokerOut(xdrs, objp)
	XDR *xdrs;
	msrpc_BrokerOut *objp;
{
	if (!xdr_long(xdrs, &objp->ProgNum)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_msrpc_BrokerIn(xdrs, objp)
	XDR *xdrs;
	msrpc_BrokerIn *objp;
{
	if (!xdr_long(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->wait, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->Branch, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->UserId, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->Tic, ~0)) {
		return (FALSE);
	}
	return (TRUE);
}


