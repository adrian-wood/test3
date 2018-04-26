/*----------------------------------------------------------------------------*
 * Program      : msrpc_call_freepn_xdr.c                                          
 *
 * Language     : C
 *
 * Description  : MetDB RPC client side xdr routines produced by rpcgen of
 *              : msrpc_call_freepn.x
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
 * Revision 1.1  98/02/02  16:02:19  16:02:19  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_freepn_xdr.c,v $
 *----------------------------------------------------------------------------*/

#include <rpc/rpc.h>
#include "msrpc_call_freepn.h"

bool_t
xdr_freepn_ip_param(xdrs, objp)
	XDR *xdrs;
	freepn_ip_param *objp;
{
	if (!xdr_int(xdrs, &objp->sv_prognum)) {
		return (FALSE);
	}
	if (!xdr_long(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_freepn_op_param(xdrs, objp)
	XDR *xdrs;
	freepn_op_param *objp;
{
	if (!xdr_int(xdrs, &objp->status)) {
		return (FALSE);
	}
	return (TRUE);
}
