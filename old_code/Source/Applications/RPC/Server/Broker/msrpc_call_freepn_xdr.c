/*-------------------------------------------------------------------*
 * Program      : msrpc_call_freepn_xdr.c                                         
 *
 * Language     : C
 *
 * Description  : MetDB RPC freepn server xdr routines produced by
 *              : rpcgen of msrpc_call_freepn.x
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:29    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  10:00:43  10:00:43  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/msrpc_call_freepn_xdr.c,v $
 *-------------------------------------------------------------------*/

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


