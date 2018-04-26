/*---------------------------------------------------------------------------*
 * Program        : ssrpc_call_mdb_xdr.c                                         
 *
 * Language       : C
 *
 * Description    : MetDB RPC single-user xdr routines produced by rpcgen
 *                : of ssrpc_call_mdb.x
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:27:21$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/ssrpc_call_mdb_xdr.c,v $
 * 
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:21    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2000/06/16  11:49:34  11:49:34  usmdb (Generic MDB account)
 * Addition of code to allow transfer of binary data
 * in variable crep_binary - S.Cox
 * 
 * Revision 1.1  98/02/02  16:02:56  16:02:56  usmdb (Generic MDB account)
 * Initial revision
 * 
 * 02-02-1998     : Written - S.Cox
 *
 *---------------------------------------------------------------------------*/

#include <rpc/rpc.h>
#include "ssrpc_call_mdb.h"

bool_t
xdr_ssrpc_ip_param(xdrs, objp)
	XDR *xdrs;
	ssrpc_ip_param *objp;
{
	if (!xdr_string(xdrs, &objp->csubt, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->creq, ~0)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->nobs)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->nelem)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->istat)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->lcsubt)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->lcreq)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->lcstr)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->lcrep)) {
		return (FALSE);
	}
	return (TRUE);
}

bool_t
xdr_ssrpc_op_param(xdrs, objp)
	XDR *xdrs;
	ssrpc_op_param *objp;
{
	if (!xdr_array(xdrs, (char **)&objp->array.array_val, (u_int *)&objp->array.array_len, ~0, sizeof(float), xdr_float)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->nobs)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->istat)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->cstr, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->crep, ~0)) {
		return (FALSE);
	}
/* 1.2 */
	if (!xdr_bytes(xdrs, (char **)&objp->crep_binary.crep_binary_val, (u_int *)&objp->crep_binary.crep_binary_len, ~0)) {
		return (FALSE);
	}
	return (TRUE);
}
