#include <rpc/rpc.h>
#include "msrpc_call_mdb.h"


bool_t
xdr_msrpc_ip_param(xdrs, objp)
	XDR *xdrs;
	msrpc_ip_param *objp;
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
	if (!xdr_long(xdrs, &objp->prognum)) {
		return (FALSE);
	}
	if (!xdr_long(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->userid, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->client_ip, ~0)) {
		return (FALSE);
	}
	if (!xdr_string(xdrs, &objp->contact, ~0)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_msrpc_op_param(xdrs, objp)
	XDR *xdrs;
	msrpc_op_param *objp;
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
	if (!xdr_bytes(xdrs, (char **)&objp->crep_binary.crep_binary_val, (u_int *)&objp->crep_binary.crep_binary_len, ~0)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_msrpc_killsv_ip_param(xdrs, objp)
	XDR *xdrs;
	msrpc_killsv_ip_param *objp;
{
	if (!xdr_long(xdrs, &objp->TimeStamp)) {
		return (FALSE);
	}
	if (!xdr_long(xdrs, &objp->prognum)) {
		return (FALSE);
	}
	return (TRUE);
}


