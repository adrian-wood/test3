/*-------------------------------------------------------------------*
 * Program      : kill_server_xdr  (kill_server_xdr.c)                                   
 *
 * Language     : C
 *
 * Description  : MetDB RPC kill_server client xdr routines for the 
 *              : freepn server. This was produced from kill_server.x
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:21    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  09:59:31  09:59:31  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/kill_server_xdr.c,v $
 *-------------------------------------------------------------------*/

#include <rpc/rpc.h>
#include "kill_server.h"


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


