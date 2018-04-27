/*----------------------------------------------------------------------------*
 * Program      : msrpc_call_freepn.h                                         
 *
 * Language     : C
 *
 * Description  : MetDB RPC client freepn include file produced by rpcgen
 *              : of msrpc_call_freepn.x
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:18    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/02  16:02:02  16:02:02  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_freepn.h,v $
 *----------------------------------------------------------------------------*/

#include <rpc/types.h>


struct freepn_ip_param {
	int sv_prognum;
	long TimeStamp;
};
typedef struct freepn_ip_param freepn_ip_param;
bool_t xdr_freepn_ip_param();

struct freepn_op_param {
	int status;
};
typedef struct freepn_op_param freepn_op_param;
bool_t xdr_freepn_op_param();

#define MSRPC_CALL_FREEPN ((u_long)0)
#define MSRPC_CALL_FREEPNV ((u_long)1)
#define MSRPC_CALL_FREEPNP ((u_long)1)
extern freepn_op_param *msrpc_call_freepnp_1();
