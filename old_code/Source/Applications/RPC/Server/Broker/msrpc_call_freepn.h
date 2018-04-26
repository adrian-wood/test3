/*-------------------------------------------------------------------*
 * Program      : msrpc_call_freepn.h                                        
 *
 * Language     : C
 *
 * Description  : MetDB RPC freepn server include file produced by
 *              : rpcgen of msrpc_call_freepn.x
 *
 * Notes        : The value of MSRPC_CALL_FREEPN will be inserted by 
 *              : the build job. This value depends on whether we
 *              : are building the operational or test freepn server.
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
 * Revision 1.2  2001/01/09  16:47:38  16:47:38  usmdb (Generic MDB account)
 * Change to comments to remove IBM references - S.Cox
 * 
 * Revision 1.1  98/02/03  10:00:30  10:00:30  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/msrpc_call_freepn.h,v $
 *-------------------------------------------------------------------*/

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


#define MSRPC_CALL_FREEPN ((u_long)XXXXXXXXX)
#define MSRPC_CALL_FREEPNV ((u_long)1)
#define MSRPC_CALL_FREEPNP ((u_long)1)
extern freepn_op_param *msrpc_call_freepnp_1();

