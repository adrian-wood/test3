/*-------------------------------------------------------------------*
 * Program      : kill_server.h                                         
 *
 * Language     : C
 *
 * Description  : MetDB RPC freepn server kill_server include file.
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:19    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  09:59:18  09:59:18  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/kill_server.h,v $
 *-------------------------------------------------------------------*/

struct msrpc_killsv_ip_param {
	long TimeStamp;
	long prognum;
};
typedef struct msrpc_killsv_ip_param msrpc_killsv_ip_param;
bool_t xdr_msrpc_killsv_ip_param();


#define MSRPC_CALL_MDB ((u_long)0)
#define MSRPC_CALL_MDBV ((u_long)1)
#define KILLSERVERP ((u_long)2)
extern void *killserverp_1();

