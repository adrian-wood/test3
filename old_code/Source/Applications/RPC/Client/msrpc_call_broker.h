/*----------------------------------------------------------------------------*
 * Program      : msrpc_call_broker.h                                          
 *
 * Language     : C
 *
 * Description  : Header file for MetDB multi-user RPC broker client code. 
 *              : Code produced by rpcgen of msrpc_call_broker.x
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:17    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/02  16:01:26  16:01:26  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_broker.h,v $
 *----------------------------------------------------------------------------*/

#include <rpc/types.h>


struct msrpc_BrokerOut {
	long ProgNum;
};
typedef struct msrpc_BrokerOut msrpc_BrokerOut;
bool_t xdr_msrpc_BrokerOut();

struct msrpc_BrokerIn {
	long TimeStamp;
	char *wait;
	char *Branch;
	char *UserId;
	char *Tic;
};
typedef struct msrpc_BrokerIn msrpc_BrokerIn;
bool_t xdr_msrpc_BrokerIn();

#define MSRPC_CALL_BROKER ((u_long)0)
#define MSRPC_CALL_BROKERV ((u_long)1)
#define MSRPC_CALL_BROKERP ((u_long)1)
extern msrpc_BrokerOut *msrpc_call_brokerp_1();
