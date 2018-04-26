/*-------------------------------------------------------------------*
 * Program      : msrpc_call_broker.h                                         
 *
 * Language     : C
 *
 * Description  : MetDB RPC broker server include file. Produced by
 *              : rpcgen of msrpc_call_broker,x
 *
 * Notes        : The value of MSRPC_CALL_BROKER will be inserted by 
 *              : the build job. This value depends on whether we
 *              : are building the operational or test broker server.
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:27    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2001/01/09  16:47:06  16:47:06  usmdb (Generic MDB account)
 * Change to comments to remove IBM references - S.Cox
 * 
 * Revision 1.1  98/02/03  10:00:00  10:00:00  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/msrpc_call_broker.h,v $
 *-------------------------------------------------------------------*/

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


#define MSRPC_CALL_BROKER ((u_long)XXXXXXXXX)
#define MSRPC_CALL_BROKERV ((u_long)1)
#define MSRPC_CALL_BROKERP ((u_long)1)
extern msrpc_BrokerOut *msrpc_call_brokerp_1();

