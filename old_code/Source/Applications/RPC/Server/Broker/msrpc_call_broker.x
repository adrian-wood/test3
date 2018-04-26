/*-------------------------------------------------------------------*
 * Program      : msrpc_call_broker.x                                     
 *
 * Language     : RPCL
 *
 * Description  : MetDB RPC broker server RPC protocol file. This
 *              : describes the structures passed in MetDB broker RPC
 *
 * Notes        : The value of MSRPC_CALL_BROKER is not set, as
 *              : we want XXXXXXXXX to be coded in the
 *              : msrpc_call_broker.h include file. It will be
 *              : subsituted with a meaningful number by the build 
 *              : job on IBM Open edition. This value depends on 
 *              : whether we are building the operational or test 
 *              : broker server.
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:27    Kudsia Gwangwaa 
 * $
# Revision 1.1  98/02/03  10:00:05  10:00:05  usmdb (Generic MDB account)
# Initial revision
# 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/msrpc_call_broker.x,v $
 *-------------------------------------------------------------------*/

struct msrpc_BrokerOut {
  long          ProgNum;
};

struct msrpc_BrokerIn {
  long    TimeStamp;
  string  wait<>;
  string  Branch<>;
  string  UserId<>;
  string  Tic<>;
};

program MSRPC_CALL_BROKER {
    version MSRPC_CALL_BROKERV {
       msrpc_BrokerOut MSRPC_CALL_BROKERP(msrpc_BrokerIn) = 1;
    } = 1 ;
} = XXXXXXXXX;

