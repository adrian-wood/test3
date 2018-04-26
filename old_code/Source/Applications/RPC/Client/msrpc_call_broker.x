/*----------------------------------------------------------------------------*
 * Program      : msrpc_call_broker.x                                          
 *
 * Language     : C
 *
 * Description  : MetDB RPC client broker protocol.
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:17    Kudsia Gwangwaa 
 * $
# Revision 1.1  98/02/02  16:01:31  16:01:31  usmdb (Generic MDB account)
# Initial revision
# 
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_broker.x,v $
 *----------------------------------------------------------------------------*/

struct msrpc_BrokerOut {
  long    ProgNum;
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
} = 0;
              
