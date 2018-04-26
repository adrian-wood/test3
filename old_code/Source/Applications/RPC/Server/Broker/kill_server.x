/*-------------------------------------------------------------------*
 * Program      : kill_server.x                                
 *
 * Language     : RPCL
 *
 * Description  : MetDB RPC kill_server protocol for the freepn
 *              : server.  
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:20    Kudsia Gwangwaa 
 * $
# Revision 1.1  98/02/03  09:59:22  09:59:22  usmdb (Generic MDB account)
# Initial revision
# 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/kill_server.x,v $
 *-------------------------------------------------------------------*/

struct msrpc_killsv_ip_param {
   long           TimeStamp;
   long           prognum;
};

program MSRPC_CALL_MDB {
    version MSRPC_CALL_MDBV {
       void KILLSERVERP(msrpc_killsv_ip_param) = 2;
    } = 1 ;
} = 0;
