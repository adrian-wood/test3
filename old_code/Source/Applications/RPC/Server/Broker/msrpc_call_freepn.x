/*-------------------------------------------------------------------*
 * Program      : msrpc_call_freepn                                        
 *
 * Language     : RPCL
 *
 * Description  : MetDB RPC freepn server protocol. 
 *
 * Notes        : The value of MSRPC_CALL_FREEPN is not set, as
 *              : we want XXXXXXXXX to be coded in the
 *              : msrpc_call_freepn.h include file. It will be
 *              : subsituted with a meaningful number by the build 
 *              : job on IBM Open edition. This value depends on 
 *              : whether we are building the operational or test 
 *              : broker server.
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:28    Kudsia Gwangwaa 
 * $
# Revision 1.1  98/02/03  10:00:34  10:00:34  usmdb (Generic MDB account)
# Initial revision
# 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/msrpc_call_freepn.x,v $
 *-------------------------------------------------------------------*/

struct freepn_ip_param {
   int            sv_prognum;
   long           TimeStamp;
};

struct freepn_op_param {
   int            status;
};

program MSRPC_CALL_FREEPN {
    version MSRPC_CALL_FREEPNV {
       freepn_op_param MSRPC_CALL_FREEPNP(freepn_ip_param) = 1;
    } = 1 ;
} = XXXXXXXXX;
