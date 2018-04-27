/*----------------------------------------------------------------------------*
 * Program      : msrpc_call_freepn.x                                          
 *
 * Language     : C
 *
 * Description  : MetDB RPC freepn RPC protocol. This defines the structures
 *              : used for the RPC call to free a program number.
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
# Revision 1.1  98/02/02  16:02:13  16:02:13  usmdb (Generic MDB account)
# Initial revision
# 
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_freepn.x,v $
 *----------------------------------------------------------------------------*/

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
} = 0;
