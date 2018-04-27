/*---------------------------------------------------------------------------*
 * Program        : msrpc_call_mdb.x                                         
 *
 * Language       : C
 *
 * Description    : MetDB RPC client protocol for main RPC. This defines
 *                : the structures passed in MetDB RPC.
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:27:19$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_mdb.x,v $
 *
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:19    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2000/06/16  11:43:23  11:43:23  usmdb (Generic MDB account)
 * Addition of opaque crep_binary to allow transfer of
 * binary data - S.Cox
 * 
 * Revision 1.1  98/02/02  16:02:24  16:02:24  usmdb (Generic MDB account)
 * Initial revision
 *
 * 02-02-1998     : Written - S.Cox
 * 
 *---------------------------------------------------------------------------*/

struct msrpc_ip_param {
   string         csubt<>;
   string         creq<>;
   int            nobs;
   int            nelem;
   int            istat;
   int            lcsubt;
   int            lcreq;
   int            lcstr;
   int            lcrep;
   long           prognum;
   long           TimeStamp;
};
struct msrpc_op_param {
   float          array<>;
   int            nobs;
   int            istat;
   string         cstr<>;
   string         crep<>;
   opaque         crep_binary<>;                                      /* 1.2 */
};

struct msrpc_killsv_ip_param {
   long           TimeStamp;
   long           prognum;
};

program MSRPC_CALL_MDB {
    version MSRPC_CALL_MDBV {
       msrpc_op_param MSRPC_CALL_MDBP(msrpc_ip_param) = 1;
       void KILLSERVERP(msrpc_killsv_ip_param) = 2;
    } = 1 ;
} = 0;
