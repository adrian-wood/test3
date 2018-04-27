/*---------------------------------------------------------------------------*
 * Program        : ssrpc_call_mdb.x                                          
 *
 * Language       : C
 *
 * Description    : MetDB RPC protocol for single-user RPC
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:27:21$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/ssrpc_call_mdb.x,v $
 * 
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:21    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2000/06/16  11:47:56  11:47:56  usmdb (Generic MDB account)
 * Addition of opaque crep_binary to allow transfer of
 * binary data - S.Cox
 * 
 * Revision 1.1  98/02/02  16:02:43  16:02:43  usmdb (Generic MDB account)
 * Initial revision
 * 
 * 02-02-1998     : Written - S.Cox
 *
 *---------------------------------------------------------------------------*/

struct ssrpc_ip_param {
   string csubt<>;
   string creq<>;
   int    nobs;
   int    nelem;
   int    istat;
   int    lcsubt;
   int    lcreq;
   int    lcstr;
   int    lcrep;
};
struct ssrpc_op_param {
   float  array<>;
   int    nobs;
   int    istat;
   string cstr<>;
   string crep<>;
   opaque crep_binary<>;                                              /* 1.2 */
};

program SSRPC_CALL_MDB {
    version SSRPC_CALL_MDBV {
       ssrpc_op_param SSRPC_CALL_MDBP(ssrpc_ip_param) = 1;
    } = 1 ;
} = 0;
              
