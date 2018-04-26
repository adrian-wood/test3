/*---------------------------------------------------------------------------*
 * Program        : ssrpc_call_mdb.h                                         
 *
 * Language       : C
 *
 * Description    : MetDB single-user RPC client include file produced by
 *                : rpcgen of ssrpc_call_mdb.x
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:27:20$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/ssrpc_call_mdb.h,v $
 * 
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:20    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2000/06/16  11:47:26  11:47:26  usmdb (Generic MDB account)
 * Addition of structure crep_binary for RPC transfer of
 * binary data - S.Cox
 * 
 * Revision 1.1  98/02/02  16:02:41  16:02:41  usmdb (Generic MDB account)
 * Initial revision
 * 
 * 02-02-1998     : Written - S.Cox
 *
 *---------------------------------------------------------------------------*/

#include <rpc/types.h>


struct ssrpc_ip_param {
	char *csubt;
	char *creq;
	int nobs;
	int nelem;
	int istat;
	int lcsubt;
	int lcreq;
	int lcstr;
	int lcrep;
};
typedef struct ssrpc_ip_param ssrpc_ip_param;
bool_t xdr_ssrpc_ip_param();

struct ssrpc_op_param {
	struct {
		u_int array_len;
		float *array_val;
	} array;
	int nobs;
	int istat;
	char *cstr;
	char *crep;
	struct {                                                      /* 1.2 */
		u_int crep_binary_len;                                /* 1.2 */
		char *crep_binary_val;                                /* 1.2 */
	} crep_binary;                                                /* 1.2 */
};
typedef struct ssrpc_op_param ssrpc_op_param;
bool_t xdr_ssrpc_op_param();

#define SSRPC_CALL_MDB ((u_long)0)
#define SSRPC_CALL_MDBV ((u_long)1)
#define SSRPC_CALL_MDBP ((u_long)1)
extern ssrpc_op_param *ssrpc_call_mdbp_1();
