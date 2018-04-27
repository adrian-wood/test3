/*---------------------------------------------------------------------------*
 * Program        : msrpc_call_mdb.h                                         
 *
 * Language       : C
 *
 * Description    : MetDB RPC main client include file produced by rpcgen
 *                : of msrpc_call_mdb.x
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:27:19$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_mdb.h,v $
 * 
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:19    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2000/06/16  11:39:26  11:39:26  usmdb (Generic MDB account)
 * Addition of structure crep_binary for RPC transfer of
 * binary data - S.Cox
 * 
 * Revision 1.1  98/02/02  16:02:22  16:02:22  usmdb (Generic MDB account)
 * Initial revision
 * 
 * 02-02-1998     : Written - S.Cox
 *
 *---------------------------------------------------------------------------*/

#include <rpc/types.h>

struct msrpc_ip_param {
	char *csubt;
	char *creq;
	int nobs;
	int nelem;
	int istat;
	int lcsubt;
	int lcreq;
	int lcstr;
	int lcrep;
	long prognum;
	long TimeStamp;
};
typedef struct msrpc_ip_param msrpc_ip_param;
bool_t xdr_msrpc_ip_param();

struct msrpc_op_param {
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
typedef struct msrpc_op_param msrpc_op_param;
bool_t xdr_msrpc_op_param();

struct msrpc_killsv_ip_param {
	long TimeStamp;
	long prognum;
};
typedef struct msrpc_killsv_ip_param msrpc_killsv_ip_param;
bool_t xdr_msrpc_killsv_ip_param();

#define MSRPC_CALL_MDB ((u_long)0)
#define MSRPC_CALL_MDBV ((u_long)1)
#define MSRPC_CALL_MDBP ((u_long)1)
extern msrpc_op_param *msrpc_call_mdbp_1();
#define KILLSERVERP ((u_long)2)
extern void *killserverp_1();
