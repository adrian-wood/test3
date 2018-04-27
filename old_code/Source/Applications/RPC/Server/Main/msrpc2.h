/*-------------------------------------------------------------------*
 * Program      : msrpc.h
 *
 * Language     : C
 *
 * Description  : MetDB multi-user RPC include file
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    MetDB_Refresh 1.0         09/02/2011 10:03:53    Alison Weir
 *       Original version
 * $
 * Revision 1.3  2001/01/09  14:25:28  14:25:28  usmdb (Generic MDB account)
 * Change to header to remove IBM references - S.Cox
 *
 * Revision 1.2  2000/06/16  11:59:28  11:59:28  usmdb (Generic MDB account)
 * Addition of structure crep_binary for RPC transfer of
 * binary data - S.Cox
 *
 * Revision 1.1  98/02/03  09:57:57  09:57:57  usmdb (Generic MDB account)
 * Initial revision
 *
 * $Source: /home/us0400/mdb/op/lib/RPC/server/main/RCS/msrpc.h,v $
 *-------------------------------------------------------------------*/

#include <rpc/types.h>

struct ip_param {
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
 	char *userid;
 	char *client_ip;
 	char *contact;
 };
 typedef struct ip_param ip_param;
 bool_t xdr_ip_param();


 struct op_param {
 	struct {
 		u_int array_len;

 		float *array_val;

 	} array;
 	int nobs;
 	int istat;
 	char *cstr;
 	char *crep;
 	struct {
 		u_int crep_binary_len;
 		char *crep_binary_val;
 	} crep_binary;
 };
 typedef struct op_param op_param;
 bool_t xdr_op_param();


 struct killsv_ip_param {
 	long TimeStamp;
 	long prognum;
 };
 typedef struct killsv_ip_param killsv_ip_param;
 bool_t xdr_killsv_ip_param();


 #define CALL_MDB ((u_long)0)
 #define CALL_MDBV ((u_long)1)
 #define CALL_MDBP ((u_long)1)
 extern op_param *call_mdbp_1();
 #define KILLSERVERP ((u_long)2)
 extern void *killserverp_1();

