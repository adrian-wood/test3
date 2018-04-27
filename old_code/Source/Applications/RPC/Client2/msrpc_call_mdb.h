
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
	char *userid;
	char *client_ip;
	char *contact;
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
	struct {
		u_int crep_binary_len;
		char *crep_binary_val;
	} crep_binary;
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

