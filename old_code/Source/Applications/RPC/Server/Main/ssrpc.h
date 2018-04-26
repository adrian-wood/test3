/*-------------------------------------------------------------------*
 * Program      : ssrpc.h                                         
 *
 * Language     : C
 *
 * Description  : MetDB single-server RPC include file
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:36:14    Kudsia Gwangwaa 
 * $
 * Revision 1.4  2001/01/09  14:25:15  14:25:15  usmdb (Generic MDB account)
 * Change to header to remove IBM references - S.Cox
 * 
 * Revision 1.3  2000/06/16  12:00:30  12:00:30  usmdb (Generic MDB account)
 * Addition of structure crep_binary for RPC transfer of
 * binary data - S.Cox
 * 
 * Revision 1.2  98/07/31  13:24:45  13:24:45  usmdb (Generic MDB account)
 * no entry
 * 
 * Revision 1.1  1998/02/03 09:58:06  usmdb
 * Initial revision
 *
 * $Source: /home/us0400/mdb/op/lib/RPC/server/main/RCS/ssrpc.h,v $
 *-------------------------------------------------------------------*/
                                                                                
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
        struct {                                              /* 1.3 */               
          u_int crep_binary_len;                              /* 1.3 */
          char *crep_binary_val;                              /* 1.3 */
        } crep_binary;                                        /* 1.3 */
};                                                                              
typedef struct op_param op_param;                                               
bool_t xdr_op_param();                                                          
                                                                                
                                                                                
#define TMDB ((u_long)0x2000995)                                                
#define TMDBV ((u_long)1)                                                       
#define TMDBP ((u_long)1)                                                       
extern op_param *tmdbp_1();                                                     
                                                                                
