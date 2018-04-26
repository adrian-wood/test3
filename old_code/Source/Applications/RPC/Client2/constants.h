/*----------------------------------------------------------------------------*
 * Program      : constants.h                                                
 *
 * Language     : C
 *
 * Description  : Define global constants, variables for MetDB multi-user 
 *              : RPC
 *
 * $Revision: 1$
 * $Date: 09/11/2009 10:44:12$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/constants.h,v $
 *
 * Change info  :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
 * Revision 1.4  2000/09/05  10:54:05  10:54:05  usmdb (Generic MDB account)
 * Addition of BROKER_TIMEOUT, DEFUALT_DEBUG_LEVEL,
 * DEFAULT_SERVER_IPNAME, metdb_timeout, server_ipname.
 * DEFAULT_MAIN_TIMEOUT = 5400 replaces old 10000 second
 * timeout - S.Cox
 * 
 * Revision 1.3  2000/08/08  11:26:21  11:26:21  usmdb (Generic MDB account)
 * MAX_BROKER_CONNECTS increased from 5 to 20 - S.Cox
 * 
 * Revision 1.2  99/07/28  12:12:07  12:12:07  usmdb (Generic MDB account)
 * Change MSRPC_BROKER_SERVER to be ukmet. Added global variable debug_level 
 * for debug output - S.Cox
 * 
 * Revision 1.1  1998/02/02 16:00:49  usmdb
 * Initial revision
 *
 *----------------------------------------------------------------------------*/

#define BROKER_TIMEOUT 1800                /* broker server timeout limit (secs) 1.4 */
#define DEFAULT_DEBUG_LEVEL 0              /* Default debug level 1.4 */
#define DEFAULT_MAIN_TIMEOUT 5400          /* Default main server timeout limit (secs) 1.4 */
#define DEFAULT_SERVER_IPNAME "ukmet"      /* default RPC server IP name 1.4 */
#define DEFAULT_CONTACT "No contact details" /* default contact details for client */
#define MAX_BROKER_CONNECTS 20             /* Max connect attempts to broker 1.3 */
#define TRANSPORT "tcp"                    /* RPC transport protocol */
#define WAITACTION "----"                  /* Broker wait action */ 

/*----------------------------------------------------------------------------*
 * Global variables for use throughout client code. These variables are
 * permanent because they are global.
 *----------------------------------------------------------------------------*/

int  debug_level;                          /* 1.2 */
int  metdb_timeout;                        /* 1.4 */
char server_ipname[200];                   /* 1.4 */

