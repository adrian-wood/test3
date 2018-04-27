/*-------------------------------------------------------------------*
 *
 * Program      : mdb_rpc_broker  (mdb_rpc_broker.c)
 *
 * Language     : C
 *
 * Description  : This routine calls get_prognum to get a free RPC
 *              : program number from the list available. It then calls
 *              : 2 scripts to 1) start the main server with this
 *              : program number, 2) To check the main server has
 *              : started. When the server has started, the program
 *              : number is returned to the calling ptogram.
 *
 * Called by    : msrpc_call_brokerp_1 (msrpc_call_broker.c)
 *
 * Calls        : print_current_time : print the current time
 *              : get_prognum        : get a free prognum
 *              : mdb_start_server   : start the main RPC server
 *              : mdb_check_server   : check the main server is running
 *
 * Arguments    : FILE *svout   (ip) : file for diagnostic output
 *              : int  prognum  (op) : main server program number
 *
 * $Revision: 2$
 * $Date: 06/10/2009 11:35:53$
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/mdb_rpc_broker.c,v $
 *
 * Changes      :
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:23    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2001/01/09  16:42:23  16:42:23  usmdb (Generic MDB account)
 * Included local include file constants.h in order to
 * use MSRPC_MAIN_SERVER - S.Cox
 *
 * Revision 1.1  98/02/03  09:59:40  09:59:40  usmdb (Generic MDB account)
 * Initial revision
 *
 * 02-04-1997   : Original by Martyn Catlow
 *
 * 29-01-1998   : Re-write. Will now call C routine to get a free
 *              : prognum, instead of a REXX routine. Addition of
 *              : more diagnostic output. - S.Cox
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <stddef.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "constants.h"                                         /* 1.2 */

/* ------------------------------------------------------------------
   Declare global variables.
   ------------------------------------------------------------------ */

int  rc=0;
char rqsttxt[250];

/* ------------------------------------------------------------------
   subprogram name
   ------------------------------------------------------------------ */

int mdb_rpc_broker(svout)

FILE  *svout;
{

int   prognum;
char  text[120];

   sprintf(text,"\nmdb_rpc_broker: start of routine\n");
   (void)fputs(text,svout);

   print_current_time(svout);

/* ------------------------------------------------------------------
   get a free program number.
   ------------------------------------------------------------------ */

   prognum = get_prognum(svout);

   sprintf(text,"\nmdb_rpc_broker: prognum from get_prognum = %d\n",
   prognum);
   (void)fputs(text,svout);

   if (prognum == 0) {
     sprintf(text,"mdb_rpc_broker: no free servers\n");
     (void)fputs(text,svout);
     return(0);
   }

/* ------------------------------------------------------------------
   call script to start the main RPC server. Prognum in rqsttxt
   ------------------------------------------------------------------ */

   sprintf(text,"mdb_rpc_broker: call mdb_start_server\n");
   (void)fputs(text,svout);

   sprintf(rqsttxt,"mdb_start_server %d",prognum);
   rc = system(rqsttxt);

   sprintf(text,"mdb_rpc_broker: rc from mdb_start_server = %d\n",rc);
   (void)fputs(text,svout);

/* ------------------------------------------------------------------
   now call script to check the main RPC server has started
   ------------------------------------------------------------------ */

   sprintf(text,"mdb_rpc_broker: call mdb_check_server\n");
   (void)fputs(text,svout);

   sprintf(rqsttxt,"mdb_check_server %d %s",prognum,
   MSRPC_MAIN_SERVER);                                        /* 1.2 */
   rc = system(rqsttxt);

   sprintf(text,"mdb_rpc_broker: rc from mdb_check_server = %d\n",rc);
   (void)fputs(text,svout);

/* ------------------------------------------------------------------
   return back to caller with the program number
   ------------------------------------------------------------------ */

   print_current_time(svout);

   sprintf(text,"mdb_rpc_broker: end of routine\n");
   (void)fputs(text,svout);

   return prognum;
}
