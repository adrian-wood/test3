/*--------------------------------------------------------------------*
 *
 * Program      : kill_server  (kill_server.c)
 *
 * Language     : C
 *
 * Description  : This routine will perform an RPC to the main server
 *              : to try and kill it. First it will try to connect with
 *              : the server (it may not be running). If it is running
 *              : it will call the server killserverp_1 routine to kill
 *              : it. kill_server is only called when force_free_prognum
 *              : is called.
 *
 * Called by    : reset_prognum  (reset_prognum.c)
 *
 * Calls        : print_current_time   : print the current time
 *              : clnt_create          : connect to main server
 *              : killserverp_1        : call the main server
 *              : clnt_destroy         : close the socket
 *
 * Arguments    : int prognum   (ip) : program number of server to kill
 *              : FILE *svout   (ip) : file for diagnostic output
 *
 * Changes      :
 *
 * 29-01-1998   : Original by Simon Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:19    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2001/01/09  16:46:27  16:46:27  usmdb (Generic MDB account)
 * Change to comments to remove IBM references - S.Cox
 * 
 * Revision 1.1  98/02/03  09:59:15  09:59:15  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/kill_server.c,v $
 *--------------------------------------------------------------------*/

#include <stdio.h>           /* for standard C i/o */
#include <stdlib.h>          /* for malloc */
#include <string.h>          /* for string handling */
#include <sys/time.h>        /* to set our own TIMEOUT */
#include <rpc/rpc.h>         /* for all the RPC/XDR calls */
#include "kill_server.h"     /* local include for main RPC server */
#include "constants.h"       /* local include for MetDB RPC constants */

kill_server(prognum, svout)

int   prognum;
FILE  *svout;
{

/*--------------------------------------------------------------------*
 * Declare and initialise local variables.
 *--------------------------------------------------------------------*/

  CLIENT                 *cl;               /* RPC client handle */
  msrpc_killsv_ip_param  *killsv_input;     /* structure for server */
  struct                 timeval timeOut;   /* RPC timeout structure */
  long                   TimeStamp = 0;     /* dummy timestamp */
  char                   text[120];         /* diagnostics text */

/*--------------------------------------------------------------------*
 * Output start of routine and current time diagnostics to diagnostics
 * file.
 *--------------------------------------------------------------------*/

  sprintf(text,"\nkill_server: start of routine\n");
  (void)fputs(text,svout);

  print_current_time(svout);

/*--------------------------------------------------------------------*
 * allocate memory for the structure killsv_input.
 *--------------------------------------------------------------------*/

  killsv_input = (msrpc_killsv_ip_param *) \
                  malloc(sizeof(msrpc_killsv_ip_param));

/*--------------------------------------------------------------------*
 * Try to connect to the main server. If it is not running (it probably
 * won't be), free structure and return to calling routine.
 *--------------------------------------------------------------------*/

  cl = clnt_create(MSRPC_MAIN_SERVER, prognum, MSRPC_CALL_MDBV, TRANSPORT);
  if (cl == NULL) {
    sprintf(text,"kill_server: cannot connect with server\n");
    (void)fputs(text,svout);
    free(killsv_input);
    return;
  }

/*--------------------------------------------------------------------*
 * Increase the RPC timeout.
 *--------------------------------------------------------------------*/

  timeOut.tv_sec  = MYTIMEOUT;
  timeOut.tv_usec = 0;

/*--------------------------------------------------------------------*
 * Put dummy timestamp and program number of server to kill into
 * structure to pass to server. Output diagnostics to diagnostics file.
 *--------------------------------------------------------------------*/

  killsv_input->TimeStamp = TimeStamp;
  killsv_input->prognum   = prognum;

  sprintf(text,"kill_server: killsv_input_prognum = %d\n",
  killsv_input->prognum);
  (void)fputs(text,svout);

/*--------------------------------------------------------------------*
 * Make the RPC call to kill the server.
 * NOTE: There could be problems here. If a server is doing something
 * e.g. MOUNTing a cartridge, it will not be listening for client
 * requests.
 *--------------------------------------------------------------------*/

  killserverp_1(killsv_input,cl);

/*--------------------------------------------------------------------*
 * Neatly close the socket, free the structure, output diagnostics to
 * the diagnostics file and return to calling program.
 *--------------------------------------------------------------------*/

  clnt_destroy(cl);
  free(killsv_input);

  print_current_time(svout);

  sprintf(text,"kill_server: end of routine\n");
  (void)fputs(text,svout);

  return;
}
