/*-------------------------------------------------------------------*
 * Program      : reset_prognum  (reset_prognum.c)                                
 *
 * Language     : C
 *
 * Description  : This routine is only used when there are no program
 *              : numbers available for multi-user RPC. If routine
 *              : force_free_prognum has assigned a program number
 *              : for resetting, this routine is called. It will 1st
 *              : kill the server corresponding to the program number.
 *              : It is highly unlikely that the main server will still
 *              : be executing. It will then update the status and
 *              : time info for this program number, before returning
 *              : to the calling routine.
 *
 * Called by    : force_free_prognum
 *
 * Calls        : print_current_time    : print the current time
 *              : kill_server           : kill the main server
 *              : write_prognum_details : write prognum info to file
 *
 * Arguments    : int ip_prognum       (ip) :
 *              : struct *file_prognum (ip) :
 *              : FILE *svout          (ip) :
 *              : int op_prognum       (op) :
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:29    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2001/01/09  16:45:47  16:45:47  usmdb (Generic MDB account)
 * Increased length of variable "filename" - S.Cox
 * 
 * Revision 1.1  98/02/03  10:01:02  10:01:02  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/reset_prognum.c,v $
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "prognum_details.h"
#include "constants.h"

int reset_prognum(ip_prognum, file_prognum, svout)

int     ip_prognum;
struct  prognum_details *file_prognum;
FILE    *svout;
{

/*----------------------------------------------------------------------*
 * Declare local variables
 *----------------------------------------------------------------------*/

  int     op_prognum = 0;
  char    filename[300];                                         /* 1.2 */
  char    text[120];
  FILE    *file_inout;

/*----------------------------------------------------------------------*
 * Output start of routine and current time to diagnostics file
 *----------------------------------------------------------------------*/

  sprintf(text,"\nreset_prognum: start of routine\n");
  (void)fputs(text,svout);

  print_current_time(svout);

/*----------------------------------------------------------------------*
 * Try to kill the main server. It shouldn't be running.
 *----------------------------------------------------------------------*/

  sprintf(text,"reset_prognum: call kill_server\n");
  (void)fputs(text,svout);

  kill_server(ip_prognum, svout);

  sprintf(text,"\nreset_prognum: after kill_server\n");
  (void)fputs(text,svout);

/*----------------------------------------------------------------------*
 * Update the status and time information in the program number file.
 *----------------------------------------------------------------------*/

  sprintf(filename,"%s/prognum_%d.txt",PROGNUM_PATH_PREFIX,ip_prognum);

  file_inout = fopen(filename, "r+");

  if (file_inout == NULL) {
    sprintf(text,"reset_prognum: cannot open %s\n",filename);
    (void)fputs(text,svout);

    free(file_prognum);
    return(0);
  }

  write_prognum_details(file_prognum, file_inout);

/*----------------------------------------------------------------------*
 * Close program number file and return to calling routine.
 *----------------------------------------------------------------------*/

  (void)fclose(file_inout);

  op_prognum = ip_prognum;

  print_current_time(svout);

  sprintf(text,"reset_prognum: end of routine\n");
  (void)fputs(text,svout);

  return(op_prognum);
}
