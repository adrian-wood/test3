/*--------------------------------------------------------------------*
 *
 * Program      : free_prognum  (free_prognum.c)
 *
 * Language     : C
 *
 * Description  : This routine is called by the RPC client after the
 *              : client has killed the server. Its purpose is to free
 *              : the program number (passed in). It will read the file
 *              : info for this program number, change the status to 0
 *              : and write the details back.
 *
 * Called by    : msrpc_call_freepnp_1  (msrpc_call_freepn.c)
 *
 * Calls        : print_current_time    : print the current time
 *              : read_prognum_details  : read in prognum file info
 *              : write_prognum_details : write out prognum file info
 *
 * Arguments    : int  prognum  (ip) : program number to free
 *              : FILE *svout   (ip) : file for diagnostic output
 *              : int  status   (op) : return code sent back to caller
 *
 * Changes      :
 *
 * 29-01-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:18    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2001/01/09  16:40:51  16:40:51  usmdb (Generic MDB account)
 * Increased length of variable "filename" - S.Cox
 * 
 * Revision 1.1  98/02/03  09:59:02  09:59:02  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/free_prognum.c,v $
 *--------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "prognum_details.h"
#include "constants.h"

int free_prognum(prognum, svout)

int   prognum;
FILE  *svout;
{

/*--------------------------------------------------------------------*
 * Declare local variables
 *--------------------------------------------------------------------*/

  FILE    *file_inout;
  char    filename[300];                                       /* 1.2 */
  char    text[120];
  int     status = 0;
  int     close_status;
  struct  prognum_details *file_prognum;

/*--------------------------------------------------------------------*
 * Output start of routine info to diagnostics file (including current
 * time).
 *--------------------------------------------------------------------*/

  sprintf(text,"\nfree_prognum: start of routine\n");
  (void)fputs(text,svout);

  print_current_time(svout);

/*--------------------------------------------------------------------*
 * Allocate memory for structure file_prognum
 *--------------------------------------------------------------------*/

  file_prognum = (struct prognum_details *) \
                  malloc(sizeof(struct prognum_details));

/*--------------------------------------------------------------------*
 * Open the prognum file. If there was a failure opening the file,
 * return to calling program with a status = 99
 *--------------------------------------------------------------------*/

  sprintf(text,"free_prognum: prognum to free = %d\n",prognum);
  (void)fputs(text,svout);

  sprintf(filename,"%s/prognum_%d.txt",PROGNUM_PATH_PREFIX,prognum);

  file_inout = fopen(filename, "r+");

  if (file_inout == NULL) {
    sprintf(text,"free_prognum: cannot open file %s\n",filename);
    (void)fputs(text,svout);

    status = 99;
    free(file_prognum);
    return(status);
  }

/*--------------------------------------------------------------------*
 * Read in the program number details from the file
 *--------------------------------------------------------------------*/

  read_prognum_details(file_prognum, file_inout);

/*--------------------------------------------------------------------*
 * Check the prognum status is 1 and change it to 0 (to free the
 * program number). Write new details to file.
 *--------------------------------------------------------------------*/

  if (file_prognum->status == 1) {
    file_prognum->status = 0;
    rewind(file_inout);
    write_prognum_details(file_prognum, file_inout);
  }
  else {
    sprintf(text,"free_prognum: prognum status already 0!\n");
    (void)fputs(text,svout);
    status = 99;
  }

/*--------------------------------------------------------------------*
 * Close the file.
 *--------------------------------------------------------------------*/

  free(file_prognum);
  close_status = fclose(file_inout);

  if (close_status != 0) {
    sprintf(text,"free_prognum: cannot close file %s\n",filename);
    (void)fputs(text,svout);

    sprintf(text,"free_prognum: fclose status = %d\n",close_status);
    (void)fputs(text,svout);
  }

/*--------------------------------------------------------------------*
 * Output diagnostics and return to client. THERE USED TO BE A STRANGE
 * PROBLEM HERE. THE SERVER WOULD ONLY RETURN TO THE CLIENT IF THERE
 * WAS A FPRINTF BEFORE THE RETURN. PROBLEM WITH STACK?
 *--------------------------------------------------------------------*/

  print_current_time(svout);

  sprintf(text,"free_prognum: end of routine\n");
  (void)fputs(text,svout);

  return(status);
}
