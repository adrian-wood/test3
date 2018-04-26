/*-------------------------------------------------------------------*
 * Program      : read_prognum_details  (read_prognum_details.c)                                         
 *
 * Language     : C
 *
 * Description  : Function to read the program number details from
 *              : the file passed in as an argument. The program
 *              : number details are returned to the calling routine
 *              : in a structure.
 *
 * Called by    : get_prognum
 *              : force_free_prognum
 *
 * Calls        : 
 *
 * Arguments    : struct *file_prognum (ip/op) : prognum details
 *              : FILE   *file_inout   (ip)    : file to read from
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
 * Revision 1.1  98/02/03  10:00:58  10:00:58  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/read_prognum_details.c,v $
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "prognum_details.h"

read_prognum_details(file_prognum, file_inout)

struct prognum_details *file_prognum;
FILE   *file_inout;
{

/*-------------------------------------------------------------------*
 * declare local variables
 *-------------------------------------------------------------------*/

  char text[60];
  int  scancount;

/*-------------------------------------------------------------------*
 * read in the program number details and put into structure
 * file_prognum to return to calling routine.
 *-------------------------------------------------------------------*/

  (void)fgets(text, sizeof(text), file_inout);
  scancount = sscanf(text, "%d", &file_prognum->confirm);
  (void)fgets(text, sizeof(text), file_inout);
  scancount = sscanf(text, "%d", &file_prognum->status);
  (void)fgets(text, sizeof(text), file_inout);
  scancount = sscanf(text, "%d", &file_prognum->year);
  (void)fgets(text, sizeof(text), file_inout);
  scancount = sscanf(text, "%d", &file_prognum->month);
  (void)fgets(text, sizeof(text), file_inout);
  scancount = sscanf(text, "%d", &file_prognum->day);
  (void)fgets(text, sizeof(text), file_inout);
  scancount = sscanf(text, "%d", &file_prognum->hour);
  (void)fgets(text, sizeof(text), file_inout);
  scancount = sscanf(text, "%d", &file_prognum->minute);
  (void)fgets(text, sizeof(text), file_inout);
  scancount = sscanf(text, "%d", &file_prognum->second);

  return;
}
