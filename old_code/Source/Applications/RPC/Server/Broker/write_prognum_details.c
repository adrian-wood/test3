/*-------------------------------------------------------------------*
 * Program      : write_prognum_details  (write_prognum_details.c)                                         
 *
 * Language     : C
 *
 * Description  : Function to write the program number details to
 *              : the file passed in as an argument.
 *
 * Called by    : get_prognum
 *              : reset_prognum
 *
 * Calls        : 
 *
 * Arguments    : struct *file_prognum (ip) : prognum details
 *              : FILE   *file_inout   (ip) : file to write to
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:30    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  10:01:06  10:01:06  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/write_prognum_details.c,v $
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "prognum_details.h"

write_prognum_details(file_prognum, file_inout)

struct prognum_details *file_prognum;
FILE   *file_inout;
{

/*-------------------------------------------------------------------*
 * declare local variables
 *-------------------------------------------------------------------*/

  char text[60];

/*-------------------------------------------------------------------*
 * write the program number details to the file.
 *-------------------------------------------------------------------*/

  sprintf(text,"%d\n",file_prognum->confirm);
  (void)fputs(text,file_inout);
  sprintf(text,"%d\n",file_prognum->status);
  (void)fputs(text,file_inout);
  sprintf(text,"%d\n",file_prognum->year);
  (void)fputs(text,file_inout);
  sprintf(text,"%d\n",file_prognum->month);
  (void)fputs(text,file_inout);
  sprintf(text,"%d\n",file_prognum->day);
  (void)fputs(text,file_inout);
  sprintf(text,"%d\n",file_prognum->hour);
  (void)fputs(text,file_inout);
  sprintf(text,"%d\n",file_prognum->minute);
  (void)fputs(text,file_inout);
  sprintf(text,"%d\n",file_prognum->second);
  (void)fputs(text,file_inout);

  return;
}
