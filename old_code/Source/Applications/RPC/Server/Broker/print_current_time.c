/*-------------------------------------------------------------------*
 * Program      : print_current_time  (print_current_time.c)                                         
 *
 * Language     : C
 *
 * Description  : Function to print the current time to the file
 *              : passed in as an argument.
 *
 * Called by    : various
 *
 * Calls        : time  (system time routine) 
 *              : ctime (system time routine)
 *
 * Arguments    : FILE *svout  (ip) : File to print current time to
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
 * Revision 1.1  98/02/03  10:00:47  10:00:47  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/print_current_time.c,v $
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void print_current_time(svout)

FILE *svout;
{

/*-------------------------------------------------------------------*
 * declare local variables
 *-------------------------------------------------------------------*/

long time_i;
char *time_s;
char text[120];

/*-------------------------------------------------------------------*
 * call system time routines to get the current time in character
 * form.
 *-------------------------------------------------------------------*/

time_i = time(0);
time_s = malloc(40);
time_s = ctime(&time_i);

sprintf(text,"print_current_time: %s\n",time_s);
(void)fputs(text,svout);

free(time_s);

return;
}
