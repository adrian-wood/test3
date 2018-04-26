/*-------------------------------------------------------------------*
 * Program      : get_current_time  (get_current_time.c)                                         
 *
 * Language     : C
 *
 * Description  : Function to return the current time (GMT)
 *
 * Called by    : get_prognum
 *              : force_free_prognum
 *
 * Calls        : time    : system time routine 
 *              : gmtime  : system time routine 
 *
 * Arguments    : struct tm *argp  (ip/op) : current time structure
 *
 * Changes      :
 *
 * 02-02-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:19    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  09:59:06  09:59:06  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/get_current_time.c,v $
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

get_current_time(argp)

struct tm *argp;
{

/*-------------------------------------------------------------------*
 * Declare local variables.
 *-------------------------------------------------------------------*/

  struct tm *tmptr;
  time_t ltime;

/*-------------------------------------------------------------------*
 * Call time to get the current time. Call gmtime to convert this to
 * a structure based time. The year is returned with 0 = 1900, so add
 * 100 to this to get the real year. Months are in the range 0 to 11,
 * so add 1 to get a real month. Finally return to calling routine.
 *-------------------------------------------------------------------*/

  time(&ltime);
  tmptr = gmtime(&ltime);
  *argp = *tmptr;
  argp->tm_year = argp->tm_year + 1900;    /* 0 == 1900 */
  argp->tm_mon  = argp->tm_mon  + 1;       /* range = 0..11 */

  return;
}
