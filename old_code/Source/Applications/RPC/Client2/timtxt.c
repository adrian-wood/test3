/*-------------------------------------------------------------------*
 * Program        : timtxt (timtxt.c)                                         
 *
 * Language       : C
 *
 * Description    : Function to print the current time.
 *
 * Called by      : various
 *
 * Calls          : time  (system time routine) 
 *                : localtime (system time routine)
 *                : strftime (system time routine)
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 09/11/2009 10:44:12$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/timtxt.c,v $
 *
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
 * Revision 1.1  2000/06/16  11:50:42  11:50:42  usmdb (Generic MDB account)
 * Initial revision
 * 
 *
 * 14-06-2000   : Written - S.Cox
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <time.h>

timtxt()
{

/*-------------------------------------------------------------------*
 * declare local variables
 *-------------------------------------------------------------------*/

  char time_text[19];
  time_t time_now;
  struct tm *datim;

/*-------------------------------------------------------------------*
 * call system time routines to get the current time and format it
 * in string time_text. Output time_text.
 *-------------------------------------------------------------------*/

  time(&time_now);
  datim = localtime(&time_now);
  strftime(time_text, 19, "%d/%m/%y %H:%M:%S ", datim); 

  (void)fprintf(stdout,"%s ", time_text);

  return;
}
