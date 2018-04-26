/*--------------------------------------------------------------------*
 *
 * Program      : get_prognum  (get_prognum.c)
 *
 * Language     : C
 *
 * Description  : This routine will loop over program number files
 *              : until it finds one with a status = 0 (available for
 *              : use). It will then change the status to 1 and put the
 *              : current time into the file. This program number is
 *              : returned. If it can't find a free program number, it
 *              : will call force_free_prognum to try and force a free
 *              : number.
 *
 * Called by    : mdb_rpc_broker
 *
 * Calls        : print_current_time    : print the current time
 *              : read_prognum_details  : read program number info
 *              : write_prognum_details : write program number info
 *              : get_current_time      : get the current time
 *              : force_free_prognum    : force a free program number
 *
 * Arguments    : FILE *svout   (ip) : file for diagnostic output
 *              : int prognum   (op) : program number to return
 *
 * Changes      :
 *
 * 29-01-1998   : Written - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:19    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2001/01/09  16:41:31  16:41:31  usmdb (Generic MDB account)
 * Increased length of variable "filename" - S.Cox
 * 
 * Revision 1.1  98/02/03  09:59:10  09:59:10  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/get_prognum.c,v $
 *--------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "prognum_details.h"
#include "constants.h"

int get_prognum(svout)

FILE  *svout;
{

/*-------------------------------------------------------------------*
 * Declare and initialise local variables
 *-------------------------------------------------------------------*/

  int     count           = 1;   /* counter for looping over files */
  int     found_a_prognum = 0;   /* if prognum assigned, this = 1 */
  int     prognum         = 0;   /* program number to return */
  char    filename[300];         /* filename of program number file v1.2 */
  char    text[120];             /* diagnostics output string */
  FILE    *file_inout;           /* file structure for prognum file */
  struct  tm *tmptr;             /* time structure for current time */

  struct  prognum_details *file_prognum;   /* prognum file details */

/*-------------------------------------------------------------------*
 * Output routine start and current time to the diagnostics file.
 *-------------------------------------------------------------------*/

  sprintf(text,"\nget_prognum: start of routine\n");
  (void)fputs(text,svout);

  print_current_time(svout);

/*-------------------------------------------------------------------*
 * Allocate memory for structures.
 *-------------------------------------------------------------------*/

  tmptr = (struct tm *) malloc(sizeof(struct tm));

  file_prognum = (struct prognum_details *) \
                  malloc(sizeof(struct prognum_details));

/*-------------------------------------------------------------------*
 * Loop over available files. PROGNUM_COUNT is the number of
 * available files. It is set in constants.h. BASE_PROFGNUM is the
 * first program number - 1. The filename is constructed and placed
 * in string filename.
 *-------------------------------------------------------------------*/

  while (count <= PROGNUM_COUNT) {

    prognum = BASE_PROGNUM + count;
    count++;
    sprintf(filename,"%s/prognum_%d.txt",PROGNUM_PATH_PREFIX,prognum);

/*-------------------------------------------------------------------*
 * Output filename to diagnostics file and try to open the prognum
 * details file. If there is a problem, output message, free memory
 * and return to calling program with prognum = 0.
 *-------------------------------------------------------------------*/

    sprintf(text,"get_prognum: will open file = %s\n",filename);
    (void)fputs(text,svout);

    file_inout = fopen(filename, "r+");
    if (file_inout == NULL) {
      sprintf(text,"get_prognum: cannot open file = %s\n",filename);
      (void)fputs(text,svout);

      free(tmptr);
      free(file_prognum);
      return(0);
    }

/*-------------------------------------------------------------------*
 * Read program number, status, date/time from file.
 *-------------------------------------------------------------------*/

    read_prognum_details(file_prognum, file_inout);

/*-------------------------------------------------------------------*
 * If the prognum status is 0 (program number free), update the
 * file information (status = 1 and time = current time)
 *-------------------------------------------------------------------*/

    if (file_prognum->status == 0) {

      file_prognum->status = 1;
      get_current_time(tmptr);

      file_prognum->year   = tmptr->tm_year;
      file_prognum->month  = tmptr->tm_mon;
      file_prognum->day    = tmptr->tm_mday;
      file_prognum->hour   = tmptr->tm_hour;
      file_prognum->minute = tmptr->tm_min;
      file_prognum->second = tmptr->tm_sec;

/*-------------------------------------------------------------------*
 * Write program number, status, date/time to file. Close the file.
 *-------------------------------------------------------------------*/

      rewind(file_inout);

      write_prognum_details(file_prognum, file_inout);

      (void)fclose(file_inout);

/*-------------------------------------------------------------------*
 * Output diagnostics to diagnostics file, set found_a_prognum = 1
 * and exit the loop over program numbers.
 *-------------------------------------------------------------------*/

      sprintf(text,"get_prognum: after write_prognum_details\n");
      (void)fputs(text,svout);
      sprintf(text,"get_prognum: will use prognum = %d\n",prognum);
      (void)fputs(text,svout);

      found_a_prognum = 1;
      break;
    }

/*-------------------------------------------------------------------*
 * close the program number file for this program number (status = 1)
 * so we can't use it.
 *-------------------------------------------------------------------*/

    (void)fclose(file_inout);
  }

/*-------------------------------------------------------------------*
 * if found_a_prognum = 0, we have not been able to find a free
 * program number. Try to force a free program number.
 *-------------------------------------------------------------------*/

  if (found_a_prognum == 0) {
    sprintf(text,"get_prognum: no free prognums available\n");
    (void)fputs(text,svout);
    sprintf(text,"get_prognum: will try to force a free prognum\n");
    (void)fputs(text,svout);

    prognum = force_free_prognum(svout);
    if (prognum == 0) {
      sprintf(text,"\nget_prognum: cannot force a free prognum\n");
      (void)fputs(text,svout);
    } else {
      sprintf(text,"\nget_prognum: forced prognum = %d\n",prognum);
      (void)fputs(text,svout);
    }
  }

/*-------------------------------------------------------------------*
 * free structures, output the current time and "end of routine"
 * to diagnostics file and return to calling routine.
 *-------------------------------------------------------------------*/

  free(tmptr);
  free(file_prognum);

  print_current_time(svout);

  sprintf(text,"get_prognum: end of routine\n");
  (void)fputs(text,svout);

  return(prognum);
}
