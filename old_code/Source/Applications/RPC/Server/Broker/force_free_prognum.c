/*--------------------------------------------------------------------*
 *
 * Program      : force_free_prognum
 *
 * Language     : C
 *
 * Description  : This routine is called by get_prognum, if all the
 *              : program numbers are already taken (status = 1).
 *              : force_free_prognum will loop over all the program
 *              : number datasets, reading in the date/times they
 *              : were last updated. For each date/time, the difference
 *              : from the current time is calculated and the maximum
 *              : kept. If the maximum time difference is greater than
 *              : the tolerance KILL_AFTER_HOURS_OLD, this program
 *              : number will be used. Reset_prognum will kill the
 *              : server if it is still running (highly unlikely as
 *              : JES should have killed it by now) and will update
 *              : the program number file info with a new date. If no
 *              : program numbers can be forced, prognum=0 is returned
 *
 * Called by    : get_prognum  (get_prognum.c)
 *
 * Calls        : print_current_time   : print the current time
 *              : get_current_time     : get the current time
 *              : calc_century_hour    : calculate century hour
 *              : read_prognum_details : read program number details
 *              : reset_prognum        : free up a program number
 *
 * Arguments    : FILE *svout   (ip) : file for diagnostic output
 *              : int prognum   (op) : main server program number
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
 * Revision 1.2  2001/01/09  16:39:41  16:39:41  usmdb (Generic MDB account)
 * Increased length of variable "filename" - S.Cox
 * 
 * Revision 1.1  98/02/03  09:58:59  09:58:59  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/force_free_prognum.c,v $
 *--------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "prognum_details.h"
#include "constants.h"

int force_free_prognum(svout)

FILE  *svout;
{

/*--------------------------------------------------------------------*
 * Declare and initialise variables.
 *--------------------------------------------------------------------*/

  int     prognum          = 0;  /* program number to return */
  int     count            = 1;  /* counter for looping over files */
  int     current_time_ch  = 0;  /* current time in century hours */
  int     server_time_ch   = 0;  /* server time in century hours */
  int     max_diff_time    = 0;  /* max diff time (current-server) */
  int     max_diff_time_pn = 0;  /* program number with max_diff_time */
  int     diff_time        = 0;  /* diff time (current-server) */
  char    filename[300];         /* file containing prognum details v1.2 */
  char    text[120];             /* text to write to diagnostics file */
  FILE    *file_inout;           /* file structure (prognum details) */
  struct  tm *tmptr;             /* structure for time info */

  struct  prognum_details *current_time;  /* current time structure */
  struct  prognum_details *file_prognum;  /* prognum info structure */

/*-------------------------------------------------------------------*
 * Indicate start of routine to diagnostics file & print the current
 * time.
 *-------------------------------------------------------------------*/

  sprintf(text,"\nforce_free_prognum: start of routine\n");
  (void)fputs(text,svout);

  print_current_time(svout);

/*-------------------------------------------------------------------*
 * Allocate memory for structures.
 *-------------------------------------------------------------------*/

  tmptr = (struct tm *) malloc(sizeof(struct tm));

  current_time = (struct prognum_details *) \
                  malloc(sizeof(struct prognum_details));

  file_prognum = (struct prognum_details *) \
                  malloc(sizeof(struct prognum_details));

/*-------------------------------------------------------------------*
 * Get the current time and convert to century hours.
 *-------------------------------------------------------------------*/

  get_current_time(tmptr);

  current_time->year   = tmptr->tm_year;
  current_time->month  = tmptr->tm_mon;
  current_time->day    = tmptr->tm_mday;
  current_time->hour   = tmptr->tm_hour;
  current_time->minute = tmptr->tm_min;
  current_time->second = tmptr->tm_sec;

  current_time_ch = calc_century_hour(current_time->year,
                                      current_time->month,
                                      current_time->day,
                                      current_time->hour);

/*-------------------------------------------------------------------*
 * Loop over available files. PROGNUM_COUNT (number of available
 * program numbers) and BASE_PROGNUM (starting program number - 1)
 * read from include file constants.h
 *-------------------------------------------------------------------*/

  while (count <= PROGNUM_COUNT) {

    prognum = BASE_PROGNUM + count;
    count++;

/*-------------------------------------------------------------------*
 * Put the filename into string filename, output details to
 * diagnostics file.
 *-------------------------------------------------------------------*/

    sprintf(filename,"%s/prognum_%d.txt",PROGNUM_PATH_PREFIX,prognum);

    sprintf(text,"force_free_prognum: open %s\n",filename);
    (void)fputs(text,svout);

/*-------------------------------------------------------------------*
 * Open this file, freeing structures and returning to calling
 * routine if there was an error.
 *-------------------------------------------------------------------*/

    file_inout = fopen(filename, "r+");

    if (file_inout == NULL) {
      sprintf(text,"force_free_prognum: cannot open %s\n",filename);
      (void)fputs(text,svout);

      free(tmptr);
      free(current_time);
      free(file_prognum);
      return(0);
    }

/*-------------------------------------------------------------------*
 * Read program number, status, date/time from file. Convert the
 * server date/time to century hours.
 *-------------------------------------------------------------------*/

    read_prognum_details(file_prognum, file_inout);

    server_time_ch = calc_century_hour(file_prognum->year,
                                       file_prognum->month,
                                       file_prognum->day,
                                       file_prognum->hour);

/*-------------------------------------------------------------------*
 * Calculate the time difference between this time and the current
 * time. If this is the biggest difference so far, keep it in
 * max_diff_time, and keep the program number in max_diff_time_pn.
 *-------------------------------------------------------------------*/

    diff_time = current_time_ch - server_time_ch;

    if (diff_time > max_diff_time) {
      max_diff_time    = diff_time;
      max_diff_time_pn = prognum;
    }

/*-------------------------------------------------------------------*
 * Close the file.
 *-------------------------------------------------------------------*/

    (void)fclose(file_inout);
  }

/*-------------------------------------------------------------------*
 * We have now looped over all available files. Output max_diff_time
 * and max_diff_time_pn to diagnostics file.
 *-------------------------------------------------------------------*/

  sprintf(text,"force_free_prognum: max_diff_time    = %d\n",
  max_diff_time);
  (void)fputs(text,svout);

  sprintf(text,"force_free_prognum: max_diff_time_pn = %d\n",
  max_diff_time_pn);
  (void)fputs(text,svout);

/*-------------------------------------------------------------------*
 * If max_diff_time is greater than our threshold (KILL_AFTER_HOURS_
 * OLD), update the file_prognum details with the current time info.
 *-------------------------------------------------------------------*/

  prognum = 0;

  if (max_diff_time > KILL_AFTER_HOURS_OLD) {

    file_prognum->confirm = max_diff_time_pn;
    file_prognum->year    = current_time->year;
    file_prognum->month   = current_time->month;
    file_prognum->day     = current_time->day;
    file_prognum->hour    = current_time->hour;
    file_prognum->minute  = current_time->minute;
    file_prognum->second  = current_time->second;

/*-------------------------------------------------------------------*
 * Call reset_prognum to kill the server and free the program number
 * (update prognum file details). The prognum is returned if all OK
 *-------------------------------------------------------------------*/

    prognum = reset_prognum(max_diff_time_pn, file_prognum, svout);
  }

/*-------------------------------------------------------------------*
 * Free memory, output diagnostics to diagnostics file & return
 * prognum to calling program
 *-------------------------------------------------------------------*/

  free(tmptr);
  free(current_time);
  free(file_prognum);

  print_current_time(svout);

  sprintf(text,"\nforce_free_prognum: end of routine\n");
  (void)fputs(text,svout);

  return(prognum);
}
