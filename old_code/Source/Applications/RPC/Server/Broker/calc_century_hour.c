/*-------------------------------------------------------------------*
 *
 * Program      : calc_century_hour  (calc_century_hour.c)
 *
 * Language     : C
 *
 * Description  : Given a year, month, day and hour, this routine
 *              : returns the century hour. It is based on fortran
 *              : routine DT2HRS.
 *
 * Called by    : force_free_prognum
 *
 * Calls        : calc_century_day  : routine to calculate century day
 *
 * Arguments    : int year          (ip) :  year
 *              : int month         (ip) :  month
 *              : int day           (ip) :  day
 *              : int hour          (ip) :  hour
 *              : int century_hours (op) :  century hours to return
 *
 * Changes      :
 *
 * 29-01-1998   : Based on fortran routine DT2HRS - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:17    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  09:58:51  09:58:51  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/calc_century_hour.c,v $
 *-------------------------------------------------------------------*/

int calc_century_hour(year, month, day, hour)

int year;
int month;
int day;
int hour;
{

/*-------------------------------------------------------------------*
 * declare local variables.
 *-------------------------------------------------------------------*/

int century_day;
int century_hours;

/*-------------------------------------------------------------------*
 * call calc_century_day to calculate the century day. Convert this
 * to century hours and return result to calling program.
 *-------------------------------------------------------------------*/

century_day   = calc_century_day(day, month, year);
century_hours = (century_day - 1)*24 + hour;

return(century_hours);
}
