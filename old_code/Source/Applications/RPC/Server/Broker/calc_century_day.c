/*-------------------------------------------------------------------*
 *
 * Program      : calc_century_day
 *
 * Language     : C
 *
 * Description  : Given a day, month and year, the routine calculates
 *              : a century day. This code is based on the fortran
 *              : routine DATE31.
 *
 * Called by    : calc_century_hour   (calc_century_hour.c)
 *
 * Calls        : none
 *
 * Arguments    : int day         (ip)  : day
 *              : int month       (ip)  : month
 *              : int year        (ip)  : year
 *              : int century_day (op)  : century day to return
 *
 * Changes      :
 *
 * 29-01-1998   : Based on DATE31 - S.Cox
 *
 * $Log:
 *  2    Met_DB_Project 1.1         06/10/2009 11:35:53    Sheila Needham
 *       z/os1.08 backup before change to z/os 1.10
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:17    Kudsia Gwangwaa 
 * $
 * Revision 1.1  98/02/03  09:58:44  09:58:44  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/calc_century_day.c,v $
 *-------------------------------------------------------------------*/

int calc_century_day(day, month, year)

int day;
int month;
int year;
{

/*-------------------------------------------------------------------*
 * declare local variables.
 *-------------------------------------------------------------------*/

  int century_day;
  int i, k, iyn, idy, iny;
  int months[13];

/*-------------------------------------------------------------------*
 * set number of days in a month for each month, with special
 * treatment for February.
 *-------------------------------------------------------------------*/

  months[0] = 0;
  months[1] = 31;

  if (year/4 * 4 != year) {
      months[2] = 28;
  } else {
      if (year/400 * 400 == year) {
          months[2] = 29;
      } else if (year/100 * 100 == year) {
          months[2] = 28;
      } else {
          months[2] = 29;
      }
  }

  months[3]  = 31;
  months[4]  = 30;
  months[5]  = 31;
  months[6]  = 30;
  months[7]  = 31;
  months[8]  = 31;
  months[9]  = 30;
  months[10] = 31;
  months[11] = 30;
  months[12] = 31;

/*-------------------------------------------------------------------*
 * same process as used in fortran routine DATE31.
 *-------------------------------------------------------------------*/

  k = 0;

  for (i = 1; i <= month; i++) {
    k = k + months[i];
  }

  k   = k - months[month];
  idy = k + day;
  iny = year;
  iyn = iny - 1900;

  if (iyn > 0) {
      century_day = idy + iyn*365 + (iyn-1)/4 - (iyn-1)/100 + (iyn+299)/400;
  } else {
      century_day = idy + iyn*365 + iyn/4 - iyn/100;
  }

/*-------------------------------------------------------------------*
 * return century day to calling program.
 *-------------------------------------------------------------------*/

  return(century_day);
}
