/*-------------------------------------------------------------------*
 * Program      : prognum_details.h                                      
 *
 * Language     : C
 *
 * Description  : MetDB RPC broker/freepn server include file
 *              : containing structure for program number file info. 
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
 * Revision 1.1  98/02/03  10:00:51  10:00:51  usmdb (Generic MDB account)
 * Initial revision
 * 
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/prognum_details.h,v $
 *-------------------------------------------------------------------*/

struct prognum_details{
  int confirm;
  int status;
  int year;
  int month;
  int day;
  int hour;
  int minute;
  int second;
};
