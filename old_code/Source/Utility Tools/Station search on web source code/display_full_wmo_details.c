/*
 * display_full_wmo_details
 * Main program to display the details of a staion from full station master
 * written by S.Kellett, Aprilh 2000
 * Copyright The Met.Office April 2000
 *
 * Revision Control:
 *
 * $Revision: 1$
 * $Date: 31/08/2006 16:04:32$
 * $Source: /home/us0400/usmdb/station_search/source/RCS/display_full_wmo_details.c,v $
 *
 * $Log:
 *  1    Met_DB_Project 1.0         31/08/2006 16:04:32    Stan Kellett    
 * $
 * Revision 1.1  2000/06/08  15:42:35  15:42:35  usmdb (Generic MDB account)
 * Initial revision
 * 
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "/home/us0400/usmdb/station_search/headers/full.h"

main(int argc,char *argv[])
	{
  int status;
  char title[]="Full Station Master Details";
  char *wmo_no=argv[1];
  status=write_html_head(title,strlen(title)); 
  printf("<BR> \r\n");
      write_full_wmo_details(wmo_no);
  printf("<BR> \r\n");
  status=write_html_tail();
	}
