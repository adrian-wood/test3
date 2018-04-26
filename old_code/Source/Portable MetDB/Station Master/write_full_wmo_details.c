/* write_full_wmo_details.c
 *
 * Location: /home/h01/usmdb/station_search/source/
 *
 * Description:
 *	Function to search Full Station Master for a wmo number and then print
 * 	the details.
 *
 * ANSI C conforming to Met.Office Programming Standard 1.0
 *
 * History:
 *   version   date          comment
 *     1.0    22/3/2000      Initial Version
 *
 *   $log: $
 *
 * Information:
 *        To call this function an argument for the wmo number must be
 *        passed which is declared as a character*5 pointer.
 *       Also passed to the function is the file pointer
 *
 * Revision Control:
 *
 * $Revision: 2$
 * $Date: 03/02/2014 16:22:01$
 * $Source: /home/h01/usmdb/station_search/source/RCS/write_full_wmo_details.c,v $
 * 
 * $Log:
 *  2    Met_DB_Project 1.1         03/02/2014 16:22:01    John Norton
 *       Changed references to us0400 to use h01.
 *  1    Met_DB_Project 1.0         13/11/2006 16:33:35    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2006/08/29 08:16:07  usmdb
 * changed mdb folder to mdb_new folder.
 * Stan Kellett.
 *
 * Revision 1.1  2000/06/08 15:06:15  usmdb
 * Initial revision
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "/home/h01/usmdb/station_search/headers/full.h"


int write_full_wmo_details(char *wmo_no)
	{
/* Declare and initialise variables */
 int status;
 int count;
 char *station_num;
 char *station_name;
 char *country;
 char *region;
 char *icao_id;
 char *dcnn;
 char *rain_num;
 char *latitude;
 char *longitude;
 char *bar_height;
 char *airfield_height;
 char *opened;
 char *closed;
 char *characteristics;
 char *classification;
 char *hawson_00;
 char *hawson_12;
 char *map_pri;
 char *runway_1;
 char *runway_2;
 char *old_rec;
 char *new_rec;
 char *ua_times;
 char *syn_times;
 char *ncm_times;
 char *syn_rep;
 char *ncm_rep;
 char *prac_no;
 char *stn_days;
 char *five_deg;
 char *chaser;
 char *rec_no;
 FILE *fp;
 char details[BUFLEN];
 char check;
 char full_stnmas[]="/home/h01/mdb_new/op/data/MDB.STNMAS.FULLLST";
/* Allocate Memory */
 station_num=malloc(8);
 station_name=malloc(45);
 country=malloc(40);
 region=malloc(3);
 icao_id=malloc(6);
 dcnn=malloc(5);
 rain_num=malloc(7);
 latitude=malloc(6);
 longitude=malloc(7);
 bar_height=malloc(7);
 airfield_height=malloc(7);
 opened=malloc(10);
 closed=malloc(10);
 characteristics=malloc(25);
 classification=malloc(3);
 hawson_00=malloc(6);
 hawson_12=malloc(6);
 map_pri=malloc(4);
 runway_1=malloc(6);
 runway_2=malloc(6);
 old_rec=malloc(6);
 new_rec=malloc(6);
 ua_times=malloc(5);
 syn_times=malloc(25);
 ncm_times=malloc(5);
 syn_rep=malloc(14);
 ncm_rep=malloc(24);
 prac_no=malloc(2);
 stn_days=malloc(10);
 five_deg=malloc(5);
 chaser=malloc(7);
 rec_no=malloc(6);
/* Check that full master can be opened and then open the file for 
 * reading
 */
 if ((fp=fopen(full_stnmas,"r"))!=NULL)
   {
/* Get the first line of file which just holds header information */
   fgets(details,BUFLEN,fp);
/* Loop to the end of the file */
   while ( (!feof(fp)) )
	{
/* Read the line in from the current Station */
  	fgets(details,BUFLEN,fp);
   	station_num=part_string(details,station_num,1,5);
	if (strcmp(station_num,wmo_no) == 0)
	   {
/* Now set each element of Station Details for printing to Web page */
           station_name=part_string(details,station_name,39,80);
           country=part_string(details,country,82,113);
	   region=part_string(details,region,115,116);
           icao_id=part_string(details,icao_id,7,10);
	   dcnn=part_string(details,dcnn,118,121);
	   rain_num=part_string(details,rain_num,123,128);
           latitude=part_string(details,latitude,12,16);
           longitude=part_string(details,longitude,18,23);
	   bar_height=part_string(details,bar_height,25,30);
  	   airfield_height=part_string(details,airfield_height,32,37);
	   opened=part_string(details,opened,130,137);
	   closed=part_string(details,closed,139,146);
	   characteristics=part_string(details,characteristics,148,171);
	   classification=part_string(details,classification,173,174);
	   hawson_00=part_string(details,hawson_00,176,180);
	   hawson_12=part_string(details,hawson_12,182,186);
	   map_pri=part_string(details,map_pri,188,190);
	   runway_1=part_string(details,runway_1,192,196);
	   runway_2=part_string(details,runway_2,198,202);
	   old_rec=part_string(details,old_rec,204,208);
	   new_rec=part_string(details,new_rec,210,214);
	   ua_times=part_string(details,ua_times,216,219);
	   syn_times=part_string(details,syn_times,272,295);
	   ncm_times=part_string(details,ncm_times,298,301);
	   syn_rep=part_string(details,syn_rep,221,233);
	   ncm_rep=part_string(details,ncm_rep,235,257);
	   prac_no=part_string(details,prac_no,259,260);
	   stn_days=part_string(details,stn_days,262,270);
	   five_deg=part_string(details,five_deg,673,676);
	   chaser=part_string(details,chaser,677,682);
	   rec_no=part_string(details,rec_no,684,688);
/* Print each element of Station Details to Web Page */
           printf(" <H2> %s:</H2>\r\n",station_name);
           printf("<BR><B>Country:</B> %s\r\n",country);
	   printf("<BR><B>Region:</B> %s\r\n",region);
	   printf("<BR><B>WMO:</B> %s\r\n",wmo_no);
	   printf("<BR><B>ICAO:</B> %s\r\n",icao_id);
	   printf("<BR><B>DCNN (District County and stn,</B> <I>UK</I><B>):</B>	%s \r\n",dcnn);
 	   printf("<BR><B>Rainfall Number</B> <I>UK</I><B>:</B> %s\r\n",rain_num);
	   printf("<BR><B>Latitude:</B> %s\r\n",latitude);
	   printf("<BR><B>Longitude:</B> %s\r\n",longitude);
	   printf("<BR><B>Height of Barometer:</B> %s metres\r\n",bar_height);
	   printf("<BR><B>Height of Airfield:</B> %s metres\r\n",airfield_height);
	   printf("<BR><B>Started Observing:</B> %s\r\n",opened);
	   printf("<BR><B>Stopped Observing:</B> %s\r\n",closed);
	   printf("<BR><B>Station Characteristics:</B> %s\r\n",characteristics);
	   printf("<BR><B>Station Classification:</B> %s\r\n",classification);
	   printf("<BR><B>Hawson 00Z :</B> %s\r\n",hawson_00);
	   printf("<BR><B>Hawson 12Z :</B> %s\r\n",hawson_12);
	   printf("<BR><B>Map Priority:</B> %s\r\n",map_pri);
	   printf("<BR><B>Runway Direction 1:</B> %s\r\n",runway_1);
	   printf("<BR><B>Runway Direction 2:</B> %s\r\n",runway_2);
	   printf("<BR><B>Old Station Record:</B> %s\r\n",old_rec);
	   printf("<BR><B>New Station Record:</B> %s\r\n",new_rec);
	   printf("<BR><B>Upper Air Reporting Times:</B> %s\r\n",ua_times);
	   printf("<BR><B>Synop Reporting Times:</B> %s\r\n",syn_times);
	   printf("<BR><B>NCM Reporting Times:</B> %s\r\n",ncm_times);
	   printf("<BR><B>Synop Elements Reported</B><I>(UK only)</I><B>:</B> %s\r\n",syn_rep);
	   printf("<BR><B>NCM Elements Reported</B><I>(UK only)</I><B>:</B> %s\r\n",ncm_rep);
	   printf("<BR><B>Number of practises to follow (max=10):</B> %s\r\n",prac_no);
	   printf("<BR><B>Days:</B> %s\r\n",stn_days);
	   printf("<BR><B>Five Degree Square Number:</B> %s\r\n",five_deg);
	   printf("<BR><B>Chaser Bulletin </B><I>(used by MetDB)</I><B>:</B> %s\r\n",chaser);
	   printf("<BR><B>Record Number </B><I>(used by MetDB)</I><B>:</B> %s\r\n",rec_no);
	   }
	}
   }
/* Free memory allocation */
   free(station_num);
   free(station_name);
   free(country);
   free(region);
   free(icao_id);
   free(dcnn);
   free(rain_num);
   free(latitude);
   free(longitude);
   free(bar_height);
   free(airfield_height);
   free(opened);
   free(closed);
   free(characteristics);
   free(classification);
   free(hawson_00);
   free(hawson_12);
   free(map_pri);
   free(runway_1);
   free(runway_2);
   free(old_rec);
   free(new_rec);
   free(ua_times);
   free(syn_times);
   free(ncm_times);
   free(syn_rep);
   free(ncm_rep);
   free(prac_no);
   free(stn_days);
   free(five_deg);
   free(chaser);
   free(rec_no);
/* Close any file which have been opened */
   fclose(fp);
       }
