/* part_string.c
 *
 * Description:
 *	Function to return the substring of a string entered into the function
 *
 * ANSI C conforming to Met.Office Programming Standard 1.0
 *
 * History:
 *   version   date          comment
 *     1.0    23/3/2000      Initial Version
 *
 *   $log: $
 *
 * Information:
 *       Arguments that need to passed to this function is the original string
 *       the start and end indexes of the substring required and the function
 *       returns the substring.
 *
 * Revision Control:
 *
 * $Revision: 2$
 * $Date: 03/02/2014 16:22:01$
 * $Source: /home/h01/usmdb/station_search/source/RCS/part_string.c,v $
 *
 * $Log:
 *  2    Met_DB_Project 1.1         03/02/2014 16:22:01    John Norton
 *       Changed references to us0400 to use h01.
 *  1    Met_DB_Project 1.0         13/11/2006 16:33:49    Kudsia Gwangwaa 
 * $
 * Revision 1.1  2000/06/08  15:48:25  15:48:25  usmdb (Generic MDB account)
 * Initial revision
 * 
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *part_string(const char *long_string,char *sub_string, int start_index, int end_index)
	{
/* Declare and initialise variables */
	int i;
	int sub_length=(end_index-start_index);
        int long_length=strlen(long_string);

/* check if valid arguments sent to function */
        if ((end_index >= start_index)&&(sub_length <= long_length))
	   {
/* create the sub_string to return */
	   for (i=0; i<=sub_length; i++)
	      {
	      sub_string[i] = long_string[i+start_index];
	      } 
              sub_string[sub_length+1]='\0';
	   }
	   else
	      {
/* return the empty string value */
              sub_string[0]='\0';
	      }
           return(sub_string);
           }
