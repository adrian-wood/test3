/*----------------------------------------------------------------*
 * Function     : check_auth    (CHKAUTH in MDB.RETRIEVL.CSRCE)
 *
 * Language     : C
 *
 * Description  : Looks at mdb_config file  to check a user has access
 *                to a given MetDB data subtype.
 *
 * Called by    : MSRPC2
 *
 * Calls        : nothing
 *
 * Arguments    : (1) Userid   (in)
 *                (2) Data type(in)
 *                (3) Return code (out)
 *                       0 - access given (none required)
 *                       1 - access given (required)
 *                      -1 - access denied
 *
 * $Workfile: check_auth.c$  $Folder: Main$
 * $Revision: 3$  $Date: 19/07/2011 15:34:40$
 *
 * Changes      :
 * $Log:
 *  3    MetDB_Refresh 1.2         19/07/2011 15:34:40    Sheila Needham
 *       Remove print statement and give access if no config file available
 *  2    MetDB_Refresh 1.1         13/07/2011 10:52:30    Sheila Needham
 *       Removed prints and set the operational config filename
 *  1    MetDB_Refresh 1.0         13/07/2011 09:20:05    Sheila Needham  New
 *       module for restricted access
 * $
 *
 *-----------------------------------------------------------------
 * (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
 *
 * Met Office, United Kingdom.
 *
 * The use, duplication and disclosure of this code is strictly
 * prohibited without the permission of The Meteorological Database
 * Team at the above address.
 *-----------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAXCHAR 200
#define MAXrecord 50

void check_auth(char *uid, char *ctype, int *irc) {

  char record[MAXCHAR];                   /* one record             */
  static char table[MAXrecord][MAXCHAR];  /* array of records       */
  char filename[] = "/usr/local/mdb/mdb_config.log";
  FILE *svfile;
  char *found;                            /* for string functions   */
  static int nrecs = 0;                   /* no. of records in d/s  */
  static int first = 0;                   /* flag set when d/s read */

  if (first == 0) {
    if (( svfile = fopen(filename,"r")) == NULL) {
      perror("error opening file");
      *irc = 0;  /* access given if no config file available */
      return;
    }

    while (fgets(record, MAXCHAR, svfile) != NULL)
      strncpy(table[nrecs++],record,MAXCHAR);
      first = 1;
    fclose(svfile);
  }
/*
   First check whether config checking is on or off; if it's off
   give access to all.
                                                     */
  for (int k=0; k < nrecs; k++) {
    found = strstr(table[k],"CONFIG SETTING");
    if (found != NULL ) {
      found = strstr(table[k],"---ON");
      if (found == NULL){
        printf("Config settings turned off\n");
        *irc = 0;
        return;
      }
    }
  }

  for (int k=0; k < nrecs; k++) {
/*
   Look for matching data subtype record
                                                  */
    found = strstr(table[k],ctype);
    if ( found !=NULL ) {
/*
   Now check the user is listed for access
                                                  */
      found = strstr(table[k],uid);
      if (found != NULL ) {
        *irc = 1;
        return;
      } else {
        *irc = -1;
        return;
      }
    }
  }
  *irc = 0;
  return;
}

