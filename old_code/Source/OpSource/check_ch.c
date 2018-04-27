/*----------------------------------------------------------------*
 * Program      : check_ch
 *
 * Language     : C
 *
 * Description  : Replaces non-print characters by spaces in the 
 *                users CSTR array.
 *                
 *
 * Called by    : mdb
 *
 * Calls        : isprint() function
 *
 * Arguments    : (1) pointer to character string 
 *                (2) length of string (i.e. string length * NOBS)
 *
 * $Workfile: check_ch.c$  $Folder: OpSource$
 * $Revision: 1$  $Date: 02/05/2012 10:47:28$
 *
 * Changes      :
 * $Log:
 *  1    Met_DB_Project 1.0         02/05/2012 10:47:28    Sheila Needham  New
 *       function to replace non-print characters with spaces.
 * $	
 *
 *-----------------------------------------------------------------
 * (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
 *
 * Met Office, United Kingdom.
 *
 * The use, duplication and disclosure of this code is strictly
 * prohibited without the permission of The Meteorological Database
 * Team at the above address.
 *-----------------------------------------------------------------*/
#include <stdio.h>
#include <ctype.h>
void check_ch_(char* text, int* size) {

  char* chptr;

/* initialise chptr to the start of the array (NB addresses) */

  chptr=text;

  for (int i=0; i<*size; i++) {
    if (!isprint(*chptr)) *chptr=' ';
    chptr+=1;
  }
return;
}
