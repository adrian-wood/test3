/*----------------------------------------------------------------*
 * Program      : submit
 *
 * Language     : C
 *
 * Description  : Opens a connection to DD:INTOUT and writes
 *                JCL contained in char array.
 *
 * Called by    : cartsub
 *
 * Calls        : standard C functions
 *
 * Arguments    : (1) jcl     pointer to char array
 *              : (2) nlines  number of lines of JCL
 *              : (3) rc      error code = -3 if open failed
 *              :     jcllen  length of jcl, passed by Fortran
 *
 * Files        : //INTOUT DD SYSOUT=(A,INTRDR)
 *
 * $Workfile: submit.c$  $Folder: OpSource$
 * $Revision: 2$  $Date: 14/02/2012 09:13:56$
 *
 * Changes      :
 * $Log:
 *  2    Met_DB_Project 1.1         14/02/2012 09:13:56    Sheila Needham
 *       Replaces Fortran version for F95 compatability.
 *  1    Met_DB_Project 1.0         24/01/2012 11:36:26    Sheila Needham
 *       Initial version
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
void submit_(char* jcl, int* lines, int *rc, int jcllen) {

  char* jclptr;
  char newline[]="\n";
  FILE *fp;

/* initialise jclptr to the start of the array (NB addresses) */

  jclptr=jcl;
  *rc = 0;

/* Open INTOUT file for writing          */

  if (( fp=fopen("DD:INTOUT","w,recfm=fb")) == NULL ) {
    perror("Open error on INTOUT in submit");
    *rc = -3;
    return;
  }
/* loop over lines of JCL writing to INOUT                 */

  for (int i=0; i<*lines ; i++) {
    fwrite(jclptr,jcllen,1,fp);
    fwrite(newline,1,1,fp);
    jclptr+=jcllen;
  }
  fclose(fp);
  return;
}
