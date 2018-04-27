#pragma linkage (system_cmd_,FORTRAN)
#pragma runopts(POSIX(ON))
/*----------------------------------------------------------------*
 * Program      : system_cmd
 *
 * Language     : C
 *
 * Description  : Uses system function system() to run the Unix
 *                command line given by "string".
 *               #pragma linkage is needed because this function is
 *                 called from Fortran.
 *               #pragma runopts is needed to run system() as a USS
 *                 function instead of a z/OS module.
 *
 * Called by    : mdbrsn
 *
 * Calls        : system() - system function
 *
 * Arguments    : (1) Unix command
 *
 * $Workfile: system_cmd.c$  $Folder: OpSource$
 * $Revision: 2$  $Date: 01/12/2010 10:52:42$
 *
 * Changes      :
 * $Log:
 *  2    MetDB_Refresh 1.1         01/12/2010 10:52:42    Sheila Needham
 *       Setting POSIX ON for system() command. Tidy up.
 *  1    MetDB_Refresh 1.0         12/11/2010 11:51:11    Sheila Needham
 *       Interface to Unix script for offline retrieval
 * $
 *
 *-----------------------------------------------------------------
 * (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
 *
 * Met Office, United Kingdom.
 *
 * The use, duplication and disclosure of this code is strictly
 * prohibited without the permission of The Meteorological Database
 * Team at the above address.
 *-----------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>

int system_cmd_(char*string) {

  int rc;

/* Run the Unix command and wait for it to end.  The rc is from
   the system() function, not the Unix script itself  */

  rc = system(string);

  if (rc != 0) printf("system_cmd:%s\n",string);

  return rc;
}
