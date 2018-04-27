#pragma runopts(POSIX(ON))
/*----------------------------------------------------------------*
 * Program      : mdbwrap
 *
 * Language     : C
 *
 * Description  : Wrapper around the F95 routine MDB to initialise
 *                the NAG Fortran environment.
 *                No #program linkage directive here since C and F95
 *                use the same format for passing character arguments.
 *
 * Called by    : mdbshell
 *
 * Calls        : MDB
 *
 * Arguments    : as for MDB but with four extra integers giving
 *                the lengths of the character variables passed/
 *
 * $Workfile: mdbwrap.c$  $Folder: OpSource$
 * $Revision: 3$  $Date: 30/01/2012 11:26:17$
 *
 * Changes      :
 * $Log:
 *  3    MetDB_Refresh 1.2         30/01/2012 11:26:17    Sheila Needham
 *       Update Nag header file for 5.2 release (f2003h)
 *  2    MetDB_Refresh 1.1         01/12/2010 10:53:13    Sheila Needham  set
 *       POSIX(ON)
 *  1    MetDB_Refresh 1.0         08/11/2010 09:38:57    Sheila Needham  C
 *       wrapper to interface MDB DLL
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
#include <nagfortran.h>

/* Declare mdb subroutine including length arguments   */

extern int mdb(char *ctype_,
                  char *request_,
                  Real *rvals_,
                  Integer *nobs_,
                  Integer *nelem_,
                  Integer *status_,
                  char *cstr_,
                  char *crept_,
                  int ctype_Len,
                  int request_Len,
                  int cstr_Len,
                  int crept_Len);

int mdbwrap(char *ctype_,
            char *request_,
            Real *rvals_,
            Integer *nobs_,
            Integer *nelem_,
            Integer *status_,
            char *cstr_,
            char *crept_,
            int ctype_Len,
            int request_Len,
            int cstr_Len,
            int crept_Len) {

/* Initialise NAG fortran environment including I/O subsystem  */

  int argc;
  char **argv;
  f90_init(argc,argv);

/* call MDB                     */

  mdb_(ctype_,
          request_,
          rvals_,
          nobs_,
          nelem_,
          status_,
          cstr_,
          crept_,
          ctype_Len,
          request_Len,
          cstr_Len,
          crept_Len);
  return 0;
}
