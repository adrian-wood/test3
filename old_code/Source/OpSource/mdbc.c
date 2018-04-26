#if !defined F95
#pragma linkage(MDBC,FORTRAN)
#endif
#pragma runopts(POSIX(ON))
/*----------------------------------------------------------------*
 * Program      : mdbc(if called from VSF) or mdb_ (if called from
 *                user F95 program)
 *
 * Language     : C
 *
 * Description  : Interface between users MDB program and MDB DLL.
 *                #pragma linkage directive required for VSFortran to
 *                C calls but not for F95(C) to C.
 *
 * Called by    : mdb pre-processor (source mdbshel) or directly from
 *                F95 user program
 *
 * Calls        : mdbwrap  - C interface to MDB DLL
 *
 * Arguments    : as for MDB but with explicit lengths for cstr and
 *                crept and implied lengths for all character variables
 *                passed.
 *
 * $Workfile: mdbc.c$  $Folder: OpSource$
 * $Revision: 4$  $Date: 30/01/2012 18:38:30$
 *
 * Changes      :
 * $Log:
 *  4    MetDB_Refresh 1.3         30/01/2012 18:38:30    Sheila Needham
 *       Updated header file for f2003
 *  3    MetDB_Refresh 1.2         02/06/2011 14:14:40    Sheila Needham  Now
 *       callable directly from F95
 *  2    MetDB_Refresh 1.1         01/12/2010 10:52:59    Sheila Needham  Set
 *       POSIX(ON)
 *  1    MetDB_Refresh 1.0         09/11/2010 09:39:55    Sheila Needham  C
 *       function to load and call the MDB DLL, passing correct character
 *       length arguments on to the MDB main subroutine.  This becomes part of
 *        the MDBSHEL load module.
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

/* declare function to be called    */

extern int mdbwrap(char *ctype_, char *request_, Real *rvals_,
   Integer *nobs_, Integer *nelem_, Integer *status_,
   char *cstr_, char *crept_, int ctype_Len,
   int request_Len, int cstr_L, int crept_L);

/* This function, uppercase since it's called from Fortran */

#if defined F95
int mdb_(char *ctype_,
   char *request_, Real *rvals_, Integer *nobs_,
   Integer *nelem_, Integer *status_, char *cstr_,
   char *crept_, int ctype_Len,
   int request_Len, int cstr_L,int crept_L) {
#else
int MDBC(char *ctype_,
   char *request_, Real *rvals_, Integer *nobs_,
   Integer *nelem_, Integer *status_, char *cstr_,
   char *crept_, int cstr_L, int crept_L, int ctype_Len,
   int request_Len, int cstr_Len,int crept_Len) {
#endif

/* Initialise NAG fortran I/O environment  */

int argc;
char **argv;
f90_init(argc,argv);

/* Call MDB via C wrapper.  This loads MDBDLL (at entry point
   mdbwrap.  Pass explicit lengths of cstr and crept rather than
   ones implied by Fortran/C interface.                          */

mdbwrap(ctype_, request_, rvals_, nobs_, nelem_, status_, cstr_,
   crept_, ctype_Len, request_Len, cstr_L, crept_L);

return 0;
}
