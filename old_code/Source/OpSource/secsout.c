#pragma linkage (secsout_,FORTRAN)
#pragma runopts(POSIX(ON))
/*----------------------------------------------------------------*
 * Program      : secsout
 *
 * Language     : C
 *
 * Description  : To wait a given number of seconds.
 *                Pragma linkage needed for Fortran calls.
 *                POSIX(ON) so that this can be called from z/OS. 
 *
 * Called by    : Fortran programs
 *
 * Calls        : Unix function sleep
 *
 * Arguments    : (1) number of seconds to wait
 *
 * $Workfile: secsout.c$  $Folder: OpSource$
 * $Revision: 2$  $Date: 21/01/2011 13:09:46$
 *
 * Changes      :
 * $Log:
 *  2    MetDB_Refresh 1.1         21/01/2011 13:09:46    Sheila Needham
 *       Updated comments
 *  1    MetDB_Refresh 1.0         21/01/2011 13:06:31    Sheila Needham
 *       Initial check-in
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

int secsout_(int nsecs) {
  sleep( nsecs );
  return 0;
}
