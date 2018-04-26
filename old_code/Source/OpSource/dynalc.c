/*----------------------------------------------------------------*
 * Program      : dynalc
 *
 * Language     : C
 *
 * Description  : Uses system function dynalloc() to dynamically
 *                connect a dataset name to a ddname.
 *
 * Called by    : various
 *
 * Calls        : dynalloc() - system function
 *
 * Arguments    : (1) dataset name terminated by null character
 *                (2) ddname terminated by null character
 *
 * $Workfile: dynalc.c$  $Folder: OpSource$
 * $Revision: 1$  $Date: 08/11/2010 09:38:15$
 *
 * Changes      :
 * $Log:
 *  1    MetDB_Refresh 1.0         08/11/2010 09:38:15    Sheila Needham
 *       Replacement for assembler dynalloc routine
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
#include <string.h>
#include <dynit.h>

void dynalc_(char* alloc_dsn, char* alloc_ddname) {

  __dyn_t ip;
  int ret;
  dyninit(&ip);
  ip.__ddname   = alloc_ddname;
  ip.__dsname   = alloc_dsn;
  ip.__status   = __DISP_SHR;

  if ((ret = dynalloc(&ip)) != 0)
    printf("dynalc dsn:%s ddname%s : dynalloc() ret=%d, error code %04x,info code %04x\n",
            alloc_dsn, alloc_ddname,ret, ip.__errcode, ip.__infocode);
}
