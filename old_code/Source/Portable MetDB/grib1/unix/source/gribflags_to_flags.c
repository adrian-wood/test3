/*+ gribflags_to_flags.c
 *
 * Converts a set of GRIB flags to an array of Boolean values.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     23/1/97   Original code. (A. Edmunds)
 *
 * Converts a set of GRIB flags to an array of Boolean values.
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: gribflags_to_flags.c, 1, 27/02/2007 16:16:22, Stan Kellett$";

/* Package header files used */

#include <gribflags.h>

void  GribFlags_To_Flags(

    /* IN */
    GribFlags  gribflags,       /* Set of eight flags in coded form */
    
    /* OUT */
    Boolean    flags[7]         /* The flags as Boolean values */
)

{

int            index;           /* Flag number */
unsigned char  mask = 128;      /* Mask used to extract one flag */

for ( index = 0; index < 7; index++ ) {
    flags[index] = ( (gribflags & mask) != 0 );
    mask = mask / 2;
} /* end for */

} /* end function GribFlags_To_Flags */
