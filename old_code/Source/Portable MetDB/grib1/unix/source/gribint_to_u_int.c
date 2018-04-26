/*+ gribint_to_u_int.c
 *
 * Converts a GRIB integer to a host machine integer.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     23/1/97   Original code. (A. Edmunds)
 *
 * Converts a GRIB integer of length 'length' bytes to an unsigned integer
 * in the host machine native representation. For correct operation, it is
 * required that:
 *
 *    length <= sizeof(unsigned int)
 *
 * The code DOES NOT check this.
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: gribint_to_u_int.c, 1, 27/02/2007 16:16:21, Stan Kellett$";

/* Package header files used */

#include <gribint.h>

unsigned int  GribInt_To_U_Int(

    /* IN */
    GribInt  gribint,       /* Integer in coded form */
    int      length         /* Length of gribint in bytes */
)

{
int           index;        /* Byte number */
unsigned int  result = 0;   /* Integer in host machine native representation */

for ( index = 0; index < length; index++ ) {
    result = result << 8 | gribint[index];
} /* end for */

return result;

} /* end function GribInt_To_U_Int */
