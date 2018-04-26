/*+ grib_new.c
 *
 * Create and return a new Grib object, or NULL if an object cannot be created
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     11/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: grib_new.c, 1, 27/02/2007 16:16:19, Stan Kellett$";

/* Standard header files used */

#include <math.h>
#include <stdlib.h>

/* Package header files used */

#include <grib.h>
#include <portability.h>

Grib  Grib_New(void)

{
INTEGER  num_elems;
Grib     new;

/* Allocate enough memory for a message GRIB_MAX_BYTES in length in coded form*/

num_elems = ceil( (float) GRIB_MAX_BYTES / (float) sizeof(*new) );
new = calloc(num_elems, sizeof(*new));

return new;

} /* end function Grib_New */
