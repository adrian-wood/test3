/*+ grib_free.c
 *
 * Disposes of a Grib object previously created by Grib_New
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     11/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: grib_free.c, 1, 27/02/2007 16:16:20, Stan Kellett$";

/* Standard header files used */

#include <stdlib.h>

/* Package header files used */

#include <grib.h>

void  Grib_Free(

    /* INOUT */
    Grib  mesg
)

{

free(mesg);

} /* end function Grib_Free */
