/*+ gd_free.c
 *
 * Disposes of a grid description previously created by GD_New
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     15/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: gd_free.c, 1, 27/02/2007 16:16:23, Stan Kellett$";

/* Standard header files used */

#include <stdlib.h>

/* Package header files used */

#include <grid_description.h>

void  GD_Free(

   /* INOUT */
   GD gd
)

{

if ( gd != NULL ) {

    /* gd points to a real grid description. Need to dispose of the
     * vert_params and quasi_lengths before the main structure. */
     
    free(gd->vert_params);
    free(gd->quasi_lengths);
    free(gd);
}

} /* end function GD_Free */
