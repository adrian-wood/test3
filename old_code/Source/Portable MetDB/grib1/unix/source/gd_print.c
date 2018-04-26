/*+ gd_print.c
 *
 * Output a grid description in human-readable form.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     20/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: gd_print.c, 1, 27/02/2007 16:16:22, Stan Kellett$";

/* Standard header files used */

#include <stdio.h>

/* Package header files used */

#include <grid_description.h>
#include <portability.h>

void GD_Print(

    /* IN */
    GD gd
)

{
INTEGER  index;           /* Index into vert_params and quasi_lengths arrays */

/* Print the basic grid description using Fortran routine PRTBK2 */

prtbk2(gd->indicator_params, gd->params, gd->real_params);
printf("\n");

if ( gd->num_vert_params > 0 ) {

    /* Grid description includes vertical coordinate parameters. Print them. */
    
    printf("Vertical Coordinate Parameters\n");
    printf("------------------------------\n");
    printf("\n");
    for ( index = 0; index < gd->num_vert_params; index++ ) {
        printf("%6d : %f\n", index + 1, gd->vert_params[index]);
    } /* end for */
    printf("\n");
} /* end if */

if ( gd->num_quasi_lengths > 0 ) {

    /* Grid type is quasi-regular. Print row lengths. */
    
    printf("Quasi-regular grid description\n");
    printf("------------------------------\n");
    printf("\n");
    for ( index = 0; index < gd->num_quasi_lengths; index++ ) {
        printf("%6d : %d\n", index + 1, gd->quasi_lengths[index]);
    } /* end for */
    printf("\n");
} /* end if */

} /* end function GD_Print */
