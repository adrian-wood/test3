/*+ gd_from_param_blocks.c
 *
 * Create a grid description from Fortran-style parameter blocks.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     19/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: gd_from_param_blocks.c, 1, 27/02/2007 16:16:23, Stan Kellett$";

/* Standard header files used */

#include <stdlib.h>

/* Package header files used */

#include <grid_description.h>
#include <portability.h>
#include <array.h>

GD  GD_From_Param_Blocks(
    
    /* IN */
    
    INTEGER indicator_params[4],             /* Params from indicator section */
    INTEGER gd_params[20],                   /* Params from grid description */
    REAL    real_params[20],                 /* Miscellaneous real params */
    REAL    vert_params[GD_MAX_VERT_PARAMS], /* Params describing vertical */
                                             /* coordinate */
    INTEGER num_vert_params,                 /* No. of vertical coord params */
    INTEGER quasi_lengths[GD_MAX_QUASI],     /* Row lengths, if grid type is */
                                             /* quasi-regular */
    INTEGER num_quasi_lengths                /* Number of row lengths given */
)

{

GD  gd = GD_New();

/* Copy the fixed size parameter blocks into the grid description */

Arr_Copy(gd->indicator_params, indicator_params, 4);
Arr_Copy(gd->params, gd_params, 20);
Arr_Copy(gd->real_params, real_params, 20);

/* Allocate memory for the vertical coordinate parameters and copy them */

gd->vert_params = calloc(num_vert_params, sizeof(*(gd->vert_params)));
Arr_Copy(gd->vert_params, vert_params, num_vert_params);
gd->num_vert_params = num_vert_params;

/* Allocate memory for the quasi-regular grid row lengths and copy them */

gd->quasi_lengths = calloc(num_quasi_lengths, sizeof(*(gd->quasi_lengths)));
Arr_Copy(gd->quasi_lengths, quasi_lengths, num_quasi_lengths);
gd->num_quasi_lengths = num_quasi_lengths;

return gd;

} /* end function GD_From_Param_Blocks */
