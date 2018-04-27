/*+ pd_from_param_blocks.c
 *
 * Create a new product definition from Fortran-style parameter blocks.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     19/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: pd_from_param_blocks.c, 1, 27/02/2007 16:16:17, Stan Kellett$";

/* Package header files used */

#include <product_definition.h>
#include <portability.h>
#include <array.h>

PD  PD_From_Param_Blocks(

    /* IN */
    INTEGER  indicator_params[4],
    INTEGER  pd_params[PD_MAX_ELEMS]
)

{
PD  pd = PD_New();

Arr_Copy(pd->indicator_params, indicator_params, 4);
Arr_Copy(pd->params, pd_params, PD_MAX_ELEMS);

return pd;

} /* end function PD_From_Param_Blocks */
