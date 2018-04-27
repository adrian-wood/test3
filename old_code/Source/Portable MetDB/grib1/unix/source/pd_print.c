/*+ pd_print.c
 *
 * Output a product definition in human-readable form.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     19/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: pd_print.c, 1, 27/02/2007 16:16:16, Stan Kellett$";

/* Package header files used */

#include <product_definition.h>

void PD_Print(

    /* IN */
    PD  pd
)

{

/* Print the product definition using Fortran routine PRTBK1 */

prtbk1(pd->indicator_params, pd->params);

} /* end function PD_Print */
