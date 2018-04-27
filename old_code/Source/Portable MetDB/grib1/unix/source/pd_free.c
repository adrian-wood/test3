/*+ pd_free.c
 *
 * Disposes of a product definition previously created by PD_New.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     15/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: pd_free.c, 1, 27/02/2007 16:16:17, Stan Kellett$";

/* Standard header files used */

#include <stdlib.h>

/* Package header files used */

#include <product_definition.h>

void  PD_Free(

    /* INOUT */
    PD  pd
)

{

free(pd);

} /* end function PD_Free */
