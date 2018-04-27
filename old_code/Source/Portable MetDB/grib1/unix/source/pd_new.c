/*+ pd_new.c
 *
 * Create and return a new product definition with undefined contents, or NULL
 * if one cannot be created.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     15/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: pd_new.c, 1, 27/02/2007 16:16:16, Stan Kellett$";

/* Standard header files used */

#include <stdlib.h>

/* Package header files used */

#include <product_definition.h>

PD  PD_New(void)

{
PD  new = malloc(sizeof(struct PD_Struct));

return new;

} /* end function PD_New */
