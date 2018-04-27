/*+ gd_new.c
 *
 * Create and return a new grid description, or NULL if one cannot be created.
 * The contents of the grid description are undefined.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     15/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: gd_new.c, 1, 27/02/2007 16:16:22, Stan Kellett$";

/* Standard header files used */

#include <stdlib.h>

/* Package header files used */

#include <grid_description.h>

GD  GD_New(void)

{
GD  new = malloc(sizeof(struct GD_Struct));

return new;

} /* end function GD_New */
