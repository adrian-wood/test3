/*+ grib_handle_read_error.c
 *
 * Determine the cause of a read error.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     21/1/97   Original code. (A. Edmunds)
 *
 * Determine the cause of a read error. Return an appropriate status value
 * based on the error.
 *
 * Code description: ANSI C
 */
static char fileID[] = "$Header: grib_handle_read_error.c, 1, 27/02/2007 16:16:19, Stan Kellett$";

/* Standard header files used */

#include <stdio.h>

/* Package header files used */

#include <grib.h>

void  Grib_Handle_Read_Error(

    /* IN */
    FILE        *file_ptr,       /* File being read when error occurred */
    
    /* OUT */
    Grib_Error  *error_status    /* Error code */
)

{

/*-    End of header */

if ( feof(file_ptr) ) {

    /* The end of the file was reached before a complete GRIB message
     * could be read */
    
    *error_status = Grib_Incomplete;
} else {

    /* A system read error occured. */
    
    *error_status = Grib_Read_Error;
} /* end if */
 
} /* end function Grib_Handle_Read_Error */
