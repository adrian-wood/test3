/*+ process_grib_file.c
 *
 * Decode one file of GRIB messages an output them in textual form
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     11/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 */
static char fileID[] = "$Header: process_grib_file.c, 2, 27/02/2007 17:02:17, Stan Kellett$";

/* Standard header files used */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

/* Package header files used */

#include <boolean.h>
#include <grib.h>
#include <portability.h>
#include <product_definition.h>
#include <grid_description.h>

int  Process_Grib_File(

    /* IN */
    FILE     *file_ptr,      /* File to read from */
    Boolean  headers_reqd,   /* True iff header information should be output */
    Boolean  data_reqd,      /* True iff data should be output */
    char     *prog_name,     /* Name of this program */
    char     *filename,      /* Name of file refered to by file_ptr */
    int      message_level   /* Level of messages required */
)

{

Grib        mesg;                     /* The current Grib message */
Grib_Error  read_status = Grib_Success; /* Status code from read operation */
INTEGER     indicator_params[4];      /* Parameters from indicator section */
PD          product_def;              /* Product definition from current mesg */
GD          grid_desc;                /* Grid description from current mesg */
INTEGER     bitmap_params[2];         /* Parameters from bitmap section */
INTEGER     *bitmap;                  /* Bitmap from current message */
INTEGER     num_bitmap_entries;       /* Number of elements in the bitmap */
INTEGER     data_params[2];           /* Parameters from data section */
REAL        *data;                    /* Data from current message */
INTEGER     num_data_items;           /* Number of data items */
REAL        real_params[20];          /* Miscellaneous real parameters */
Grib_Error  get_status;               /* Status code from 'Get' operation */
INTEGER     index;                    /* Index into data array */
int         error_status = EXIT_SUCCESS; /* Status code for processing of the */
                                         /* whole file */
int         message_no   = 0;       /* No. of current message within the file */

/*- End of header */

bitmap = (INTEGER *) malloc( ((size_t)GRIB_MAX_ITEMS ) * sizeof(INTEGER)) ; 
if ( bitmap == NULL ) {
  read_status = Grib_Read_Error ; 
  fprintf(stderr, "%s: Unable to allocate memory for GRIB bitmap with size "
                    "%d INTEGERs\n", prog_name,GRIB_MAX_ITEMS);
  error_status = EXIT_FAILURE;
}

data = (REAL *) malloc( ((size_t)GRIB_MAX_ITEMS ) * sizeof(REAL)) ; 
if ( data == NULL ) {
  read_status = Grib_Read_Error ; 
  fprintf(stderr, "%s: Unable to allocate memory for GRIB data with size "
                    "%d REALs\n", prog_name,GRIB_MAX_ITEMS);
  error_status = EXIT_FAILURE;
}

while ( read_status == Grib_Success || read_status == Grib_Too_Long ) {

    /* Read a message from the file */

    Grib_Read(file_ptr, &mesg, &read_status);
    message_no++;
    
    if ( read_status == Grib_Success ) {

        /* Get all the information from the message */
    
        Grib_Get_All(mesg, message_level, indicator_params, &product_def,
            &grid_desc, bitmap_params, bitmap, &num_bitmap_entries, data_params,
            data, &num_data_items, real_params, &get_status);
            
        if ( get_status == Grib_Success ) {
            if ( headers_reqd ) {
        
                /* Header information was requested, so output it */
            
                PD_Print(product_def);
                GD_Print(grid_desc);
            } /* end if */
            if ( data_reqd ) {
        
                /* Data was requested, so output it */
            
                printf("Data\n");
                printf("----\n");
                printf("\n");
                for ( index = 0; index < num_data_items; index++ ) {
                    printf("%6d : %f\n", index + 1, data[index]);
                } /* end for */
                printf("\n");
            } /* end if */
        
            /* Dispose of data structures created by the Get */
        
            PD_Free(product_def);
            GD_Free(grid_desc);
        } else {
        
            /* 'Get' failed */
            
            fprintf(stderr, "%s: Invalid or corrupted GRIB message found "
                "(message no. %d in file %s)\n", prog_name, message_no,
                filename);
            error_status = EXIT_FAILURE;
        } /* end if */
    } else {
    
        /* Read failed */
        
        if ( message_level < 3 ) {

            /* Output an approriate error message based on the status code */
        
            switch ( read_status ) {
            case Grib_Too_Long:
                fprintf(stderr, "%s: Unable to read GRIB message longer than "
                    "%d bytes\n(message no. %d in file %s)\n", prog_name,
                    GRIB_MAX_BYTES, message_no, filename);
                break;
            case Grib_Incomplete:
                fprintf(stderr, "%s: Incomplete GRIB message found "
                    "(message no. %d in file %s)\n", prog_name, message_no,
                    filename);
                break;
            case Grib_Read_Error:
                fprintf(stderr, "%s: %s (message no. %d in file %s)\n",
                    prog_name, strerror(errno), message_no, filename);
                break;
            } /* end switch */
        } /* end if */
        error_status = EXIT_FAILURE;
    }/* end if */
    
    /* Dispose of the current message. */
    
    if (read_status == Grib_Success) Grib_Free(mesg);
    
} /* end while */

if ( bitmap != NULL ) free(bitmap);
if ( data != NULL ) free(data);

return error_status;

} /* end function Process_Grib_File */
