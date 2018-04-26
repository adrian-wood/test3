/*+ grib_get_all.c
 *
 * Decode a GRIB message and return all the data
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     5/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 $log$
 */
static char fileID[] = "$Header: grib_get_all.c, 1, 27/02/2007 16:16:20, Stan Kellett$";

/* Standard header files used */

#include <math.h>

/* Package header files used */

#include <grib.h>
#include <portability.h>
#include <product_definition.h>
#include <grid_description.h>
#include <boolean.h>

void Grib_Get_All(

    /* IN */
    Grib    mesg,                       /* GRIB message */
    int     message_level,              /* Message level */
                                       /* (none/errors/warnings/notes) */
    /* OUT */
    INTEGER indicator_params[4],        /* Parameters from indicator section */
    PD      *pd,                        /* Product description */
    GD      *gd,                        /* Grid descrition */
    INTEGER bitmap_params[2],           /* Parameters from bitmap section */
    INTEGER bitmap[GRIB_MAX_ITEMS],     /* Decoded bitmap */
    INTEGER *num_bitmap_entries,        /* Number of entries in bitmap */
    INTEGER data_params[2],             /* Parameters from data section */
    REAL    data[GRIB_MAX_ITEMS],       /* Decoded data */
    INTEGER *num_data_items,            /* Number of data items in message */
    REAL    real_params[20],            /* Various real parameters */
    Grib_Error  *error_status          /* Error status */
)

{

/* Local scalars */

/* These are really constants, but have to be declared as variables because
 * they are passed by reference to the Fortran routine DECODE and so must
 * exist in memory at run-time.
 */

INTEGER  max_items       = GRIB_MAX_ITEMS;
INTEGER  max_vert_params = GD_MAX_VERT_PARAMS;
INTEGER  max_quasi       = GD_MAX_QUASI;

/* Fortran unit to send error messages to */
INTEGER  error_unit      = 6;

/* Size of one element of the mesg array (in bits) */
INTEGER  elem_size       = 8 * sizeof(*mesg);

/* Number of elements in the mesg array */
INTEGER  max_elems = ceil( (float)GRIB_MAX_BYTES / (float)sizeof(*mesg) );

/* Offset in the message at which to start decoding */
INTEGER  offset          = 0;

/* True variables */

int      index;
INTEGER  num_vert_params   = 0;   /* No. vertical coordinate params returned */
INTEGER  num_quasi_lengths = 0;   /* Number of row lengths, if grid type is */
                                  /* quasi-regular */
INTEGER  decode_status     = 1;   /* Status returned by decode operation */
INTEGER  data_size         = 0;   /* Number of bits which were used to encode */
                                  /* each data item */
INTEGER  elems_decoded     = 0;   /* Number of elements of mesg decoded */

/* Local arrays */

INTEGER  area_to_extract[4];                 /* Geographical area to extract */
                                             /* data for */
INTEGER    pd_params[PD_MAX_ELEMS];          /* Product definition params */
INTEGER    gd_params[20];                    /* Grid description params */
REAL       vert_params[GD_MAX_VERT_PARAMS];  /* Vertical coordinate */
                                             /* parameters */
INTEGER    quasi_lengths[GD_MAX_QUASI];      /* Row lengths, if grid type*/
                                             /* is quasi-regular */

REAL       workspace[GRIB_MAX_ITEMS];        /* Workspace arrays used by */
INTEGER    workspace_1[GD_MAX_COLUMNS];      /* decode routine */
INTEGER    workspace_2[2 * GD_MAX_ROWS];
REAL       workspace_real[GD_MAX_COLUMNS];

/*- End of header */

/* Initialise real_params to reasonable default values, since decode does not
 * assign to it in all cases. */

for( index = 0; index < 20; index++ ) {
    real_params[index] = 0;
}

area_to_extract[0] = 0;       /* Extract data for the whole */
area_to_extract[1] = 0;       /* geographical area */
area_to_extract[2] = 0;
area_to_extract[3] = 0;
data_params[1]     = 0;       /* Don't convert to SI units */

/* Call the Fortran routine to extract all the information */

decode(data, workspace, &max_items, num_data_items,
    vert_params, &max_vert_params, &num_vert_params,
    bitmap, &max_items, num_bitmap_entries,
    quasi_lengths, &max_quasi, &num_quasi_lengths,
    &data_size, &elem_size, indicator_params, pd_params,
    gd_params, bitmap_params, data_params, real_params,
    mesg, &max_elems, area_to_extract, &elems_decoded, &offset,
    &decode_status, workspace_1, workspace_2, workspace_real, &error_unit,
    &message_level);

if ( decode_status <3 ) {               /* !1.2 */

    /* Successful decode. Build a product description from some of the
     * information returned */
    
    *pd = PD_From_Param_Blocks(indicator_params, pd_params);
    
    /* Build a grid description from other bits */
    
    *gd = GD_From_Param_Blocks(indicator_params, gd_params, real_params,
         vert_params, num_vert_params, quasi_lengths, num_quasi_lengths);
    
    *error_status = Grib_Success;
} else {

    /* Decode failed because message is invalid or corrupted in some way */

    *error_status = Grib_Invalid;
}

} /* end function Grib_Get_All */
