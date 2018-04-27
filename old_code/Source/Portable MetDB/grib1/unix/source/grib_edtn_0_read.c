/*+ grib_edtn_0_read.c
 *
 * Read an edition 0 GRIB message from file.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     21/1/97   Original code. (A. Edmunds)
 *
 * Reads part of block 1 and blocks 2 to 5 of an edition 0 GRIB message from
 * file and stores them in mesg starting at an offset of total_stored bytes
 * from the beginning.
 *
 * Code description: ANSI C
 */
static char fileID[] = "$Header: grib_edtn_0_read.c, 1, 27/02/2007 16:16:20, Stan Kellett$";

/* Standard header files used */

#include <stdio.h>

/* Package header files used */

#include <grib.h>
#include <gribflags.h>
#include <boolean.h>
#include <array.h>

void  Grib_Edtn_0_Read(

    /* IN */
    FILE           *file_ptr,       /* File to read from */
    unsigned long  pd_len,          /* Length of the product definition sectn */
    unsigned char  buffer[],        /* Buffer containing part of P.D. section */
                                    /* already read */
    int            bytes_in_buffer, /* Number of bytes held in buffer */
    unsigned long  total_stored,    /* Number of bytes stored in mesg so far */
    
    /* INOUT */
    Grib           mesg,            /* The message being read in */
    
    /* OUT */
    Grib_Error     *error_status    /* Error code */
)
{

/* Local constants */

/* Length of product definition section in standard edition 0 GRIB.
 * Pre-standardisation experimental GRIB had a shorter product definition
 * section. */
#define STANDARD_EDTN_0_PD_BYTES  24 

/* Maximum length of product definition section, edition 0 or experimental */
#define MAX_EDTN_0_PD_BYTES  STANDARD_EDTN_0_PD_BYTES

/* Byte number at which the block flags are held. Note that all _POSN values
 * are one less than the octet numbers given in the Manual on Codes, because C
 * arrays are indexed from zero. */
#define BLOCK_FLAGS_POSN  7
#define END_BYTES  4          /* Length of end section, in bytes */

/* Local scalars */

unsigned long  pd_bytes_left = pd_len - bytes_in_buffer;
unsigned char  pd_coded[MAX_EDTN_0_PD_BYTES];   /* The product definition in */
                                                /* coded form */
size_t         bytes_read;         /* Number of bytes successfully read */
unsigned long  index;              /* Index into pd_coded */
Boolean        block_flags[7];     /* Flags indicating whether sections of */
                                   /* the message are present (coded form) */
Boolean        gd_present = False;     /* True iff grid description present */
Boolean        bitmap_present = False; /* True iff bitmap section present */

/*- End of header */

*error_status = Grib_Success;
Arr_Copy(pd_coded, buffer, bytes_in_buffer);
bytes_read = fread(&pd_coded[bytes_in_buffer], sizeof(unsigned char),
    pd_bytes_left, file_ptr);

if ( bytes_read < pd_bytes_left ) {
    Grib_Handle_Read_Error(file_ptr, error_status);
}

if ( *error_status == Grib_Success ) {

    for ( index = 0; index < pd_len; index++ ) {
        Grib_Insert_Byte(pd_coded[index], mesg, total_stored);
        total_stored++;
    } /* end for */

    GribFlags_To_Flags(pd_coded[BLOCK_FLAGS_POSN], block_flags);
    if ( pd_len == STANDARD_EDTN_0_PD_BYTES ) {
        gd_present     = block_flags[0];
        bitmap_present = block_flags[1];
    } else {
        gd_present     = block_flags[7];
        bitmap_present = block_flags[6];
    } /* end if */
    
    if ( gd_present ) {
        Grib_Read_Block(file_ptr, mesg, &total_stored, error_status);
    } /* end if */
    
    if ( bitmap_present && (*error_status == Grib_Success) ) {
        Grib_Read_Block(file_ptr, mesg, &total_stored, error_status);
    } /* end if */
    
    /* Read the data */
    
    if ( *error_status == Grib_Success ) {
        Grib_Read_Block(file_ptr, mesg, &total_stored, error_status);
    } /* end if */
    
    /* Read the end section */
    
    if ( *error_status == Grib_Success ) {
        bytes_read = Grib_Read_Bytes(file_ptr, total_stored, END_BYTES, mesg);
        if ( bytes_read < END_BYTES ) {
            Grib_Handle_Read_Error(file_ptr, error_status);
        }
    } /* end if */
} /* end if */

} /* end function Grib_Edtn_0_Read */
