/*+ grib_read_block.c
 *
 * Read one block of a GRIB message from file.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     21/1/97   Original code. (A. Edmunds)
 *
 * Reads one block of a GRIB message from file and stores it in mesg,
 * starting at an offset of total_stored from the beginning. On exit,
 * total_stored has been incremented by the number of bytes stored.
 *
 * Code description: ANSI C
 */
static char fileID[] = "$Header: grib_read_block.c, 1, 27/02/2007 16:16:18, Stan Kellett$";

/* Standard header files used */

#include <stdio.h>

/* Package header files used */

#include <grib.h>
#include <gribint.h>

void  Grib_Read_Block(

    /* IN */
    FILE           *file_ptr,       /* File to read from */
    
    /* INOUT */
    Grib           mesg,            /* Grib object being read */
    unsigned long  *total_stored,   /* Number of bytes stored in mesg so far */
    
    /* OUT */
    Grib_Error     *error_status    /* Status */
)
{

/* Local constants */

/* Byte number within block at which length field starts. Note that all _POSN
 * values are one less than the octet numbers given in the Manual on Codes,
 * because C arrays are indexed from zero. */
#define LEN_POSN    0
#define LEN_BYTES   3             /* Length of the length field in bytes. */

/* Local scalars */

unsigned char  len_coded[LEN_BYTES];
size_t         bytes_read;
int            index;
unsigned long  block_len;

/*-    End of header */

*error_status = Grib_Success;
bytes_read = fread(len_coded, sizeof(unsigned char), LEN_BYTES, file_ptr);

if ( bytes_read < LEN_BYTES ) {
    Grib_Handle_Read_Error(file_ptr, error_status);
} /* end if */

if ( *error_status == Grib_Success ) {

    /* Read successful. Store the length field. */
    
    for ( index = 0; index < LEN_BYTES; index++ ) {
        Grib_Insert_Byte(len_coded[index], mesg, *total_stored);
        (*total_stored)++;
    } /* end for */
    
    /* Now decode it. */
    
    block_len = GribInt_To_U_Long(len_coded, LEN_BYTES);
    
    /* Read in the rest of the block. */
    
    if ( *total_stored + block_len <= GRIB_MAX_BYTES ) {
        bytes_read = Grib_Read_Bytes(file_ptr, *total_stored,
            block_len - LEN_BYTES, mesg);
        *total_stored = *total_stored + bytes_read;
    
        if ( bytes_read < block_len - LEN_BYTES ) {
            Grib_Handle_Read_Error(file_ptr, error_status);
        } /* end if */
    } else {
        *error_status = Grib_Too_Long;
    } /* end if */
} /* end if */

} /* end function Grib_Read_Block */
