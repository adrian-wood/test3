/*+ grib_read_bytes.c
 *
 * Read bytes from a file into a Grib object.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     21/1/97   Original code. (A. Edmunds)
 *
 * Reads up to bytes_to_read bytes from a file and stores them in mesg,
 * starting at an offset of total_stored bytes from the beginning. Returns
 * the number of bytes actually read.
 *
 * If Grib messages were held as unsigned char arrays, this routine could be
 * discarded and calls to it replaced with calls to fread().
 *
 * Code description: ANSI C
 */
static char fileID[] = "$Header: grib_read_bytes.c, 1, 27/02/2007 16:16:17, Stan Kellett$";

/* Standard header files used */

#include <stdio.h>

/* Package header files used */

#include <grib.h>

size_t  Grib_Read_Bytes(

    /* IN */
    FILE           *file_ptr,       /* File to read from */
    unsigned long  total_stored,    /* Number of bytes already stored in mesg */
    size_t         bytes_to_read,   /* Number of bytes to read */
    
    /* INOUT */
    Grib           mesg              /* Grib object being read */
)
{

/* Local scalars */

int            value_read = 0;      /* Current byte of message, or EOF if */
                                    /* the read failed */
unsigned char  byte;                /* Current byte of message */
size_t         read_so_far = 0;     /* Number of bytes read so far */

/*-    End of header */

/* Read bytes_to_read bytes from the file into mesg, if possible */

while ( (value_read != EOF) && (read_so_far < bytes_to_read) ) {
    value_read = fgetc(file_ptr);
    if ( value_read != EOF ) {
        byte = (unsigned char) value_read;
        Grib_Insert_Byte(byte, mesg, total_stored);
        read_so_far++;
        total_stored++;
    } /* end if */
} /* end while */

return read_so_far;

} /* end function Grib_Read_Bytes */
