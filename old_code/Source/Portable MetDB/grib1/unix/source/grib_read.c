/*+ grib_read.c
 *
 * Read one GRIB message from a file
 *
 * Current Code Owner: MetDB Team
 *
 * Version    Date     Comment
 *   1.0     11/11/96   Original code. (A. Edmunds)
 * See Log section below
 * 
 * Reads one GRIB message from a file and returns it in mesg. The file must be
 * open. Any bytes before the start of the message will be discarded. The
 * format of the file should be as follows:
 *
 * GribFile = (Garbage GribMessage)* [Garbage]
 *
 * Garbage = GarbageChars*
 *
 * GarbageChars = NonG | 'G' NonR | 'G' 'R' NonI | 'G' 'R' 'I' NonB
 *
 * where:
 *
 * GribMessage          is a GRIB message as defined in the WMO Manual on Codes
 *
 * 'G'                  is the byte representing the letter 'G' when coded in
 *                      the International Alphabet no. 5 (i.e. ASCII)
 *
 * 'R', 'I', 'B'        are defined similarly
 *
 * NonG                 is any byte except that representing the letter 'G'
 *                      when coded in the International Alphabet no. 5
 *
 * NonR, NonI and NonB  are defined similarly
 *
 * Code description: ANSI C
 *
 *$Log:
 * 1    Met_DB_Project 1.0         27/02/2007 16:16:18    Stan Kellett    
 *$
 *Revision 1.2  1999/04/21 12:00:47  usmdb
 *Added header terminator
 *
 *$Revision: 1$
 *$Date: 27/02/2007 16:16:18$
*/ 
static char fileID[] = "$Header: grib_read.c, 1, 27/02/2007 16:16:18, Stan Kellett$";

/* Standard header files used */

#include <stdio.h>
#include <string.h>

/* Package header files used */

#include <grib.h>
#include <gribint.h>

void  Grib_Read(

    /* IN */
    FILE *file_ptr,               /* File to read from */
    
    /* OUT */
    Grib *mesg,                   /* Grib object read, if status Grib_Success */
    Grib_Error *error_status      /* Status */
)
{

/* Local constants */

/* The GRIB header string - "GRIB" in ASCII. Define it this way just in case
 * the code is ever ported to MVS */
unsigned char  Header[] = "\x47\x52\x49\x42";

/* Length of the header string. */
#define HEADER_BYTES  4

/* Byte offset from start of message at which length field starts. Note that
 * all _POSN values are counted starting from zero and are therefore one less
 * than the octet numbers given in the Manual on Codes. */
#define LEN_POSN    4
#define LEN_BYTES   3      /* Length of the length field in bytes. */
#define EDTN_POSN   7      /* Byte offset from start of message of edition no */
#define EDTN_BYTES  1      /* Length of edition no. */
#define EDTN_1_INDICATOR_BYTES  8  /* Length of indicator section, edition 1 */
#define EDTN_0_INDICATOR_BYTES  4  /* Length of indicator section, edition 0 */

#define MAX_INDICATOR_BYTES  Max(EDTN_1_INDICATOR_BYTES, EDTN_0_INDICATOR_BYTES)

/* Local scalars */

int            matched_so_far = 0;  /* Number of bytes matching GRIB */
                                    /* header found so far */
int            value_read = 0;      /* Current byte of message, or EOF if */
                                    /* the read failed */
unsigned char  byte;                /* Current byte of message */
size_t         bytes_read;          /* Number of bytes successfully read */
                                    /* using fread() */
unsigned char  buffer[MAX_INDICATOR_BYTES];  /* Header string, product */
                                    /* defintion length or message length, */
                                    /* and edition number, in encoded form */
int            index;               /* Index into buffer */
unsigned int   edtn_no;             /* GRIB edition number */
unsigned long  pd_len;              /* Length of product definition section, */
                                    /* if edition number is 0 */
unsigned long  mesg_len;            /* Length of complete message, if */
                                    /* edition number is 1 */
/*-    End of header */

*error_status = Grib_Success;

/* Search forward from the current position in the file until a GRIB header
 * string is found. */

while ( (value_read != EOF) && (matched_so_far < HEADER_BYTES) ) {
    value_read = fgetc(file_ptr);
    if ( value_read != EOF ) {
        byte = (unsigned char)value_read;
        matched_so_far = Grib_Chars_Matched(byte, Header, matched_so_far);
    } /* end if */
} /* end while */

if ( matched_so_far < HEADER_BYTES ) {
    if ( feof(file_ptr) ) {

        /* The end of the file was reached without finding a GRIB message at
         * all */
    
        *error_status = Grib_EOF;
    } else {
        *error_status = Grib_Read_Error;
    } /* end if */
} /* end if */

if ( *error_status == Grib_Success ) {

    /* A header string was found. Copy it to the buffer. */
    
    strncpy((char *)buffer, (char *)Header, HEADER_BYTES);
    
    /* Read the length and edition number fields. In edition 1 GRIB, these
     * are part of the indicator section and the length field gives the
     * total length of the message. In edition 0 GRIB, these are the first
     * part of the product definition section and the length field gives the
     * length of the product definition section. */
     
    bytes_read = fread(&buffer[LEN_POSN], sizeof(unsigned char),
        LEN_BYTES + EDTN_BYTES, file_ptr);
    
    if ( bytes_read < LEN_BYTES + EDTN_BYTES ) {
        Grib_Handle_Read_Error(file_ptr, error_status);
    } /* end if */
} /* end if */
    
if ( *error_status == Grib_Success ) {
        
    /* Decode the edition number and length field. */
    
    edtn_no = GribInt_To_U_Int(&buffer[EDTN_POSN], EDTN_BYTES);
    if ( edtn_no == 0 ) {
        pd_len = GribInt_To_U_Long(&buffer[LEN_POSN], LEN_BYTES);
    } else {
        mesg_len = GribInt_To_U_Long(&buffer[LEN_POSN], LEN_BYTES);
    } /* end if */
    
    *mesg = Grib_New();
    
    /* Copy indicator section to mesg, then read in the rest of the
       message. */
    
    switch ( edtn_no ) {
    case 0:
        for ( index = 0; index < EDTN_0_INDICATOR_BYTES; index++ ) {
            Grib_Insert_Byte(buffer[index], *mesg, index);
        } /* end for */
        Grib_Edtn_0_Read(file_ptr, pd_len, &buffer[LEN_POSN],
            LEN_BYTES + EDTN_BYTES, EDTN_0_INDICATOR_BYTES, *mesg,
            error_status);
        break;
    case 1:
        for ( index = 0; index < EDTN_1_INDICATOR_BYTES; index++ ) {
            Grib_Insert_Byte(buffer[index], *mesg, index);
        } /* end for */
        if ( mesg_len <= GRIB_MAX_BYTES ) {
            bytes_read = Grib_Read_Bytes(file_ptr, EDTN_1_INDICATOR_BYTES,
                mesg_len - EDTN_1_INDICATOR_BYTES, *mesg);
            if ( bytes_read < mesg_len - EDTN_1_INDICATOR_BYTES ) {
                Grib_Handle_Read_Error(file_ptr, error_status);
            } /* end if */
        } else {
            *error_status = Grib_Too_Long;
        } /* end if */
        break;
    default:
        /* error */
        break;
    } /* end switch */
} /* end if */

} /* end function Grib_Read */
