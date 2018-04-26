/*+ grib_insert_byte.c
 *
 * Insert a byte into a Grib message
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     15/11/96   Original code. (A. Edmunds)
 *
 * Inserts one byte into a Grib message. The message must have been initialised
 * with Grib_New.  The Grib type is defined as an unsigned int array and 
 * sizeof(int) bytes are packed into each element. This format is purely for
 * compatability with the Fortran DECODE routine. If this routine was replaced,
 * switching to an unsigned char array representation would be preferable.
 *
 * The order of bytes within a word is significant in this routine. Big-endian
 * and little-endian orderings are supported. The choice of which is used is
 * made in portability.h.
 *
 * Code description: ANSI C
 */
static char fileID[] = "$Header: grib_insert_byte.c, 2, 27/02/2007 17:02:17, Stan Kellett$";

/* Standard header files used */

#include <math.h>
#include <stdlib.h>

/* Package header files used */
 
#include <grib.h>
#include <portability.h>
#include <boolean.h>

void Grib_Insert_Byte(

    unsigned char  byte,           /* Byte to insert */
    Grib           mesg,           /* Message to insert it into */
    unsigned long  byte_num        /* Position to insert it at (bytes from */
                                   /* start) */
)

{
static Boolean           initialised  = False;
static unsigned INTEGER  masks[sizeof(unsigned INTEGER)];
int                      bit_num;
int                      index;
int                      byte_of_word;
unsigned INTEGER         shifted_byte;

if ( ! initialised ) {

    /* Calculate the required bit masks once and cache them for efficiency */
    
    for ( byte_of_word = 0; byte_of_word < sizeof(unsigned INTEGER);
          byte_of_word++ ) {

/*
#if   defined(LITTLE_ENDIAN)
        bit_num = 8 * byte_of_word;
#elif defined(BIG_ENDIAN)
*/
        bit_num = 8 * (sizeof(unsigned INTEGER) - byte_of_word - 1);

/*
#elif defined(MAKEDEPEND)
*/
/* Do nothing */
/*
#else
#error Unknown byte ordering scheme. Please edit grib_insert_byte.c
#endif
*/
        masks[byte_of_word]   = ~(0xFF << bit_num);
    } /* end for */
    initialised = True;
} /* end if */

index        = byte_num / sizeof(unsigned INTEGER);
byte_of_word = byte_num % sizeof(unsigned INTEGER);

/* 
  #if   defined(LITTLE_ENDIAN)
  bit_num      = 8 * byte_of_word;
  #elif defined(BIG_ENDIAN)
  */
bit_num      = 8 * (sizeof(unsigned INTEGER) - byte_of_word - 1);
/*
  #elif defined(MAKEDEPEND)
*/
  /* Do nothing */
/*
  #else
  #error Unknown byte ordering scheme. Please edit grib_insert_byte.c
  #endif
*/

shifted_byte = byte << bit_num;
mesg[index]  = mesg[index] & masks[byte_of_word] | shifted_byte;

} /* end function Grib_Insert_Byte */
