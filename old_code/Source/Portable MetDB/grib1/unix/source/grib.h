/*+ grib.h
 *
 * Defines a type Grib capable of representing a complete message in the
 * format FM 92-X Ext. GRIB, as described in the World Meterological
 * Organisation Manual on Codes.  A GRIB message represents an analysis or
 * forecast of certain physical properties of the atmosphere at a particular
 * time.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     5/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 */

#if ! defined(GRIB_H)

#define GRIB_H

/* Standard header files used */

#include <stdio.h>

/* Package header files used */

#include <portability.h>
#include <product_definition.h>
#include <grid_description.h>

/*- End of file header */

/* ARBITARY LIMITS on the amount of data in a GRIB message */

/* Maximum length of an encoded message (in bytes) */
#define GRIB_MAX_BYTES  5000000

/* Maximum number of data items in a message */
#define GRIB_MAX_ITEMS  1000000

/* Error status type */

enum Grib_Error_Enum { Grib_Success, Grib_EOF, Grib_Too_Long, Grib_Incomplete,
    Grib_Read_Error, Grib_Invalid };

typedef enum Grib_Error_Enum Grib_Error;

/* The Grib type. The way data is stored in a Grib object should be regarded
 * as private. You should only access the data using the public functions below.
 *
 * The Grib message is held in encoded form for compatability with the Fortran
 * routine DECODE. If this was replaced by C routine(s), it would be desireable
 * to switch to representing a Grib message in unencoded form using a series
 * of structures.
 */
typedef unsigned INTEGER *Grib;

/* -------------------------------------------------------------------------- */
/* PUBLIC functions                                                           */
/* -------------------------------------------------------------------------- */

/* Read a Grib object from file. */

void  Grib_Read(

    /* IN */
    FILE *file_ptr,               /* File to read from */
    
    /* OUT */
    Grib *mesg,                   /* Grib object read, if status Grib_Success */
    Grib_Error *error_status      /* Status */
);

/* Return all the information contained in a Grib object. */

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
    Grib_Error  *error_status           /* Error status */
);

/* Dispose of a Grib object */

void  Grib_Free(

    /* INOUT */
    Grib  mesg
);

/* -------------------------------------------------------------------------- */
/* PRIVATE functions, to be used only by the public functions above           */
/* -------------------------------------------------------------------------- */

/* Create a new Grib object with undefined contents */

Grib Grib_New(void);

/* Match a character against a string and return the number of characters
 * matched so far */

int Grib_Chars_Matched(

    /* IN */
    unsigned char  chr,              /* Character to test */
    unsigned char  *string,          /* String to compare it against */
    int            matched_so_far    /* Position in string to compare against */
);

/* Read an edition 0 GRIB message from file */

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
);

/* Read one block of a GRIB message from file */

void  Grib_Read_Block(

    /* IN */
    FILE           *file_ptr,       /* File to read from */
    
    /* INOUT */
    Grib           mesg,            /* Grib object being read */
    unsigned long  *total_stored,   /* Number of bytes stored in mesg so far */
    
    /* OUT */
    Grib_Error     *error_status    /* Status */
);

/* Read bytes from file into a Grib object */

size_t  Grib_Read_Bytes(

    /* IN */
    FILE           *file_ptr,       /* File to read from */
    unsigned long  total_stored,    /* Number of bytes already stored in mesg */
    size_t         bytes_to_read,   /* Number of bytes to read */
    
    /* INOUT */
    Grib           mesg              /* Grib object being read */
);

/* Insert one byte into a Grib object */

void Grib_Insert_Byte(

    unsigned char  byte,           /* Byte to insert */
    Grib           mesg,           /* Message to insert it into */
    unsigned long  byte_num        /* Position to insert it at (bytes from */
                                   /* start) */
);

/* C definition for the Fortran routine DECODE, which decodes a Grib message
 * and returns all the data. Use Grib_Get_All if calling from other C code. */

FORTRAN void decode(
    REAL     fp_data[],      /* OUT */
    REAL     fp_work[],      /* WORKSPACE */
    INTEGER  *len_fp,        /* IN */
    INTEGER  *num_fp,        /* OUT */
    REAL     vert_coords[],  /* OUT */
    INTEGER  *len_vert,      /* IN */
    INTEGER  *num_vert,      /* OUT */
    INTEGER  bitmap[],       /* OUT */
    INTEGER  *len_bitmap,    /* IN */
    INTEGER  *num_bitmap,    /* OUT */
    INTEGER  quasi[],        /* OUT */
    INTEGER  *len_q,         /* IN */
    INTEGER  *num_q,         /* OUT */
    INTEGER  *width,         /* OUT */
    INTEGER  *word_size,     /* IN */
    INTEGER  block_0[4],     /* OUT */
    INTEGER  block_1[],      /* OUT */
    INTEGER  block_2[20],    /* OUT */
    INTEGER  block_3[2],     /* OUT */
    INTEGER  block_4[2],     /* INOUT */
    REAL     block_r[20],    /* OUT */
    unsigned INTEGER mesg[], /* IN */
    INTEGER  *len_mesg,      /* IN */
    INTEGER  posn[4],        /* IN */
    INTEGER  *word,          /* OUT */
    INTEGER  *off,           /* IN */
    INTEGER  *error,         /* OUT */
    INTEGER  work_int1[],    /* WORKSPACE */
    INTEGER  work_int2[],    /* WORKSPACE */
    REAL     work_re1[],     /* WORKSPACE */
    INTEGER  *err_unit,      /* IN */
    INTEGER  *msglvl         /* IN */
);

#endif  /* ! defined(GRIB_H) */
