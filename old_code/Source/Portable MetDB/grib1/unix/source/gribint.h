/*+ gribint.h
 *
 * Defines the integer type used in encoded GRIB messages.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     23/1/97   Original code. (A. Edmunds)
 *
 * Defines the integer type used in encoded GRIB messages. This is a
 * variable-length unsigned integer with big-endian byte ordering.
 *
 * Code description: ANSI C
 */

#if ! defined(GRIBINT_H)

#define GRIBINT_H

/*- End of file header */

/* The GribInt type. The way data is stored in a Grib object should be regarded
 * as private. You should only access the data using the public functions below.
 */
typedef unsigned char  GribInt[];

/* -------------------------------------------------------------------------- */
/* PUBLIC functions                                                           */
/* -------------------------------------------------------------------------- */

/* Convert a GribInt to a host machine unsigned integer */

unsigned int  GribInt_To_U_Int(

    /* IN */
    GribInt  gribint,       /* Integer in coded form */
    int      length         /* Length of gribint in bytes */
);

/* Convert a GribInt to a host machine unsigned long integer */

unsigned long  GribInt_To_U_Long(

    /* IN */
    GribInt  gribint,     /* Integer in coded form */
    int     length        /* Length of gribint in bytes */
);

#endif  /* ! defined(GRIBINT_H) */
