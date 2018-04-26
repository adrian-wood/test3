/*+ gribflags.h
 *
 * Defines the 'flags' type used in encoded GRIB messages.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     23/1/97   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 */

#if ! defined(GRIBFLAGS_H)

#define GRIBFLAGS_H

/* Package header files used */

#include <boolean.h>

/*- End of file header */

/* The GribFlags type. The way data is stored in a Grib object should be
 * regarded as private. You should only access the data using the public
 * functions below. */

typedef unsigned char  GribFlags;

/* -------------------------------------------------------------------------- */
/* PUBLIC functions                                                           */
/* -------------------------------------------------------------------------- */

/* Convert a GribFlags object to an array of eight Boolean values. */

void  GribFlags_To_Flags(

    /* IN */
    GribFlags  gribflags,       /* Set of eight flags in coded form */
    
    /* OUT */
    Boolean    flags[7]         /* The flags as Boolean values */
);

#endif  /* ! defined(GRIBINT_H) */
