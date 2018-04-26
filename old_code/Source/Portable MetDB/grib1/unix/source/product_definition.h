/*+ product_definition.h
 *
 * Defines a type PD capable of representing the product definition section
 * of a message in the format FM 92-X Ext. GRIB, as described in the World
 * Meterological Organisation Manual on Codes. The product definition section
 * specifies some details of how the message was generated, the physical
 * variable represented in the message, and the time at which the analysis
 * or forecast of this variable is valid.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     8/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 */

#if ! defined(PRODUCT_DEFINITION_H)

#define PRODUCT_DEFINITION_H

/* Package header files used */

#include <portability.h>

/*- End of file header */

/* ARBITARY LIMIT on the amount of data in a product definition */

/* Maximum number of bytes in a product definition. Must be >=21 for
 * edition 1 GRIB. */
#define PD_MAX_BYTES  1000

/* Maximum length of Fortran-style parameter block, pd_params */
#define PD_MAX_ELEMS  21 + (PD_MAX_BYTES - 28)

/* The product definition type. The way data is stored in a product definition
 * should be regarded as private. You should only access the data using the
 * public functions below.
 *
 * The contents of indicator_params and params correspond to BLOCK_0 and
 * BLOCK_1, as defined in ../docs/appA. This representation is chosen to allow
 * the Fortran routines DECODE, PRTBK1 and PRTBK2 to be used conveniently. If
 * these were replaced by C routines, it would be desireable to replace the
 * parameter block arrays with a structure or structures.
 */
struct PD_Struct {
    INTEGER  indicator_params[4];
    INTEGER  params[PD_MAX_ELEMS];
};

typedef struct PD_Struct  *PD;

/* -------------------------------------------------------------------------- */
/* PUBLIC functions                                                           */
/* -------------------------------------------------------------------------- */

/* Create a product definition from Fortran-style parameter blocks */

PD  PD_From_Param_Blocks(

    /* IN */
    INTEGER  indicator_params[4],
    INTEGER  pd_params[PD_MAX_ELEMS]
);

/* Output a product definition in human-readable form. */

void PD_Print(

    /* IN */
    PD  pd
);

/* Dispose of a PD object */

void  PD_Free(

    /* INOUT */
    PD  pd
);

/* -------------------------------------------------------------------------- */
/* PRIVATE functions, to be used only by the public functions above           */
/* -------------------------------------------------------------------------- */

/* Create a new product definition with undefined contents */

PD  PD_New(void);

/* C definition for the Fortran routine PRTBK1, which outputs a product
 * definition in human-readable form. Use PD_Print if calling from other C
 * code. */

FORTRAN void prtbk1(

    /* IN */
    INTEGER block_0[4],
    INTEGER block_1[]
);

#endif  /* ! defined(PRODUCT_DEFINITION_H) */
