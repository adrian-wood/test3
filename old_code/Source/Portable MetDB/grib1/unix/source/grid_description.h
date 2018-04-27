/*+ grid_description.h
 *
 * Defines a type GD capable of representing the grid description section
 * of a message in the format FM 92-X Ext. GRIB, as described in the World
 * Meterological Organisation Manual on Codes. The grid description section
 * specifies the position in the atmosphere to which each data item in the
 * message corresponds.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     8/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 */

#if ! defined(GRID_DESCRIPTION_H)

#define GRID_DESCRIPTION_H

/* Package header files used */

#include <minmax.h>
#include <portability.h>

/*- End of file header */

/* ARBITARY LIMITS on the grids which can be represented */

/* Maximum number of parameters describing vertical coordinate */
#define GD_MAX_VERT_PARAMS  100

/* Maximum number of rows in grid */
#define GD_MAX_ROWS         300

/* Maximum number of columns in grid */
#define GD_MAX_COLUMNS      300

/* Maximum number of row or column lengths which can be returned for a
 * quasi-regular grid
 */
#define GD_MAX_QUASI Max(GD_MAX_ROWS, GD_MAX_COLUMNS)

/* The grid description type. The way data is stored in a grid description
 * should be regarded as private. You should only access the data using the
 * public functions below.
 *
 * The contents of indicator_params, params and real_params correspond to 
 * BLOCK_0, BLOCK_2 and BLOCK_R, as defined in ../docs/appA. This
 * representation is chosen to allow the Fortran routines DECODE, PRTBK1
 * and PRTBK2 to be used conveniently. If these were replaced by C routines,
 * it would be desireable to replace the parameter block arrays with a
 * structure or structures.
 */
struct GD_Struct {
    INTEGER  indicator_params[4];
    INTEGER  params[20];
    REAL     real_params[20];
    REAL     *vert_params;
    INTEGER  num_vert_params;
    INTEGER  *quasi_lengths;
    INTEGER  num_quasi_lengths;
};

typedef struct GD_Struct  *GD;

/* ------------------------------------------------------------------------- */
/* PUBLIC functions                                                          */
/* ------------------------------------------------------------------------- */

/* Create a new grid description from Fortran-style parameter blocks */

GD  GD_From_Param_Blocks(
    
    /* IN */
    
    INTEGER indicator_params[4],             /* Params from indicator section */
    INTEGER gd_params[20],                   /* Params from grid description */
    REAL    real_params[20],                 /* Miscellaneous real params */
    REAL    vert_params[GD_MAX_VERT_PARAMS], /* Params describing vertical */
                                             /* coordinate */
    INTEGER num_vert_params,                 /* No. of vertical coord params */
    INTEGER quasi_lengths[GD_MAX_QUASI],     /* Row lengths, if grid type is */
                                             /* quasi-regular */
    INTEGER num_quasi_lengths                /* Number of row lengths given */
);

/* Output a grid description in human-readable form. */

void GD_Print(

    /* IN */
    GD gd
);

/* Dispose of a grid description */

void  GD_Free(

   /* INOUT */
   GD gd
);

/* -------------------------------------------------------------------------- */
/* PRIVATE functions, to be used only by the public functions above           */
/* -------------------------------------------------------------------------- */

/* Create a new grid description with undefined contents */

GD  GD_New(void);

/* C definition for Fortran routine PRTBK2, which outputs some parts of a
 * grid description in human-readable form. Use GD_Print if calling from
 * other C code. */

FORTRAN void prtbk2(

    /* IN */
    INTEGER block_0[4],
    INTEGER block_2[20],
    REAL block_r[20]
);

#endif  /* ! defined(GRID_DESCRIPTION_H) */
