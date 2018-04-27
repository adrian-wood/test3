/*+ portability.h
 *
 * Defines preprocessor macros to improve portability between machines.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     17/12/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 */

#if ! defined(PORTABILITY_H)

#define PORTABILITY_H

/*- End of file header */

/* C declarations for Fortran routines have the following form when using the
 * Cray C compiler.
 *
 * fortran void decode(....);
 *
 * The word 'fortran' should not appear when using the HP ANSI C compiler or
 * the GNU C compiler.
 */

#if defined(HP_ANSI) || defined(__GNUC__)

#define FORTRAN

#elif defined(_CRAY)

#define FORTRAN  fortran

#elif defined(MAKEDEPEND)

/* Do nothing */

#else

#error Unknown compiler. Please edit portability.h.

#endif

/* INTEGER is the C type which is equivelent to a Fortran INTEGER
 *
 * REAL is the C type which is equivelent to a Fortran REAL. This is float on
 * HP's and the C90, but double on the T3E.
 *
 * BIG_ENDIAN implies that the machine uses big-endian ordering of bytes within
 * a word. For machines which use little-endian ordering (P.C.s, VAX's) you
 * should define LITTLE_ENDIAN instead.
 */

#if defined(__hppa) || defined(_CRAY1)

#define INTEGER    int
#define REAL       float
#define BIG_ENDIAN 1

#elif defined(_CRAYMPP)

#define INTEGER    int
#define REAL       double
#define BIG_ENDIAN 1

#elif defined(LINUX_IFC_R4)

#define INTEGER    int
#define REAL       float
/* #define LITTLE_ENDIAN 1 */
# include <endian.h>

#elif defined(MAKEDEPEND)

/* Do nothing */

#else

#error Unknown machine. Please edit portability.h

#endif

#endif  /* ! defined(PORTABILITY_H) */
