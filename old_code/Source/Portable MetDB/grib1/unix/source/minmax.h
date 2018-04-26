/*+ minmax.h
 *
 * Defines macros, Min and Max, which return the lesser and greater of two
 * values.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     19/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 */

#if ! defined(MINMAX_H)

#define MINMAX_H

/*- End of file header */

/* Return the lesser of A and B. A and B may be of any comparable types. */

#define Min(A, B) ((A) < (B) ? (A) : (B))

/* Return the greater of A and B. A and B may be of any comparable types. */

#define Max(A, B) ((A) > (B) ? (A) : (B))

#endif  /* ! defined(MINMAX_H) */
