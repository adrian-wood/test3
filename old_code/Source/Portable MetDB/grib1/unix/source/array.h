/*+ array.h
 *
 * Defines a macro to copy an array.
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     19/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 */

#if ! defined(ARRAY_H)

#define ARRAY_H

/*- End of file header */

/* Copy NUM_ELEMS elements from array SRC to array DEST. The arrays may be
 * of any type, provided elements of SRC are assignable to elements of DEST
 */

#define Arr_Copy(DEST, SRC, NUM_ELEMS) \
{ \
int i; \
for ( i = 0; i < (NUM_ELEMS); i++ ) \
    (DEST)[i] = (SRC)[i]; \
}

#endif  /* ! defined(ARRAY_H) */
