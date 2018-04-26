/*+ boolean.h
 *
 * Defines a Boolean type which has the values False and True
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     6/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 */

#if ! defined(BOOLEAN_H)

#define BOOLEAN_H

/*- End of file header */

enum Boolean_Enum { False = 0, True = 1 };

typedef enum Boolean_Enum  Boolean;

#endif  /* ! defined(BOOLEAN_H) */
