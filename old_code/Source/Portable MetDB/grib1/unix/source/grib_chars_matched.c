/*+ grib_chars_matched.c
 *
 * Return no. of chars in string which have been matched so far
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     15/11/96   Original code. (A. Edmunds)
 *
 * Tests chr against string[matched_so_far]. Returns matched_so_far+1 if 
 * they are equal, or 0 otherwise.
 *
 * Code description: ANSI C
 */
static char fileID[] = "$Header: grib_chars_matched.c, 1, 27/02/2007 16:16:21, Stan Kellett$";

int Grib_Chars_Matched(

    /* IN */
    unsigned char  chr,              /* Character to test */
    unsigned char  *string,          /* String to compare it against */
    int            matched_so_far    /* Position in string to compare against */
)

{

if ( chr == string[matched_so_far] ) {
    return matched_so_far+1;
} else {
    return 0;
} /* end if */

} /* end function Grib_Chars_Matched */
