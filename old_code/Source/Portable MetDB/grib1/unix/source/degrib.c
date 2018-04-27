/*+ degrib.c
 *
 * Decode file(s) of GRIB messages and output them in textual form
 *
 * Current Code Owner: Andrew Edmunds, OS(US)
 *
 * Version    Date     Comment
 *   1.0     5/11/96   Original code. (A. Edmunds)
 *
 * Code description: ANSI C
 *
 * Usage: degrib [-h -d -m 0-3] grib-file ...
 */
static char fileID[] = "$Header: degrib.c, 1, 27/02/2007 16:16:24, Stan Kellett$";

/* Standard header files used: */
 
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

/* Package header files used */

#include <boolean.h>
#include <stringterm.h>

int main(int argc, char **argv)
{

/* Local scalars */

char     *prog_name;
char     c;
char     *m_arg;
Boolean  headers_reqd  = False;
Boolean  data_reqd     = False;
int      message_level = 2;
int      error_status  = EXIT_SUCCESS;
FILE     *file_ptr;
int      processing_status;

/*- End of header */

/* Get program name (for error messages) */

prog_name = *argv;
argc--;
argv++;

/* Parse options */

/* Loop while there are more words starting with '-' and no errors found */

while ( (argc > 0) && (**argv == '-' ) && (error_status == EXIT_SUCCESS) ) {
    (*argv)++;
    c = **argv;
    
    /* Loop over each letter, or until an error found */
    
    while ( (c != End_Of_String) && (error_status == EXIT_SUCCESS) ) {
        switch ( c ) {
        case 'h':
        
            /* 'h' option found. Set headers required flag. */
            
            headers_reqd = True;
            break;
        case 'd':
        
            /* 'd' option found. Set data required flag. */
            
            data_reqd = True;
            break;
        case 'm':
        
            /* 'm' option found. Parse the argument to this option. */
            
            m_arg = NULL;
            (*argv)++;
            if ( **argv != End_Of_String ) {
            
                /* If there are more chars in this word, take them as */
                /* the argument, e.g. '-m1' */
                
                m_arg = *argv;
            } else if ( argc > 1 ) {
            
                /* Otherwise take the next word as the argument, e.g. '-m 1' */
                
                argc--;
                argv++;
                m_arg = *argv;
            } else {
            
                /* No argument given */
                
                fprintf(stderr, "Usage: %s [-h -d -m 0-3] grib-file ...\n",
                    prog_name);
                error_status = EXIT_FAILURE;
            }
            if ( m_arg != NULL ) {
            
                /* Check argument is an integer 0..3. Set message_level. */
                
                message_level = atoi(m_arg);
                if ( (! isdigit(m_arg[0])) || (message_level < 0) ||
                     (message_level > 3) ) {
                    fprintf(stderr, "%s: invalid message level %s\n",
                        prog_name, m_arg);
                    error_status = EXIT_FAILURE;
                }
            }
            break;
        default:
        
            /* Unrecognised option */
            
            fprintf(stderr, "Usage: %s [-h -d -m 0-3] grib-file ...\n",
                prog_name);
            error_status = EXIT_FAILURE;
            break;
            
        } /* end switch */
        (*argv)++;
        c = **argv;
    
    } /* end while */
    argc--;
    argv++;
    
} /* end while */

if ( ( ! headers_reqd ) && ( ! data_reqd ) ) {

    /* If neither header information nor data was specified, default to */
    /* outputting both. */

    headers_reqd = True;
    data_reqd    = True;
} /* end if */

if ( error_status == EXIT_SUCCESS ) {

    if ( argc == 0 ) {

        /* No filenames specified. */
    
        fprintf(stderr, "Usage: %s [-h -d -m 0-3] grib-file ...\n",
                prog_name);
        error_status = EXIT_FAILURE;
    }
    
    /* Take remaining arguments as file names. Process each file in turn. */

    while ( argc > 0 ) {
        file_ptr = fopen(*argv, "rb");
        if ( file_ptr != NULL ) {
    
            /* Successfully opened file. Process it. */
        
            processing_status = Process_Grib_File(file_ptr, headers_reqd,
                data_reqd, prog_name, *argv, message_level);
            if ( processing_status != EXIT_SUCCESS ) {
                error_status = EXIT_FAILURE;
            }
        } else {
    
            /* Open failed */
        
            if ( message_level < 3 ) {
                fprintf(stderr, "%s: %s (file %s)\n", prog_name,
                    strerror(errno), *argv);
            } /* end if */
            error_status = EXIT_FAILURE;
        } /* end if */
        argc--;
        argv++;
    } /* end while */
} /* end if */

/* Done. */

exit(error_status);

} /* end function main */


