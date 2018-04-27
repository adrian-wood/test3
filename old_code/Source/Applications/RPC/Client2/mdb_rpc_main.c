/*----------------------------------------------------------------------------*
 * Program        : mdb  (mdb_rpc_main.c)                                                
 *
 * Language       : C
 *
 * Description    : This is the MDB RPC top client routine. It receives MDB 
 *                : arguments from the user's Fortran calling program and 
 *                : reads an RPC program number set as the environment 
 *                : variable METDB_SERVER_NUMBER. It will then pass the MDB 
 *                : arguments and the RPC program number to the multi-tasking		 
 *                : server. 
 *
 * Called by      : FORTRAN77 or Fortran 90 MetDB calling program
 *                : 
 * Calls          :  
 *                : msrpc_call_mdb  : multi-tasking server RPC client routine. 
 *                : timtxt          : Print time & diagnostic
 *
 * Revision info  :
 *
 * $Revision: 2$
 * $Date: 21/09/2011 10:35:33$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/mdb_rpc_main.c,v $
 *
 * Change history :
 *
 * $Log:
 *  2    Met_DB_Project 1.1         21/09/2011 10:35:33    Sheila Needham
 *       Changed version to 3.1 for RHEL6 release (no other code changes in
 *       this module)
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
 * Revision 2.0  2003/03/14 12:33:20  usmdb
 * Combined HP/T3E/SX6 version with pre-processor statements - S.Cox
 *
 * Revision 1.10  2001/04/27 10:11:51  usmdb
 * Default routine name is now mdb_ rather than mdb. This is
 * necessary if called by NAG f90/f95 compiled fortran - S.Cox
 *
 * Revision 1.9  2001/03/29 09:38:45  usmdb
 * Updated MetDB Release info - S.Cox
 *
 * Revision 1.8  2000/09/20 15:48:31  usmdb
 * Output metdb_timeout to stderr - S.Cox
 *
 * Revision 1.7  2000/09/05  10:58:56  10:58:56  usmdb (Generic MDB account)
 * Addition of environment variables METDB_RPC_TIMEOUT and
 * METDB_SERVER_IPNAME - S.Cox
 * 
 * Revision 1.6  2000/06/16  11:36:03  11:36:03  usmdb (Generic MDB account)
 * Addition of call to timtxt to output the time at each
 * fprintf - S.Cox
 * 
 * Revision 1.5  99/07/28  12:13:04  12:13:04  usmdb (Generic MDB account)
 * Added debug output - switched on if environment variable 
 * METDB_DEBUG_LEVEL>0 - S.Cox 
 * 
 * Revision 1.4  1998/07/17 08:10:10  usmdb
 * Increase number of single servers available - S.Cox
 *
 * Revision 1.3  1998/07/13 14:51:04  usmdb
 * Output version number - S.Cox
 *
 * Revision 1.2  1998/02/11 09:49:15  usmdb
 * Return immediately if METDB_SERVER_NUMBER not found - S.Cox
 *
 * Revision 1.1  1998/02/02 16:00:56  usmdb
 * Initial revision
 *
 * 04-12-1997   : Written - S.Cox
 *----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*
 * Standard include files.
 * define global constants
 *----------------------------------------------------------------------------*/

#include <stdio.h>                               /* for standard C i/o */  
#include <stdlib.h>                              /* for malloc */
#include <string.h>                              /* for string processing */
#include "constants.h"

#if defined (T3E)
#include <fortran.h>                       /* for C/Fortran conversion on T3E */
#endif
 
#define ENV_VARNAME_BROKER "METDB_SERVER_NUMBER"
#define ENV_VARNAME_FREEPN "METDB_FREEPN_NUMBER"
#define ENV_VARNAME_DEBUG "METDB_DEBUG_LEVEL"
#define ENV_VARNAME_TIMEOUT "METDB_RPC_TIMEOUT"                        /* 1.7 */
#define ENV_VARNAME_IPNAME "METDB_SERVER_IPNAME"                       /* 1.7 */
#define ENV_VARNAME_CONTACT "METDB_CLIENT_CONTACT"                     /* ST3 */

/*----------------------------------------------------------------------------*
 * Subroutine name: Variables are passed from Fortran to C by reference.
 *----------------------------------------------------------------------------*/

#if defined (T3E)
void MDB(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,
         CSTR,CREP)
#else
#if defined (UNDERSCORE)
void mdb_(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,
          CSTR,CREP,LCSUBT,LCREQ,LCSTR,LCREP)
#else
void mdb(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,
         CSTR,CREP,LCSUBT,LCREQ,LCSTR,LCREP)
#endif /* UNDERSCORE */
#endif /* T3E */

/*----------------------------------------------------------------------------*
 * Declare subroutine variables (arguments to mdb subroutine). Fortran
 * parameters are passed by reference, so the arguments recieved by C must
 * be pointers (prefixed with *).
 *
 * The passing of characters from fortran to C differs depending on system:
 * 
 * T3E : There are a set of functions on the Cray to cope with the
 * conversions. These are in the <fortran.h> standard include file. The
 * character strings are declared as type _fcd e.g. _fcd CSUBT (not 
 * char *CSUBT as on other systems). There is also no need to pass the
 * lengths of the strings at the end of the argument list.
 *
 * HP/SX6 : When Fortran passes character strings to C, it passes them by
 * descriptor. The descriptor includes 2 items, a pointer to the first
 * character in the string and an integer value for the declared length of
 * the string. The lengths of the strings are received by the HP9000 CHARS
 * method, i.e. at the end of the argument list. Also, they are not pointers. 
 *----------------------------------------------------------------------------*/

#if defined (T3E)
_fcd   CSUBT;              /* ip      MDB subtype                             */
_fcd   CREQ;               /* ip      MDB request string                      */
_fcd   CSTR;               /* op      MDB report returned as character string */
_fcd   CREP;               /* op      MDB character string observations       */
double *ARRAY;             /* op      MDB array of values                     */
int    *NOBS;              /* ip/op   Number of observations per MDB call     */
int    *NELEM;             /* ip      Number of elements per MDB call         */
int    *ISTAT;             /* ip/op   MDB status flag                         */
#else
char     *CSUBT;           /* ip      MDB subtype                             */
char     *CREQ;            /* ip      MDB request string                      */
char     *CSTR;            /* op      MDB report returned as character string */
char     *CREP;            /* op      MDB character string observations       */
int      LCREQ;            /* hidden  CREQ string length                      */
int      LCSUBT;           /* hidden  CSUBT string length                     */
int      LCSTR;            /* hidden  CSTR string length                      */
int      LCREP;            /* hidden  CREP string length                      */
#if defined (L64)
double   *ARRAY;           /* op      MDB array of values                     */
long int *NOBS;            /* ip/op   Number of observations per MDB call     */
long int *NELEM;           /* ip      Number of elements per MDB call         */
long int *ISTAT;           /* ip/op   MDB status flag                         */
#else
float *ARRAY;              /* op      MDB array of values                     */
int   *NOBS;               /* ip/op   Number of observations per MDB call     */
int   *NELEM;              /* ip      Number of elements per MDB call         */
int   *ISTAT;              /* ip/op   MDB status flag                         */
#endif /* L64 */
#endif /* T3E */

{    

/*----------------------------------------------------------------------------*
 * Declare local variables 
 *
 * *char_debug   : character debug number (set as environment variable
 *               : METDB_DEBUG_LEVEL) from getenv call.
 *
 * *char_prognum : character RPC program number (set as environment variable
 *               : METDB_SERVER_NUMBER) from getenv call.
 *
 * *char_timeout : character RPC timeout (set as environment variable
 *               : METDB_RPC_TIMEOUT) from getenv call.
 *
 * *char_ipname  : character server IP name (set as environment variable
 *               : METDB_SERVER_IPNAME) from getenv call.
 * 
 * *char_contact : character client contact details(set as environment variable
 *               : METDB_CLIENT_CONTACT) from getenv call.
 *
 * sscanf_rc     : integer return code from sscanf function. 
 *
 * user_prognum  : integer broker program number (converted from *char_prognum) 
 *
 * user_free_prognum  : integer freepn program number (converted from 
 *                    : *char_prognum) 
 *
 * first         : first is type int, and is static. When mdb_rpc_main is first
 *               : called, first=1 to indicate that it is the first call to
 *               : the subroutine. On subsequent calls, first=0.
 *
 * client_contact : string client contact details as supplied via envirnoment
 *                : variable METDB_CLIENT_CONTACT.
 * hostname : string name of client machine e.g. els018
 *
 * username : string userid e.g. ussn
 *
 *----------------------------------------------------------------------------*/

  char      *char_debug;
  char      *char_prognum;
  char      *char_timeout;                                             /* 1.7 */
  char      *char_ipname;                                              /* ST3 */
  char      *char_contact;                                             /* ST3 */
  int       sscanf_rc;

  static    char      client_contact[200];                             /* ST3 */ 
  static    char      hostname[100];                                   /* ST3 */ 
  static    char      username[20];                                    /* ST3 */
  static    int user_prognum;
  static    int user_freepn_prognum;
  static    int first   = 1;
/*----------------------------------------------------------------------------*
 * Set up pipe for system commands                                        ST3 
 *----------------------------------------------------------------------------*/
  FILE      *in;                                                       /* ST3 */
  extern    FILE *popen();                                             /* ST3 */

/*----------------------------------------------------------------------------*
 * If this is the 1st call to this routine, get environment variables.
 *----------------------------------------------------------------------------*/

  if (first != 0) {
    first = 0;
    
/*----------------------------------------------------------------------------*
 * 1) get the MetDB RPC server IP name from the environment variable        1.7
 *    METDB_SERVER_IPNAME. If this is not set, default to 
 *    DEFAULT_SERVER_IPNAME
 *----------------------------------------------------------------------------*/

    char_ipname = getenv(ENV_VARNAME_IPNAME);
    if (char_ipname != NULL) {
      sscanf_rc = sscanf(char_ipname, "%s", &server_ipname);
      if (sscanf_rc < 1) {
        strcpy(server_ipname,DEFAULT_SERVER_IPNAME);
      }
    }
    else {
      strcpy(server_ipname,DEFAULT_SERVER_IPNAME);
    }

/*----------------------------------------------------------------------------*
 * 2) get the debug level from the environment variable
 *    METDB_DEBUG_LEVEL. If this is not set, default to 0.
 *----------------------------------------------------------------------------*/

    char_debug = getenv(ENV_VARNAME_DEBUG);
    if (char_debug != NULL) {
      sscanf_rc = sscanf(char_debug, "%d", &debug_level);
      if (sscanf_rc < 1) {
        debug_level = DEFAULT_DEBUG_LEVEL;                             /* 1.7 */
      }
    }
    else {
      debug_level = DEFAULT_DEBUG_LEVEL;                               /* 1.7 */
    }

/*----------------------------------------------------------------------------*
 * 3) get the MetDB RPC timeout from the environment variable               1.7
 *    METDB_RPC_TIMEOUT. If this is not set, or it is < 25 or 
 *    > DEFAULT_MAIN_TIMEOUT, default to DEFAULT_MAIN_TIMEOUT.
 *----------------------------------------------------------------------------*/

    char_timeout = getenv(ENV_VARNAME_TIMEOUT);
    if (char_timeout != NULL) {
      sscanf_rc = sscanf(char_timeout, "%d", &metdb_timeout);
      if (sscanf_rc < 1) {
        metdb_timeout = DEFAULT_MAIN_TIMEOUT;
      }
      if (metdb_timeout < 25 || metdb_timeout > DEFAULT_MAIN_TIMEOUT) {
        metdb_timeout = DEFAULT_MAIN_TIMEOUT;
      }
    }
    else {
      metdb_timeout = DEFAULT_MAIN_TIMEOUT;
    }
    (void)fprintf(stderr,"mdb: for info, metdb_timeout = %d\n",        /* 1.8 */
                  metdb_timeout);                                      /* 1.8 */
      
/*----------------------------------------------------------------------------*
 * 4) get the RPC program number from the environment variable 
 *    METDB_SERVER_NUMBER. If this is not set, output an error.
 *----------------------------------------------------------------------------*/

    char_prognum = getenv(ENV_VARNAME_BROKER);
    sscanf_rc = sscanf(char_prognum, "%d", &user_prognum);

    if (sscanf_rc < 1 & *ISTAT < 99) {
      ErrorNoUserServerNumber();
      *ISTAT = 99;
      return;
    }

/*----------------------------------------------------------------------------*
 * 5) Get the RPC freepn server program number from the user-set environment
 * variable METDB_FREEPN_NUMBER. If this is not set output an error. 
 *----------------------------------------------------------------------------*/

      char_prognum = getenv(ENV_VARNAME_FREEPN);
      sscanf_rc = sscanf(char_prognum, "%d", &user_freepn_prognum);
      

      if (sscanf_rc < 1 & *ISTAT < 99) {
        ErrorNoUserFreepnNumber();
        *ISTAT = 99;
        return;
      }
    

/*----------------------------------------------------------------------------*
 * 6) Get client contact details from the user-set environment
 * variable METDB_CLIENT_CONTACT. If this is not set display a warning. 
 *----------------------------------------------------------------------------*/

      char_contact = getenv(ENV_VARNAME_CONTACT);                    /* ST3 */
      if (char_contact != NULL) {                                    /* ST3 */
        sscanf_rc = sscanf(char_contact, "%s", &client_contact);     /* ST3 */
      
        if (sscanf_rc < 1) {                                         /* ST3 */
          ErrorNoClientContact();                                    /* ST3 */ 
        
        }                                                            /* ST3 */
               
      }                                                              /* ST3 */ 
      else {                                                         /* ST3 */
        strcpy(client_contact,DEFAULT_CONTACT);                      /* ST3 */
        ErrorNoClientContact();                                      /* ST3 */
      }                                                              /* ST3 */ 

/*----------------------------------------------------------------------------*
 * 7) Get client hostname from the system
 *----------------------------------------------------------------------------*/
  
    if (!(in = popen("hostname","r"))) {                             /* ST3 */ 
      strcpy(hostname,"nohost ");                                    /* ST3 */
    }                                                                /* ST3 */
    else {                                                           /* ST3 */ 
      sscanf_rc = fscanf(in, "%s", &hostname);                       /* ST3 */
    }                                                                /* ST3 */
    
/*----------------------------------------------------------------------------*
 * 8) Get user id from the system
 *----------------------------------------------------------------------------*/
  
    if (!(in = popen("whoami","r"))) {                               /* ST3 */ 
      strcpy(username,"nouser ");                                    /* ST3 */
    }                                                                /* ST3 */
    else {                                                           /* ST3 */
      sscanf_rc = fscanf(in, "%s", &username);                       /* ST3 */
    }                                                                /* ST3 */

    

    timtxt();
    (void)fprintf(stdout,"MetDB RPC Release 3.1\n");
    timtxt();
    (void)fprintf(stdout,"server_prognum  = %d\n",user_prognum);
    timtxt();
    (void)fprintf(stdout,"server_ipname = %s\n",server_ipname);    /* 1.7 */
    timtxt();
    (void)fprintf(stdout,"freepn_prognum = %d\n",user_freepn_prognum);
    timtxt();
    (void)fprintf(stdout,"debug_level   = %d\n",debug_level);
    timtxt();
    (void)fprintf(stdout,"metdb_timeout = %d\n",metdb_timeout);    /* 1.7 */
    timtxt();
    (void)fprintf(stdout,"contact details = %s\n",client_contact);    /* ST3 */
    timtxt();
    (void)fprintf(stdout,"hostname = %s\n",hostname);             /* ST3 */
    timtxt();
    (void)fprintf(stdout,"user id = %s\n",username);         /* ST3 */
    timtxt();
    (void)fprintf(stdout,"Server output can be viewed at http://www01/metdb/mdb/librarian/rpc_viewer.html \n");  /* ST3 */
 
  }   /* end of "first" section   */
  

  timtxt();
  fprintf(stdout,"mdb_rpc_main: Will call multi-server rpc\n");

/*----------------------------------------------------------------------------*
 * Attempt multi-user RPC if call_ss == 2
 *----------------------------------------------------------------------------*/

 
  if (debug_level > 0) {
    timtxt();
    (void)fprintf(stdout,"mdb_rpc_main: about to call msrpc_call_mdb\n");
  } 
#if defined (T3E)
  msrpc_call_mdb(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,
                 CSTR,CREP,user_prognum,user_freepn_prognum,
		 client_contact,hostname,username);                     /* ST3 */
#else
  msrpc_call_mdb(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,
                 CSTR,CREP,LCSUBT,LCREQ,LCSTR,LCREP,user_prognum,
                   user_freepn_prognum,
		   client_contact,hostname,username);                   /* ST3 */
#endif
  if (debug_level > 0) {
    timtxt();
    (void)fprintf(stdout,"mdb_rpc_main: back from msrpc_call_mdb\n");
  } 
  
}
