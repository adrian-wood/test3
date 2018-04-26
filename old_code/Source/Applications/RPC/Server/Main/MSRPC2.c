/*--------------------------------------------------------------------*
 * Program        : call_mdbsp.c - IBM. Subroutine name = call_mdbp_1
 *
 * Language       : C
 *                : NOTE!! - Some RPC routines included in <rpc/rpc.h>
 *                : don't conform to ANSI C.
 *
 * Description    : This is the RPC C MDB server program on the IBM.
 *                : It receives arguments from the RPC C server stub
 *                : produced using ONC RPCGEN on the IBM and passes
 *                : them to the MetDB Fortran subroutine MDB via a
 *                : c interface (mdbwrap) needed to handle
 *                : variable length character strings.
 *
 * Called by      : RPC C server stub.
 *
 * Calls          : mdbwrap : C interface which calls the
 *                          : FORTRAN subroutine MDB
 *
 * Revision info  :
 *
 * $Workfile: msrpc2.c$ $Folder: Main$
 * $Revision: 6$ $Date: 01/11/2011 10:03:29$
 *
 * Change history :
 *
 * $Log:
 *  6    MetDB_Refresh 1.5         01/11/2011 10:03:29    Sheila Needham
 *       Refreshed version
 *  5    MetDB_Refresh 1.4         13/07/2011 10:53:40    Sheila Needham
 *       Corrected the log file pathname
 *  4    MetDB_Refresh 1.3         13/07/2011 09:19:25    Sheila Needham
 *       Updated for restricted access
 *  3    MetDB_Refresh 1.2         07/07/2011 11:35:26    Sheila Needham  WIP
 *  2    MetDB_Refresh 1.1         09/02/2011 10:33:58    Alison Weir
 *       Updated to use MDBDLL
 *  1    MetDB_Refresh 1.0         09/02/2011 10:03:53    Alison Weir
 *       Original version
 * $
 *
 *--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*
 * #define MVS                     : This is necessary to define the
 *                                 : MVS environment for C. Without it,
 *                                 : various functions will not be
 *                                 : recognised by the linkage editor.
 *
 * #pragma runopts(PLIST(MVS),     : A #pragma directive is used to
 *                 EXECOPS,        : specify some run-time options that
 *                 POSIX(ON))      : are to be used at execution time.
 *                                 : PLIST(MVS) specifies that the
 *                                 : parameter list recived by the C
 *                                 : program is in MVS format. EXECOPS
 *                                 : specifies that run-time options
 *                                 : can be specified on the invocation
 *                                 : line. Not sure if this #pragma is
 *                                 : needed, but does no harm.
 *                                 : POSIX(ON) is required in order for
 *                                 : offline retrievals to work - it
 *                                 : allows the MDBDLL to access the
 *                                 : MASS retrieval scripts.
 *--------------------------------------------------------------------*/

#define MVS
#pragma runopts(PLIST(MVS),EXECOPS,POSIX(ON))

/*--------------------------------------------------------------------*
 * Standard include files needed :
 *
 * math.h       : needed for interlanguage calls with FORTRAN.
 * stdio.h      : needed for standard C i/o
 * stdlib.h     : needed for malloc calls on HDS.
 * sting.h      : needed for strstr function
 * time.h       : needed for time functions.
 * stdarg.h     : not sure if this is needed, but does no harm.
 * rpc/rpc.h    : needed for all the RPC calls.
 *
 * Local include file needed :
 *
 * msrpc.h      : produced by RPCGEN. Needed by client and server
 *              : procedures. Contains argument declarations, program
 *              : and version numbers.
 *--------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <rpc/rpc.h>
#include "msrpc.h"

#define DIAG_PATH_PREFIX "/tmp/mdb"

void rpclog(ip_param *input_arg,FILE *svout);
void check_auth(char *uid, char *ctype, int *access);
/*--------------------------------------------------------------------*
 * C Procedure declaration. Procedure is called call_mdbp_1, receives
 * the argument input_arg, and returns an argument of type pointer to
 * structure op_param which is a structure defined in the local include
 * file "msrpc.h"
 *
 * Input argument (input_arg) to the procedure call_mdbp_1 is declared
 * of type pointer to structure ip_param which is a structure defined
 * in the local include file "msrpc.h"
 *--------------------------------------------------------------------*/

op_param *call_mdbp_1(input_arg)

ip_param *input_arg;
{

/*--------------------------------------------------------------------*
 * Declare variables:
 *
 * output_arg  : This is the argument returned to the calling program.
 *             : It contains MetDB returned arguments and is declared
 *             : of type structure op_param, where op_param is defined
 *             : in the local include file "msrpc.h". This return
 *             : value MUST declared as static, otherwise there is the
 *             : risk of the values becoming undefined when you leave
 *             : the scope of the server routine and return to the
 *             : dispatcher function.
 *
 * free_space  : This argument is of type int and is initially set to
 *             : zero to indicate that memory space does not need
 *             : freeing. So, for the first call to the subroutine,
 *             : memory space does not need to be freed, because it
 *             : hasn't yet been "malloc'd", but for subsequent calls,
 *             : free_space is set to 1 to indicate that memory does
 *             : need freeing. The argument is declared as static so
 *             : that the value is preserved for the next subroutine
 *             : call.
 *
 * request_binary : is set to 1 if the user requests binary data to
 *                : returned in CREP, otherwise the user is requesting
 *                : text data.
 *--------------------------------------------------------------------*/

  static op_param output_arg;
  static int free_space = 0;
  static int request_binary;                                   /* 1.3 */
  char   *char_time;
  long   int_time;
  FILE                   *svout;        /* diagnostic o/p file struct */
  static char            outfile[300];  /* diagnostic o/p filename    */
  char text[120];
  int  log_ok;
  int  access;

/*--------------------------------------------------------------------*
 * Free previously "malloc'd" memory.
 *
 * The permanent (static) variable free_space is checked to see whether
 * the call to the server subroutine is the first call or not. If
 * free_space = 0, the call is the first call, and memory does not need
 * freeing as it has not yet been "malloc'd" and free_space is set
 * equal to 1. For subsequent calls, malloc'd" memory will need to be
 * freed (free_space = 1). The three areas of "malloc'd" space to free
 * are :
 *
 * output_arg.array            : MetDB returned ARRAY values.
 * output_arg.cstr             : MetDB returned CSTR values.
 * output_arg.crep             : MetDB returned CREP values (text).
 * output_arg.crep_binary      : MetDB returned CREP values (binary).
 *--------------------------------------------------------------------*/

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: MetDB Server V3.1\n");
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: Start of routine\n");
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: free_space = %d\n",free_space);

  if (free_space != 0) {
    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: About to free array\n");
    free(output_arg.array.array_val);

    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: About to free cstr\n");
    free(output_arg.cstr);

    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: About to free crep\n");
    free(output_arg.crep);

/* 1.3 */
    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: About to free crep_binary\n");
    free(output_arg.crep_binary.crep_binary_val);

    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: About to xdr_free\n");
    xdr_free(xdr_op_param,&output_arg);
    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: After xdr_free\n");

/* log call details if different call */

    svout = fopen(outfile, "a+");
    if (svout == NULL)
      (void)fprintf(stdout,"rpclog: cannot open %s\n",
      outfile);
    else {
      if(input_arg->istat == 0)(void)rpclog(input_arg,svout);
    }
  }
  else {                 /* FIRST CALL  */
    int_time  = input_arg->TimeStamp;

    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: About to malloc char_time\n");
    char_time = malloc(40);
    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: After malloc char_time\n");

    char_time = ctime(&int_time);

    (void)fprintf(stdout,"--------------------------------------\n");
    (void)fprintf(stdout,"call_mdbp_1: Client timestamp = %d\n",
    input_arg->TimeStamp);
    (void)fprintf(stdout,"call_mdbp_1: Client timestamp = %s",
    char_time);
    (void)fprintf(stdout,"--------------------------------------\n");

    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: About to free char_time\n");
    free(char_time);
    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: After free char_time\n");

/*--------------------------------------------------------------------*
 * Open the output file for logging
 *--------------------------------------------------------------------*/
     srand(time(NULL)); /* seed random no. generator  */
     sprintf(outfile,"%s/rpclog_%d%d",DIAG_PATH_PREFIX,
     input_arg->TimeStamp,rand());

     svout = fopen(outfile, "a+");
     if (svout == NULL)
       (void)fprintf(stdout,"rpclog: cannot open %s\n",
       outfile);
     else
       (void)rpclog(input_arg,svout);

    free_space = 1;
    check_auth(input_arg->userid,input_arg->csubt,&access);
    if (access == -1 ) {
      sprintf(text,"Access denied to %s\n",input_arg->csubt);
      (void)fputs(text,svout);
    }
  }

/*--------------------------------------------------------------------*
 * look for "RETBUFR" or "RETGRIB" or "BINARY" in the CREQ request
 * string. If found, user requests binary data to be returned in string
 * CREP.
 *--------------------------------------------------------------------*/

/* 1.3 */
  request_binary = 0;
  if (strstr(input_arg->creq,"RETBUFR") != NULL ||
      strstr(input_arg->creq,"RETGRIB") != NULL ||
      strstr(input_arg->creq,"BINARY") != NULL) {
    request_binary = 1;
  }

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: client contact= %s\n"
                  ,input_arg->contact);
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: client ip= %s\n"
                  ,input_arg->client_ip);
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: userid= %s\n"
                  ,input_arg->userid);
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: request_binary = %d\n",
  request_binary);
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: csubt = %s\n",input_arg->csubt);
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: creq  = %s\n",input_arg->creq);
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: nobs  = %d\n",input_arg->nobs);
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: nelem = %d\n",input_arg->nelem);
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: istat = %d\n",input_arg->istat);

/*--------------------------------------------------------------------*
 * Determine the array length of the array to return to the client
 * calling program. It will be of length NOBS*NELEM, where:
 *
 * NELEM = input_arg->nelem
 * NOBS  = input_arg->nobs
 *--------------------------------------------------------------------*/

  output_arg.array.array_len = (input_arg->nobs)*(input_arg->nelem);

/*--------------------------------------------------------------------*
 * Allocate memory for arrays/strings.
 *
 * At the moment, all we know about the output arrays ARRAY, CSTR and
 * CREP is the position of the starting address in memory (pointer).
 * We need to allocate a size in memory for them. This is done using
 * the malloc function defined in the standard include file <stdlib.h>
 *--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*
 * ARRAY(NOBS,NELEM) : space is allocated for the variable
 * output_arg.array.array_val according to the length of the array
 * (calculated previously) and the sizeof the type of variable (float)
 *--------------------------------------------------------------------*/

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: About to malloc array\n");

  output_arg.array.array_val = malloc((output_arg.array.array_len)*
  sizeof(float));

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: After malloc array\n");

/*--------------------------------------------------------------------*
 * CSTR(NOBS) : space is allocated for the variable output_arg.cstr
 * according to the length of the string (lcstr - passed by client
 * program), the length of CSTR array (length input_arg->nobs) and the
 * sizeof the type of variable (char). 1 is added for the '0\'
 *--------------------------------------------------------------------*/

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: About to malloc cstr\n");

  output_arg.cstr = malloc((input_arg->nobs)*(input_arg->lcstr)*
  sizeof(char)+1);

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: After malloc cstr\n");

/*--------------------------------------------------------------------*
 * CREP(NOBS) : space is allocated for the variable output_arg.crep
 * according to the length of the string (lcrep - passed by client
 * program), the length of CREP array (length input_arg->nobs) and the
 * sizeof the type of variable (char). 1 is added for the '0\'
 *--------------------------------------------------------------------*/

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: About to malloc crep\n");

  output_arg.crep = malloc((input_arg->nobs)*(input_arg->lcrep)*
  sizeof(char)+1);

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: After malloc crep\n");

/* 1.3 */
  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: About to malloc crep_binary\n");

  output_arg.crep_binary.crep_binary_len = (input_arg->lcrep)*
  (input_arg->nobs)+1;
  output_arg.crep_binary.crep_binary_val = malloc((input_arg->nobs)*
  (input_arg->lcrep)*sizeof(char)+1);

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: After malloc crep_binary\n");

/*--------------------------------------------------------------------*
 * CALL THE MetDB !!!
 * ==================
 *
 * We now call the MetDB. We call the MetDB subroutine MDB through
 * a C interface (mdbwrap), both of which are in a dll, MDBDLL.
 * The interface is necessary in order to pass
 * character strings from the C to Fortran where the Fortran declares
 * them as CHARACTER*(*). See tech note. Parameters need to be passed
 * by reference, so that changes may be made to them, so arguments
 * passed must be addresses. However, the character string lengths
 * are actual values.
 *--------------------------------------------------------------------*/

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: About to call mdbwrap\n");

  if (access != -1) {
/* 1.3 */
  if (request_binary > 0 ) {
    mdbwrap(input_arg->csubt, input_arg->creq,
           output_arg.array.array_val, &input_arg->nobs,
           &input_arg->nelem, &input_arg->istat, output_arg.cstr,
           output_arg.crep_binary.crep_binary_val, input_arg->lcsubt,
           input_arg->lcreq, input_arg->lcstr, input_arg->lcrep);
  } else {
    mdbwrap(input_arg->csubt, input_arg->creq,
           output_arg.array.array_val, &input_arg->nobs,
           &input_arg->nelem, &input_arg->istat, output_arg.cstr,
           output_arg.crep, input_arg->lcsubt, input_arg->lcreq,
           input_arg->lcstr, input_arg->lcrep);
  }
  } else {
    timtxt();
    (void)fprintf(stdout,"call_mdbp_1: Access denied to %s\n",
           input_arg->csubt);
    input_arg->istat = 16;
    input_arg->nobs = 0;
  }

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: Back from mdbwrap\n");

/*--------------------------------------------------------------------*
 * Two variables are ip/op in the call to the MetDB, NOBS and ISTAT.
 * We now  need to re-assign the values input_arg->nobs and
 * input_arg->istat to take on the values of NOBS and ISTAT returned
 * from the MetDB.
 *--------------------------------------------------------------------*/

  output_arg.nobs  = input_arg->nobs;
  output_arg.istat = input_arg->istat;

/*--------------------------------------------------------------------*
 * Need to NULL terminate output_arg.cstr, output_arg.crep and
 * output_arg.crep_binary. This is necessary for the XDR routines.
 * The problem came to light with Cray T3E -> HDS communication.
 *--------------------------------------------------------------------*/

  output_arg.cstr[input_arg->lcstr*input_arg->nobs]='\0';
  output_arg.crep[input_arg->lcrep*input_arg->nobs]='\0';

/* 1.3 */
  output_arg.crep_binary.crep_binary_val[input_arg->lcrep*
  input_arg->nobs]='\0';

/*--------------------------------------------------------------------*
 * Return to the client calling program. Return the output_arg
 * structure (pointer to it)
 *--------------------------------------------------------------------*/

  timtxt();
  (void)fprintf(stdout,"call_mdbp_1: About to return\n");

/* close log file */
  fclose(svout);

  return(&output_arg);
}

/*====================================================================*
 * KillServer function - To kill the server and unregister the
 * ProgNum from portmapper.
 *====================================================================*/

void *killserverp_1(killsv_input)

killsv_ip_param *killsv_input;
{

  char *char_time;
  long int_time;

  timtxt();
  (void)fprintf(stdout,"killserverp_1: Start of routine\n");

  int_time  = killsv_input->TimeStamp;
  char_time = malloc(40);
  char_time = ctime(&int_time);

  (void)fprintf(stdout,"--------------------------------------\n");
  (void)fprintf(stdout,"You are in subroutine killserver.     \n");
  (void)fprintf(stdout,"RPC server called with ISTAT=99, EXIT!\n");
  (void)fprintf(stdout,"Client timestamp = %d\n", \
                        killsv_input->TimeStamp);
  (void)fprintf(stdout,"Client timestamp = %s",char_time);
  (void)fprintf(stdout,"--------------------------------------\n");

  timtxt();
  (void)fprintf(stdout,"killserverp_1: About to call svc_unregister\n");

  svc_unregister(killsv_input->prognum, CALL_MDBV);

  timtxt();
  (void)fprintf(stdout,"killserverp_1: After svc_unregister\n");

  free(char_time);

  timtxt();
  (void)fprintf(stdout,"killserverp_1: About to exit(0)\n");

  exit(0);
}

/*-------------------------------------------------------------------*
 * Program        : timtxt (timtxt.c)
 *
 * Language       : C
 *
 * Description    : Function to print the current time.
 *
 * Called by      : various
 *
 * Calls          : time  (system time routine)
 *                : localtime (system time routine)
 *                : strftime (system time routine)
 *
 * Revision info  :
 *
 * $Workfile: msrpc2.c$ $Folder: Main$
 * $Revision: 6$ $Date: 01/11/2011 10:03:29$
 *
 * Change history :
 *
 * $Log:
 *  6    MetDB_Refresh 1.5         01/11/2011 10:03:29    Sheila Needham
 *       Refreshed version
 *  5    MetDB_Refresh 1.4         13/07/2011 10:53:40    Sheila Needham
 *       Corrected the log file pathname
 *  4    MetDB_Refresh 1.3         13/07/2011 09:19:25    Sheila Needham
 *       Updated for restricted access
 *  3    MetDB_Refresh 1.2         07/07/2011 11:35:26    Sheila Needham  WIP
 *  2    MetDB_Refresh 1.1         09/02/2011 10:33:58    Alison Weir
 *       Updated to use MDBDLL
 *  1    MetDB_Refresh 1.0         09/02/2011 10:03:53    Alison Weir
 *       Original version
 * $
 *
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <time.h>

timtxt()
{

/*-------------------------------------------------------------------*
 * declare local variables
 *-------------------------------------------------------------------*/

  char time_text[19];
  time_t time_now;
  struct tm *datim;

/*-------------------------------------------------------------------*
 * call system time routines to get the current time and format it
 * in string time_text. Output time_text.
 *-------------------------------------------------------------------*/

  time(&time_now);
  datim = localtime(&time_now);
  strftime(time_text, 19, "%d/%m/%y %H:%M:%S ", datim);

  (void)fprintf(stdout,"%s ", time_text);

  return;
}

/*====================================================================*
 * Server RPC stub (produced by RPCGEN) - Include statements removed.
 * ProgNum is passed to the main server in argc. ProgNum is used in
 * the place of CALL_MDB on starting the server.
 *====================================================================*/

static void call_mdb_1();

int ProgNum;

main(argc,argv)

int   argc;
char *argv[];

{
 SVCXPRT *transp;

 timtxt();
 (void)fprintf(stdout,"main: Start of routine\n");

 sscanf(argv[1], "%d", &ProgNum);

 timtxt();
 (void)fprintf(stdout,"main: ProgNum = %d\n",ProgNum);
 timtxt();
 (void)fprintf(stdout,"main: About to call pmap_unset\n");

 (void)pmap_unset(ProgNum, CALL_MDBV);

 timtxt();
 (void)fprintf(stdout,"main: After pmap_unset\n");
 timtxt();
 (void)fprintf(stdout,"main: About to call svcudp_create\n");

 transp = svcudp_create(RPC_ANYSOCK);

 timtxt();
 (void)fprintf(stdout,"main: After svcudp_create\n");

 if (transp == NULL) {
  (void)fprintf(stdout, "cannot create udp service.\n");
  exit(1);
 }

 timtxt();
 (void)fprintf(stdout,"main: About to call svc_register for UDP\n");

 if (!svc_register(transp, ProgNum, CALL_MDBV, call_mdb_1, \
 IPPROTO_UDP)) {
  (void)fprintf(stdout, "unable to register (CALL_MDB, \
  CALL_MDBV, udp).\n");
  exit(1);
 }

 timtxt();
 (void)fprintf(stdout,"main: After UDP svc_register - OK\n");
 timtxt();
 (void)fprintf(stdout,"main: About to call svctcp_create\n");

 transp = svctcp_create(RPC_ANYSOCK, 0, 0);
 timtxt();
 (void)fprintf(stdout,"main: After svctcp_create\n");
 if (transp == NULL) {
  (void)fprintf(stdout, "cannot create tcp service.\n");
  exit(1);
 }

 timtxt();
 (void)fprintf(stdout,"main: About to call svc_register for TCP\n");

 if (!svc_register(transp, ProgNum, CALL_MDBV, call_mdb_1, \
 IPPROTO_TCP)) {
  (void)fprintf(stdout, "unable to register (CALL_MDB, \
  CALL_MDBV, tcp).\n");
  exit(1);
 }

 timtxt();
 (void)fprintf(stdout,"main: After TCP svc_register - OK\n");
 timtxt();
 (void)fprintf(stdout,"main: About to call svc_run\n");

 svc_run();

 timtxt();
 (void)fprintf(stdout,"main: svc_run returned - exit\n");

 exit(1);
}

static void
call_mdb_1(rqstp, transp)

 struct svc_req *rqstp;
 SVCXPRT *transp;
{
 union {
  ip_param call_mdbp_1_arg;
  killsv_ip_param killserverp_1_arg;

 } argument;
 char *result;
 bool_t (*xdr_argument)(), (*xdr_result)();
 char *(*local)();

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: Start of routine\n");

 switch (rqstp->rq_proc) {
 case NULLPROC:

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: case NULLRPOC\n");
 timtxt();
 (void)fprintf(stdout,"call_mdb_1: About to call svc_sendreply\n");

 (void)svc_sendreply(transp, xdr_void, (char *)NULL);

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: After svc_sendreply - return\n");

 return;

 case CALL_MDBP:

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: case TMDBP\n");

  xdr_argument = xdr_ip_param;
  xdr_result = xdr_op_param;
  local = (char *(*)()) call_mdbp_1;

  break;

 case KILLSERVERP:

  timtxt();
  (void)fprintf(stdout,"call_mdb_1: case KILLSERVERP\n");

  xdr_argument = xdr_killsv_ip_param;
  xdr_result = xdr_void;
  local = (char *(*)()) killserverp_1;

  break;

 default:

  timtxt();
  (void)fprintf(stdout,"call_mdb_1: case default\n");

  timtxt();
  (void)fprintf(stdout,"call_mdb_1: About to call svcerr_noproc\n");

  svcerr_noproc(transp);

  timtxt();
  (void)fprintf(stdout,"call_mdb_1: After svcerr_noproc - return\n");
    return;
  }
 bzero((char *)&argument, sizeof(argument));

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: About to call svc_getargs\n");

 if (!svc_getargs(transp, xdr_argument, &argument)) {

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: After svc_getargs - FAILED\n");

 svcerr_decode(transp);
 return;
 }

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: After svc_getargs - OK\n");
 timtxt();
 (void)fprintf(stdout,"call_mdb_1: About to call server proc\n");

 result = (*local)(&argument, rqstp);

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: After server proc\n");

/* 1.4 */
 if (result == NULL) {
   timtxt();
   (void)fprintf(stdout,"call_mdb_1: result == NULL !\n");
 }

 if (result != NULL && !svc_sendreply(transp, xdr_result, result)) {
/* 1.4 */
  timtxt();
  (void)fprintf(stdout,"call_mdb_1: svc_sendreply FAILED !\n");
  svcerr_systemerr(transp);
 }

/* 1.4 */
 timtxt();
 (void)fprintf(stdout,"call_mdb_1: After svc_sendreply\n");

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: About to call svc_freeargs\n");

 if (!svc_freeargs(transp, xdr_argument, &argument)) {
  (void)fprintf(stdout, "unable to free arguments\n");
  exit(1);
 }

 timtxt();
 (void)fprintf(stdout,"call_mdb_1: After svc_freeargs - OK\n");
 timtxt();
 (void)fprintf(stdout,"call_mdb_1: End of routine\n");

}

/*====================================================================*
 * Server XDR stub (produced by RPCGEN) - Include statements removed.
 *====================================================================*/

bool_t
xdr_ip_param(xdrs, objp)
 XDR *xdrs;
 ip_param *objp;
{
 if (!xdr_string(xdrs, &objp->csubt, ~0)) {
  return (FALSE);
 }
 if (!xdr_string(xdrs, &objp->creq, ~0)) {
  return (FALSE);
 }
 if (!xdr_int(xdrs, &objp->nobs)) {
  return (FALSE);
 }
 if (!xdr_int(xdrs, &objp->nelem)) {
  return (FALSE);
 }
 if (!xdr_int(xdrs, &objp->istat)) {
  return (FALSE);
 }
 if (!xdr_int(xdrs, &objp->lcsubt)) {
  return (FALSE);
 }
 if (!xdr_int(xdrs, &objp->lcreq)) {
  return (FALSE);
 }
 if (!xdr_int(xdrs, &objp->lcstr)) {
  return (FALSE);
 }
 if (!xdr_int(xdrs, &objp->lcrep)) {
  return (FALSE);
 }
 if (!xdr_long(xdrs, &objp->prognum)) {
  return (FALSE);
 }
 if (!xdr_long(xdrs, &objp->TimeStamp)) {
  return (FALSE);
 }
 if (!xdr_string(xdrs, &objp->userid, ~0)) {
  return (FALSE);
 }
 if (!xdr_string(xdrs, &objp->client_ip, ~0)) {
  return (FALSE);
 }
 if (!xdr_string(xdrs, &objp->contact, ~0)) {
  return (FALSE);
 }
 return (TRUE);
}

bool_t
xdr_op_param(xdrs, objp)
 XDR *xdrs;
 op_param *objp;
{
 if (!xdr_array(xdrs, (char **)\
&objp->array.array_val, (u_int *)&objp->array.array_len, ~0, sizeof(\
float), xdr_float)) {
  return (FALSE);
 }
 if (!xdr_int(xdrs, &objp->nobs)) {
  return (FALSE);
 }
 if (!xdr_int(xdrs, &objp->istat)) {
  return (FALSE);
 }
 if (!xdr_string(xdrs, &objp->cstr, ~0)) {
  return (FALSE);
 }
 if (!xdr_string(xdrs, &objp->crep, ~0)) {
  return (FALSE);
 }

/* 1.3 */
 if (!xdr_bytes(xdrs, (char **)&objp->\
crep_binary.crep_binary_val, (u_int *)&objp->\
crep_binary.crep_binary_len, ~0)) {
  return (FALSE);
 }
 return (TRUE);
}

bool_t
xdr_killsv_ip_param(xdrs, objp)
 XDR *xdrs;
 killsv_ip_param *objp;
{
 if (!xdr_long(xdrs, &objp->TimeStamp)) {
  return (FALSE);
 }
 if (!xdr_long(xdrs, &objp->prognum)) {
  return (FALSE);
 }
 return (TRUE);
}
