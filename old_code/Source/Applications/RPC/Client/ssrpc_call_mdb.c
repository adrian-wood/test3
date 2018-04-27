/*---------------------------------------------------------------------------*
 * Program        : ssrpc_call_mdb.c
 *
 * Language       : C
 *
 * Description    : This is the single RPC server MDB client program.
 *                : It receives MDB arguments from mdb_rpc_main and passes 
 *                : them to the RPC C client stub (produced using RPCGEN) 
 *
 * Called by      : mdb_rpc.main
 *
 * Calls          : clnt_create        : RPC routine to create the CLIENT 
 *                :                    : structure for the specified server 
 *                :                    : host, program and version numbers 
 *                :                    : and transport protocol
 *                : clnt_control       : RPC routine to change the CLIENT
 *                :                    : TIMEOUT period. 
 *                : ssrpc_call_mdbp_1  : RPC C client stub procedure
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:27:20$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/ssrpc_call_mdb.c,v $
 * 
 * Change history : 
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:27:20    Kudsia Gwangwaa 
 * $
 * Revision 1.3  2003/03/14 12:46:55  usmdb
 * Combined HP/T3E/SX6 version using pre-processor statements - S.Cox
 *
 * Revision 1.2  2000/06/16  11:48:36  11:48:36  usmdb (Generic MDB account)
 * Addition of code to allow binary RPC transfer of
 * data in crep_binary structure - S.Cox
 * 
 * Revision 1.1  98/02/02  16:02:47  16:02:47  usmdb (Generic MDB account)
 * Initial revision
 *
 * 04-12-1997     : Now called by mdb_rpc.main - S.Cox 
 *
 * 16-10-1997     : Read in RPC program number from environment variable
 *                : METDB_SERVER_NUMBER. Also, don't print error message
 *                : for server crash, if the user is "killing" the server
 *                : with an ISTAT of 99 - S.Cox
 *
 * 11-02-1997     : xdr_free added to release memory associated with 
 *                : output_arg - S.Cox
 *
 * 16-11-1995     : Re-write & Comments added - S.Cox
 *
 * 10-08-1995     : Written - S.Cox
 *
 *---------------------------------------------------------------------------*/
 
/*---------------------------------------------------------------------------*
 * Standard include files needed :
 *
 * stdio.h      : needed for standard C i/o
 * stdlib.h     : needed for malloc
 * string.h     : needed for memcpy function.
 * sys/time.h   : needed to set our own TIMEOUT through clnt_control
 * rpc/rpc.h    : needed for all the RPC calls.
 *
 * Local include file needed :
 *
 * ssrpc_call_mdb.h  : produced by RPCGEN. Needed by client and server 
 *                   : procdeures. Contains argument declarations, program 
 *                   : and version numbers.
 *---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <rpc/rpc.h>
#include "ssrpc_call_mdb.h"
 
#if defined (T3E)
#include <fortran.h>                      /* for C/Fortran conversion on T3E */
#endif

/*---------------------------------------------------------------------------*
 * define global constants
 *
 * SERVER       : server machine. "ukmet" = IBM.
 * TRANSPORT    : transport protocol = TCP.
 * MYTIMEOUT    : set TIMEOUT limit to 10000 seconds.
 * ENV_VARNAME  : environment variable name for RPC program number
 *---------------------------------------------------------------------------*/

#define SERVER "ukmet"
#define TRANSPORT "tcp"
#define MYTIMEOUT 10000

/*---------------------------------------------------------------------------*
 * Subroutine name ssrpc_call_mdb.
 * 
 * Receives arguments from the mdb_rpc_main. Variables are passed
 * by reference, so changes may be made to them, so the input arguments are
 * also the output arguments. The return argument from the subroutine is 
 * therefore of type void. See the below description of arguments for more
 * details.
 *---------------------------------------------------------------------------*/

#if defined (T3E)
 void ssrpc_call_mdb(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,
	             CSTR,CREP,user_prognum)
#else
 void ssrpc_call_mdb(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,
                      CSTR,CREP,LCSUBT,LCREQ,LCSTR,LCREP,user_prognum)
#endif
 
/*---------------------------------------------------------------------------*
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
 *---------------------------------------------------------------------------*/

#if defined (T3E)
  _fcd   CSUBT;           /* ip      MDB subtype                             */
  _fcd   CREQ;            /* ip      MDB request string                      */
  _fcd   CSTR;            /* op      MDB report returned as character string */
  _fcd   CREP;            /* op      MDB character string observations       */
  double *ARRAY;          /* op      MDB array of values                     */
  int    *NOBS;           /* ip/op   Number of observations per MDB call     */
  int    *NELEM;          /* ip      Number of elements per MDB call         */
  int    *ISTAT;          /* ip/op   MDB status flag                         */
#else
  char     *CSUBT;        /* ip      MDB subtype                             */
  char     *CREQ;         /* ip      MDB request string                      */
  char     *CSTR;         /* op      MDB report returned as character string */
  char     *CREP;         /* op      MDB character string observations       */
  int      LCREQ;         /* hidden  CREQ string length                      */
  int      LCSUBT;        /* hidden  CSUBT string length                     */
  int      LCSTR;         /* hidden  CSTR string length                      */
  int      LCREP;         /* hidden  CREP string length                      */
#if defined (L64)
  double   *ARRAY;        /* op      MDB array of values                     */
  long int *NOBS;         /* ip/op   Number of observations per MDB call     */
  long int *NELEM;        /* ip      Number of elements per MDB call         */
  long int *ISTAT;        /* ip/op   MDB status flag                         */
#else
  float *ARRAY;           /* op      MDB array of values                     */
  int   *NOBS;            /* ip/op   Number of observations per MDB call     */
  int   *NELEM;           /* ip      Number of elements per MDB call         */
  int   *ISTAT;           /* ip/op   MDB status flag                         */
#endif /* L64 */
#endif /* T3E */

int   user_prognum;        /* ip      RPC program number set by user         */

{    

/*---------------------------------------------------------------------------*
 * Declare local variables 
 *
 * *cl           : cl is type pointer to structure CLIENT which is defined in 
 *               : the <rpc/rpc.h> standard include file. It is the "client
 *               : handle", and is unique for each client/server connection.
 *
 * *input_arg    : input_arg is type pointer to structure ssrpc_ip_param which 
 *               : is defined in the "ssrpc_call_mdb.h" local include file. 
 *               : This structure contains the ip arguments passed from the 
 *               : Fortran calling program. 
 *
 * *output_arg   : output_arg is type pointer to structure ssrpc_op_param 
 *               : which is defined in the "ssrpc_call_mdb.h" local include 
 *               : file. This structure contains the op arguments to be 
 *               : passed back to the Fortran calling program.
 *
 * timeOut       : timeOut is of type structure timeval which is defined in
 *               : the <sys/time.h> standard include file. This structure
 *               : is used to tell the RPC, how long to attempt the remote
 *               : call before timing-out.
 *
 * iadd          : integer counter used for looping
 * 
 *---------------------------------------------------------------------------*/

    CLIENT          *cl;
    ssrpc_ip_param  *input_arg;
    ssrpc_op_param  *output_arg;
    struct          timeval timeOut;
    int             request_binary;                                   /* 1.2 */
    char            *temp_csubt;                                      /* 1.4 */
    char            *temp_creq;                                       /* 1.4 */ 
#if defined (T3E) || defined (L64)
    int             iadd;                                             /* 1.3 */  
#endif

#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: Start of routine\n");
#endif

/*---------------------------------------------------------------------------*
 * allocate memory for the structure input_arg. The type cast 
 * (ssrpc_ip_param *) is needed in UNIX C to change the character pointer. In 
 * ANSI C this is not necessary, but does no harm so it is left in. Memory is 
 * allocated for the structure input_arg, with size (bytes) determined using 
 * the expression, sizeof(ssrpc_ip_param) as input_arg is of type pointer to 
 * structure ssrpc_ip_param.
 *---------------------------------------------------------------------------*/

    input_arg=(ssrpc_ip_param *) malloc(sizeof(ssrpc_ip_param));
    
/*---------------------------------------------------------------------------*
 * put the ip values passed by the Fortran calling program into the structure
 * input_arg.
 *---------------------------------------------------------------------------*/

    input_arg->nobs   = *NOBS;
    input_arg->nelem  = *NELEM;
    input_arg->istat  = *ISTAT;

#if defined (T3E)
    input_arg->lcsubt = _fcdlen(CSUBT);
    input_arg->lcreq  = _fcdlen(CREQ);
    input_arg->lcstr  = _fcdlen(CSTR);
    input_arg->lcrep  = _fcdlen(CREP);
#else
    input_arg->lcsubt = LCSUBT;
    input_arg->lcreq  = LCREQ;
    input_arg->lcstr  = LCSTR;
    input_arg->lcrep  = LCREP;
#endif /* T3E */
		        
    temp_csubt = malloc(input_arg->lcsubt+1);
    temp_creq  = malloc(input_arg->lcreq+1);

#if defined (T3E)
    strncpy(temp_csubt,_fcdtocp(CSUBT),input_arg->lcsubt);
    strncpy(temp_creq,_fcdtocp(CREQ),input_arg->lcreq);
#else
    strncpy(temp_csubt,CSUBT,input_arg->lcsubt);
    strncpy(temp_creq,CREQ,input_arg->lcreq);
#endif /* T3E */
				    
    temp_csubt[input_arg->lcsubt] = '\0';
    temp_creq[input_arg->lcreq]   = '\0';

    input_arg->csubt  = temp_csubt;
    input_arg->creq   = temp_creq;
					  
/*---------------------------------------------------------------------------*
 * look for "RETBUFR" in the CREQ request string. If found, user wants binary
 * data to be returned in string CREP
 *---------------------------------------------------------------------------*/

/* 1.2 */
  request_binary = 0;
  if (strstr(input_arg->creq,"RETBUFR") != NULL ||
      strstr(input_arg->creq,"RETGRIB") != NULL ||
      strstr(input_arg->creq,"BINARY") != NULL) {
    request_binary = 1;
  }
  
#ifdef DEBUG
  (void)fprintf(stdout,"ssrpc_call_mdb: request_binary = %d\n", \
	        request_binary);
#endif

/*---------------------------------------------------------------------------*
 * Create CLIENT handle. clnt_create() creates the CLIENT structure for the
 * specified server host, program number, version number and transport protocol.
 * If the server host portmapper has a copy of the program number (user_prognum)
 * registered, i.e. the server procedure is up and running, the CLIENT handle
 * is created.
 *
 * SERVER          : defined at top of this subroutine as "ukmet".
 * user_prognum    : RPC program number.    
 * SSRPC_CALL_MDBV : defined in "ssrpc_call_mdb.h".
 * TRANSPORT       : defined at top of this subroutine as "tcp".
 *---------------------------------------------------------------------------*/

#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: About to call clnt_create\n");
#endif

    cl = clnt_create(SERVER, user_prognum, SSRPC_CALL_MDBV, TRANSPORT);

#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: Back from clnt_create\n");
#endif

/*---------------------------------------------------------------------------*
 * Check to see that the CLIENT handle was created successfully. If not, NULL
 * is returned. In this instance, output an error message , explaining
 * probable failure reason. clnt_pcreateerror() provides further diagnostics
 * by translating CLIENT handle creation errors into something useful at stderr.
 * ISTAT is set to 99 to pass back to the Fortran calling program, and finally
 * the subroutine returns to the Fortran calling program.
 *---------------------------------------------------------------------------*/

    if (cl == NULL) {
       fprintf(stderr,"----------------------------------------------------\n");
       fprintf(stderr,"You are in subroutine ssrpc_call_mdb.c              \n");
       fprintf(stderr,"Problem connecting with server. Server procedure    \n");
       fprintf(stderr,"probably not executing on server machine - check.   \n");
       fprintf(stderr,"Returning to calling Fortran program with ISTAT=99  \n");
       fprintf(stderr,"----------------------------------------------------\n");
       clnt_pcreateerror(SERVER);
       fprintf(stderr,"----------------------------------------------------\n");
       *ISTAT = 99;
       return;
    }

/*---------------------------------------------------------------------------*
 * Overide the default RPC TIMEOUT period. The RPC default TIMEOUT period is
 * 25 seconds. This is not long enough for many MetDB calls. This TIMEOUT
 * period is overridden with MYTIMEOUT (defined at top of subroutine) using
 * the clnt_control RPC function.
 *
 * timeOut.tv_sec  : timeout period in seconds
 * timeOut.tv_usec : ................. and microseconds.
 *
 * variables passed to the function clnt_control are cl (CLIENT handle),
 * CLSET_TIMEOUT (request value to tell function to set timeout), and
 * the address of the timeOut structure.
 *
 * If for any reason the CLSET_TIMEOUT fails, FALSE is returned, and a
 * message is printed for the user. The subroutine will continue to attempt
 * the Remote Procedure Call as it may complete in the default 25 secs.
 *---------------------------------------------------------------------------*/

    timeOut.tv_sec  = MYTIMEOUT;
    timeOut.tv_usec = 0;
    
#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: About to call clnt_control\n");
#endif

    if (clnt_control(cl,CLSET_TIMEOUT,(char *)&timeOut) == FALSE) {   /* 1.3 */
       fprintf(stderr,"----------------------------------------------------\n");
       fprintf(stderr,"You are in subroutine ssrpc_call_mdb.c              \n");
       fprintf(stderr,"Problem occured changing TIMEOUT from 25 secs to    \n");
       fprintf(stderr,"%d secs - RPC continuing with TIMEOUT = 25 secs\n",
       MYTIMEOUT);
       fprintf(stderr,"----------------------------------------------------\n");
    }
    
#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: Back from clnt_control\n");
#endif
    
/*---------------------------------------------------------------------------*
 * CALL THE CLIENT C RPC STUB - calls the Remote MetDB!!
 * ==========================
 *
 * This function was produced using RPCGEN. The structure input_arg and the
 * structure cl (CLIENT handle) are passed to this function and the structure
 * output_arg is returned.
 *---------------------------------------------------------------------------*/

#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: About to call ssrpc_call_mdbp_1\n");
#endif
    
    output_arg = ssrpc_call_mdbp_1(input_arg,cl);

#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: back from ssrpc_call_mdbp_1\n");
#endif
	
/*---------------------------------------------------------------------------*
 * Check to see that the Remote call was successful. If the returned structure
 * output_arg is NULL, there was a problem calling the server. Output a
 * message to the user explaining this. clnt_perror() provides further
 * diagnostics. ISTAT is set to 99 to pass back to the Fortran calling
 * program, and finally the subroutine returns to the Fortran calling program. 
 *---------------------------------------------------------------------------*/

    if (output_arg == NULL) {
      if (input_arg->istat != 99) {
        fprintf(stderr,"---------------------------------------------------\n");
        fprintf(stderr,"You are in subroutine ssrpc_call_mdb.c             \n");
        fprintf(stderr,"Problem calling the server. There was probably a   \n");
        fprintf(stderr,"failure at the server. Check server procedure      \n");
        fprintf(stderr,"output on EJESH and RPC error specified below.     \n");
        fprintf(stderr,"---------------------------------------------------\n");
        clnt_perror(cl, SERVER);
        fprintf(stderr,"---------------------------------------------------\n");
        *ISTAT = 99;
      }
      return;
    }

/*---------------------------------------------------------------------------*
 * Put arguments returned from the Remote procedure into the variables
 * to pass back to the Fortran calling routine. For ISTAT and NOBS this is
 * a simple assignment.
 *---------------------------------------------------------------------------*/

    *ISTAT = output_arg->istat;
    *NOBS  = output_arg->nobs;

/*---------------------------------------------------------------------------* 
 * CSTR : copy the string in output_arg->cstr of length input_arg->lcstr     * 
 * output_arg->nobs*sizeof(char) into CSTR                                   *
 *---------------------------------------------------------------------------*/

#if defined (T3E)
    memcpy(_fcdtocp(CSTR),output_arg->cstr,input_arg->lcstr*
    output_arg->nobs*sizeof(char));     
#else
    memcpy(CSTR,output_arg->cstr,input_arg->lcstr*
    output_arg->nobs*sizeof(char));
#endif
    
/*---------------------------------------------------------------------------* 
 * CREP : copy the string in output_arg->crep of length input_arg->lcrep     * 
 * output_arg->nobs*sizeof(char) into CREP                                   *
 * 1.3 - Added request_binary                                                *
 *---------------------------------------------------------------------------*/

    if (request_binary > 0) {
#if defined (T3E)
      memcpy(_fcdtocp(CREP),output_arg->crep_binary.crep_binary_val,
      input_arg->lcrep*output_arg->nobs*sizeof(char));
#else
      memcpy(CREP,output_arg->crep_binary.crep_binary_val,
      input_arg->lcrep*output_arg->nobs*sizeof(char));
#endif /* T3E */
    } else {
#if defined (T3E)
      memcpy(_fcdtocp(CREP),output_arg->crep,input_arg->lcrep*
      output_arg->nobs*sizeof(char)); 	      
#else
      memcpy(CREP,output_arg->crep,input_arg->lcrep*
      output_arg->nobs*sizeof(char));
#endif /* T3E */
    }

/*---------------------------------------------------------------------------* 
 * ARRAY : If T3E or L64, loop over the obs in output_arg->array.array_val
 * and put them in ARRAY. This is necessary as output_arg->array.array_val
 * is type float, but ARRAY is type double. If float to float copy required,
 * just perform a memcpy.             
 *---------------------------------------------------------------------------*/
    
#if defined (T3E) || defined (L64)
    iadd=0;                                                           /* 1.3 */
    while (iadd<output_arg->array.array_len) {                        /* 1.3 */
      ARRAY[iadd]=output_arg->array.array_val[iadd];                  /* 1.3 */
      iadd=iadd+1;                                                    /* 1.3 */
    }                                                                 /* 1.3 */
#else
    memcpy(ARRAY,output_arg->array.array_val,
    output_arg->array.array_len*sizeof(float));
	
#endif
    
/*---------------------------------------------------------------------------*
 * free memory
 *---------------------------------------------------------------------------*/

    free(temp_csubt);
    free(temp_creq);
    
#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: About to call xdr_free\n");
#endif    
    
    xdr_free(xdr_ssrpc_op_param, output_arg);
       
#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: Back from xdr_free\n");
#endif

/*---------------------------------------------------------------------------*
 * Before returning to the calling Fortran program, we need to neatly close
 * the socket that was opened by the clnt_create call in this program.
 *---------------------------------------------------------------------------*/
    
#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: About to call clnt_destroy\n");
#endif
	
    clnt_destroy(cl);
    
#ifdef DEBUG
    (void)fprintf(stdout,"ssrpc_call_mdb: Back from clnt_destroy\n");
    (void)fprintf(stdout,"ssrpc_call_mdb: About to exit routine\n");
#endif    
} 
