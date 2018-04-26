/*--------------------------------------------------------------------*
 * Program        : tmdbsp.c - IBM. Subroutine name = tmdbp_1
 *
 * Language       : C
 *                : NOTE!! - Some RPC routines included in <rpc/rpc.h>
 *                : don't conform to ANSI C.
 *
 * Description    : This is the RPC C MDB server program on the IBM.
 *                : It receives arguments from the RPC C server stub
 *                : produced using ONC RPCGEN on the IBM and passes
 *                : them to the MetDB Fortran subroutine MDB via a
 *                : assembler interface (ASMRPC) needed to handle
 *                : variable length character strings.
 *
 * Called by      : RPC C server stub.
 *
 * Calls          : ASMRPC  : Assembler interface which calls the
 *                          : FORTRAN subroutine MDB
 *
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:36:15$
 * $Source: /home/us0400/mdb/op/lib/RPC/server/main/RCS/ssrpc_IBM.c,v $
 *
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:36:15    Kudsia Gwangwaa 
 * $
 * Revision 1.3  2000/06/16  12:00:03  12:00:03  usmdb (Generic MDB account)
 * Addition of code to return binary data to client in
 * variable crep_binary - S.Cox
 * 
 * Revision 1.2  99/03/15  11:07:28  11:07:28  usmdb (Generic MDB account)
 * 15-03-1999 S.Cox
 * Addition of DEBUG directives. To enable debugging mode,
 * compile source with CPARM='DEFINE(DEBUG=)'
 *
 * Revision 1.1  1998/02/03 09:58:02  usmdb
 * Initial revision
 *
 * 21-02-97 3.1 : Add a NULL termination to the strings cstr and crep.
 *              : This is needed by the XDR routines and was causing
 *              : problems on the Cray T3E.
 *
 * 27-11-1995   : Calls an assembler interface.
 *
 * 16-11-1995   : Comments added
 *
 * 10-08-1995   : Written
 *
 *--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*
 * #define MVS                     : This is necessary to define the
 *                                 : MVS environment for C. Without it,
 *                                 : various functions will not be
 *                                 : recognised by the linkage editor.
 *
 * #pragma runopts(PLIST(MVS),     : A #pragma directive is used to
 *                 EXECOPS)        : specify some run-time options that
 *                                 : are to be used at execution time.
 *                                 : PLIST(MVS) specifies that the
 *                                 : parameter list recived by the C
 *                                 : program is in MVS format. EXECOPS
 *                                 : specifies that run-time options
 *                                 : can be specified on the invocation
 *                                 : line. Not sure if this #pragma is
 *                                 : needed, but does no harm.
 *
 * #pragma linkage(asmrpc,OS)      : A #pragma directive is used to
 *                                 : define the Assmbler linkage to C.
 *                                 : asmrpc is the Assembler routine
 *                                 : called. OS specifies Assembler as
 *                                 : the language.
 *--------------------------------------------------------------------*/

#define MVS
#pragma runopts(PLIST(MVS),EXECOPS)
#pragma linkage(asmrpc,OS)

/*--------------------------------------------------------------------*
 * Standard include files needed :
 *
 * math.h       : needed for interlanguage calls with FORTRAN.
 * stdio.h      : needed for standard C i/o
 * stdlib.h     : needed for malloc calls on HDS.
 * stdarg.h     : not sure if this is needed, but does no harm.
 * rpc/rpc.h    : needed for all the RPC calls.
 *
 * Local include file needed :
 *
 * ssrpc.h      : produced by RPCGEN. Needed by client and server
 *              : procedures. Contains argument declarations, program
 *              : and version numbers.
 *--------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <rpc/rpc.h>
#include "ssrpc.h"

/*--------------------------------------------------------------------*
 * C Procedure declaration. Procedure is called tmdbp_1, receives the
 * argument input_arg, and returns an argument of type pointer to
 * structure op_param which is a structure defined in the local include
 * file "ssrpc.h"
 *
 * Input argument (input_arg) to the procedure tmdbp_1 is declared of
 * type pointer to structure ip_param which is a structure defined in
 * the local include file "ssrpc.h"
 *--------------------------------------------------------------------*/

op_param *tmdbp_1(input_arg)

ip_param *input_arg;
{

/*--------------------------------------------------------------------*
 * Declare variables:
 *
 * output_arg  : This is the argument returned to the calling program.
 *             : It contains MetDB returned arguments and is declared
 *             : of type structure op_param, where op_param is defined
 *             : in the local include file "ssrpc.h". This return value
 *             : MUST declared as static, otherwise there is the risk
 *             : of the values becoming undefined when you leave the
 *             : scope of the server routine and return to the
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
 * output_arg.array.array_val  : MetDB returned ARRAY values.
 * output_arg.cstr             : MetDB returned CSTR values.
 * output_arg.crep             : MetDB returned CREP values (text).
 * output_arg.crep_binary      : MetDB returned CREP values (binary).
 *--------------------------------------------------------------------*/

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: Start of routine, free_space = %d\n",
                free_space);
#endif /* DEBUG */

  if (free_space != 0) {

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to free array\n");
#endif /* DEBUG */

    free(output_arg.array.array_val);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: After free array\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to free cstr\n");
#endif /* DEBUG */

    free(output_arg.cstr);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: After free cstr\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to free crep\n");
#endif /* DEBUG */

    free(output_arg.crep);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: After free crep\n");
#endif /* DEBUG */

/* 1.3 */
#ifdef DEBUG
  (void)fprintf(stdout,"call_mdbp_1: About to free crep_binary\n");
#endif /* DEBUG */
    free(output_arg.crep_binary.crep_binary_val);


#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to call xdr_free\n");
#endif /* DEBUG */

    xdr_free(xdr_op_param,&output_arg);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: After xdr_free\n");
#endif /* DEBUG */

  }
  else {
    free_space = 1;
  }

/*--------------------------------------------------------------------*
 * look for "RETBUFR" or "RETGRIB" or "BINARY" in the CREQ request
 * string. If found, user requests binary data to be returned in string
 * CREP. The strstr function should return a pointer to char, but on
 * the IBM mainframe it seems to return an integer, hence the != 0
 * rather than != NULL.
 *--------------------------------------------------------------------*/

/* 1.3 */
  request_binary = 0;
  if (strstr(input_arg->creq,"RETBUFR") != 0 ||
      strstr(input_arg->creq,"RETGRIB") != 0 ||
      strstr(input_arg->creq,"BINARY") != 0) {
    request_binary = 1;
  }
#ifdef DEBUG
  (void)fprintf(stdout,"call_mdbp_1: request_binary = %d\n",
  request_binary);
#endif /* DEBUG */

/*--------------------------------------------------------------------*
 * This will not remain in the server code, but is used to "crash" the
 * server procedure when it is called with the MetDB ISTAT argument
 * set to 99.
 *--------------------------------------------------------------------*/

  if (input_arg->istat == 99) {

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: User call to kill server\n");
#endif /* DEBUG */

     (void)fprintf(stderr,"--------------------------------------\n");
     (void)fprintf(stderr,"You are in subroutine tmdbp_1         \n");
     (void)fprintf(stderr,"RPC server called with ISTAT=99, EXIT!\n");
     (void)fprintf(stderr,"--------------------------------------\n");

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to call svc_unregister\n");
#endif /* DEBUG */

     svc_unregister(TMDB,TMDBV);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: After svc_unregister\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to exit\n");
#endif /* DEBUG */

     exit(1);
  }

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

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to malloc array\n");
#endif /* DEBUG */

  output_arg.array.array_val = malloc((output_arg.array.array_len)*
  sizeof(float));

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: After malloc array\n");
#endif /* DEBUG */

/*--------------------------------------------------------------------*
 * CSTR(NOBS) : space is allocated for the variable output_arg.cstr
 * according to the length of the string (lcstr - passed by client
 * program), the length of CSTR array (length input_arg->nobs) and the
 * sizeof the type of variable (char). 1 is added for the '0\'
 *--------------------------------------------------------------------*/

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to malloc cstr\n");
#endif /* DEBUG */

  output_arg.cstr = malloc((input_arg->nobs)*(input_arg->lcstr)*
  sizeof(char)+1);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: After malloc cstr\n");
#endif /* DEBUG */

/*--------------------------------------------------------------------*
 * CREP(NOBS) : space is allocated for the variable output_arg.crep
 * according to the length of the string (lcrep - passed by client
 * program), the length of CREP array (length input_arg->nobs) and the
 * sizeof the type of variable (char). 1 is added for the '0\'
 *--------------------------------------------------------------------*/

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to malloc crep\n");
#endif /* DEBUG */

  output_arg.crep = malloc((input_arg->nobs)*(input_arg->lcrep)*
  sizeof(char)+1);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: After malloc crep\n");
#endif /* DEBUG */

/* 1.3 */
#ifdef DEBUG
  (void)fprintf(stdout,"call_mdbp_1: About to malloc crep_binary\n");
#endif /* DEBUG */
  output_arg.crep_binary.crep_binary_len = (input_arg->lcrep)*
  (input_arg->nobs)+1;
  output_arg.crep_binary.crep_binary_val = malloc((input_arg->nobs)*
  (input_arg->lcrep)*sizeof(char)+1);
#ifdef DEBUG
  (void)fprintf(stdout,"call_mdbp_1: After malloc crep_binary\n");
#endif /* DEBUG */

/*--------------------------------------------------------------------*
 * CALL THE MetDB !!!
 * ==================
 *
 * We now call the MetDB. We call the MetDB subroutine MDBRT through
 * an assembler interface (asmrpc). This is necessary in order to pass
 * character strings from the C to Fortran where the Fortran declares
 * them as CHARACTER*(*). See tech note. Parameters need to be passed
 * by reference, so that changes may be made to them, so arguments
 * passed must be addresses.
 *--------------------------------------------------------------------*/

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to call asmrpc\n");
#endif /* DEBUG */

/* 1.3 */
  if (request_binary > 0 ) {
    asmrpc(input_arg->csubt, input_arg->creq,
           output_arg.array.array_val, &input_arg->nobs,
           &input_arg->nelem, &input_arg->istat, output_arg.cstr,
           output_arg.crep_binary.crep_binary_val, &input_arg->lcsubt,
           &input_arg->lcreq, &input_arg->lcstr, &input_arg->lcrep);
  } else {
    asmrpc(input_arg->csubt, input_arg->creq,
           output_arg.array.array_val, &input_arg->nobs,
           &input_arg->nelem, &input_arg->istat, output_arg.cstr,
           output_arg.crep, &input_arg->lcsubt, &input_arg->lcreq,
           &input_arg->lcstr, &input_arg->lcrep);
  }

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: Back from asmrpc\n");
#endif /* DEBUG */

/*--------------------------------------------------------------------*
 * Two variables are ip/op in the call to the MetDB, NOBS and ISTAT.
 * We now  need to re-assign the values input_arg->nobs and
 * input_arg->istat to take on the values of NOBS and ISTAT returned
 * from the MetDB.
 *--------------------------------------------------------------------*/

  output_arg.nobs            = input_arg->nobs;
  output_arg.istat           = input_arg->istat;

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

#ifdef DEBUG
  (void)fprintf(stdout,"tmdbp_1: About to return\n");
#endif /* DEBUG */

  return(&output_arg);
}

static void tmdb_1();

main()
{
	SVCXPRT *transp;

#ifdef DEBUG
  (void)fprintf(stdout,"main: Start of routine\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"main: About to call pmap_unset\n");
#endif /* DEBUG */

	(void)pmap_unset(TMDB, TMDBV);

#ifdef DEBUG
  (void)fprintf(stdout,"main: After pmap_unset\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"main: About to call svcudp_create\n");
#endif /* DEBUG */

	transp = svcudp_create(RPC_ANYSOCK);

#ifdef DEBUG
  (void)fprintf(stdout,"main: After svcudp_create\n");
#endif /* DEBUG */

	if (transp == NULL) {
		(void)fprintf(stderr, "cannot create udp service.\n");
		exit(1);
	}

#ifdef DEBUG
  (void)fprintf(stdout,"main: About to call svc_register for UDP\n");
#endif /* DEBUG */

	if (!svc_register(transp, TMDB, TMDBV, tmdb_1, IPPROTO_UDP)) {
		(void)fprintf(stderr, "unable to register (TMDB, TMDBV, udp).\n");
		exit(1);
	}

#ifdef DEBUG
  (void)fprintf(stdout,"main: After UDP svc_register - OK\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"main: About to call svctcp_create\n");
#endif /* DEBUG */

	transp = svctcp_create(RPC_ANYSOCK, 0, 0);

#ifdef DEBUG
  (void)fprintf(stdout,"main: After svctcp_create\n");
#endif /* DEBUG */

	if (transp == NULL) {
		(void)fprintf(stderr, "cannot create tcp service.\n");
		exit(1);
	}

#ifdef DEBUG
  (void)fprintf(stdout,"main: About to call svc_register for TCP\n");
#endif /* DEBUG */

	if (!svc_register(transp, TMDB, TMDBV, tmdb_1, IPPROTO_TCP)) {
		(void)fprintf(stderr, "unable to register (TMDB, TMDBV, tcp).\n");
		exit(1);
	}

#ifdef DEBUG
  (void)fprintf(stdout,"main: After TCP svc_register - OK\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"main: About to call svc_run\n");
#endif /* DEBUG */

	svc_run();
	(void)fprintf(stderr, "main: svc_run returned\n");
	exit(1);
}

static void
tmdb_1(rqstp, transp)
 struct svc_req *rqstp;
 SVCXPRT *transp;
{
	union {
		ip_param tmdbp_1_arg;
	} argument;
	char *result;
	bool_t (*xdr_argument)(), (*xdr_result)();
	char *(*local)();

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: Start of routine\n");
#endif /* DEBUG */

	switch (rqstp->rq_proc) {
	case NULLPROC:

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: case NULLRPOC\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: About to call svc_sendreply\n");
#endif /* DEBUG */

		(void)svc_sendreply(transp, xdr_void, (char *)NULL);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: After svc_sendreply - return\n");
#endif /* DEBUG */

		return;

	case TMDBP:

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: case TMDBP\n");
#endif /* DEBUG */

		xdr_argument = xdr_ip_param;
		xdr_result = xdr_op_param;
		local = (char *(*)()) tmdbp_1;
		break;

	default:

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: case default\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: About to call svcerr_noproc\n");
#endif /* DEBUG */

		svcerr_noproc(transp);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: After svcerr_noproc - return\n");
#endif /* DEBUG */

		return;
	}
	bzero((char *)&argument, sizeof(argument));

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: About to call svc_getargs\n");
#endif /* DEBUG */

	if (!svc_getargs(transp, xdr_argument, &argument)) {

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: After svc_getargs - FAILED\n");
#endif /* DEBUG */

		svcerr_decode(transp);
		return;
	}

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: After svc_getargs - OK\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: About to call tmdbp_1\n");
#endif /* DEBUG */

	result = (*local)(&argument, rqstp);

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: After tmdbp_1\n");
#endif /* DEBUG */

	if (result != NULL && !svc_sendreply(transp, xdr_result, result)) {
		svcerr_systemerr(transp);
	}

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: About to call svc_freeargs\n");
#endif /* DEBUG */

	if (!svc_freeargs(transp, xdr_argument, &argument)) {
		(void)fprintf(stderr, "tmdb_1: unable to free arguments\n");
		exit(1);
	}

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: After svc_freeargs - OK\n");
#endif /* DEBUG */

#ifdef DEBUG
  (void)fprintf(stdout,"tmdb_1: End of routine\n");
#endif /* DEBUG */

}

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
