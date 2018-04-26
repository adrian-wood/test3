/*----------------------------------------------------------------------------*
 * Program        : msrpc_call_freepn.c  Subroutine name = msrpc_call_freepn                                                
 *
 * Language       : C
 *
 * Description    : This is the RPC C MDB freepn client program on the HP. It
 *                : calls the freepn server on ukmet-open to free a program 
 *                : number when the cliet has finished with it. 
 *
 * Called by      : msrpc_call_mdb.c
 *
 * Calls          : clnt_create          : RPC routine to create the CLIENT 
 *                                         structure for the specified server 
 *                                         host, program and version numbers
 *                                         and transport protocol. 
 *                : clnt_control         : RPC routine to change the CLIENT 
 *                                         TIMEOUT period. 
 *                : msrpc_call_brokerp_1 : RPC C call broker client procedure
 *                : timtxt               : Print time & diagnostic
 *
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
 * Revision 1.5  2003/03/14 12:36:19  usmdb
 * Added (char *) to clnt_control call - S.Cox
 *
 * Revision 1.4  2000/09/05  11:01:24  11:01:24  usmdb (Generic MDB account)
 * Server name and broker time out changed. Now set according
 * to global variables set in calling code - S.Cox
 * 
 * Revision 1.3  2000/06/16  11:38:13  11:38:13  usmdb (Generic MDB account)
 * Addition of call to timtxt to output the time at each
 * fprintf - S.Cox
 * 
 * Revision 1.2  99/07/28  12:14:20  12:14:20  usmdb (Generic MDB account)
 * Addition of debug output. Switched on by global variable debug_level in 
 * constants.h - S.Cox
 * 
 * Revision 1.1  1998/02/02 16:01:58  usmdb
 * Initial revision
 *
 * 12-01-1998   : Written - S.Cox
 *----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*
 * include files
 *----------------------------------------------------------------------------*/

#include <stdio.h>               /* for standard C i/o */            
#include <stdlib.h>              /* for malloc */            
#include <sys/time.h>            /* to set our own TIMEOUT through clnt_control */ 
#include <rpc/rpc.h>             /* for all the RPC/XDR calls */
#include "msrpc_call_freepn.h"   /* local include for killsv RPC server */
#include "constants.h"           /* local include for MetDB RPC constants */

/*----------------------------------------------------------------------------*
 * subroutine name. Takes int variable user_freepn_prognum (user-defined
 * freepn server program number, sv_prognum (main server program number to
 * free) and long variable TimeStamp (client timestamp). It returns
 * an int variable status.
 *----------------------------------------------------------------------------*/

int msrpc_call_freepn(user_freepn_prognum, sv_prognum, TimeStamp)

int            user_freepn_prognum;   /* user freepn server prognum */
int            sv_prognum;            /* main server prognum to free */
long           TimeStamp;             /* client timestamp */
{

/*----------------------------------------------------------------------------*
 * declare local variables.
 *----------------=------------------------------------------------------------*/

    CLIENT          *cl;                /* client handle */
    freepn_ip_param *freepn_input;      /* structure to pass to server */
    freepn_op_param *freepn_output;     /* structure to return from server */
    struct          timeval timeOut;    /* RPC timeout structure */
    int             status;             /* return status */

/*----------------------------------------------------------------------------*
 * allocate memory for the structure freepn_input.
 *----------------------------------------------------------------------------*/

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_freepn: start of routine\n");
    }

    freepn_input = (freepn_ip_param *) malloc(sizeof(freepn_ip_param));
             
/*----------------------------------------------------------------------------*
 * Try and connect to the MetDB freepn server. It should always be running. If
 * not, return -99 to the calling program.
 *----------------------------------------------------------------------------*/

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_freepn: about to call clnt_create\n");
    }

    cl = clnt_create(server_ipname, user_freepn_prognum,               /* 1.4 */
                     MSRPC_CALL_FREEPNV, TRANSPORT);                   /* 1.4 */

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_freepn: after clnt_create\n");
    }

    if (cl == NULL) {
       ErrorFreepnConnect(sv_prognum);
       return(-99);
    }

/*----------------------------------------------------------------------------*
 * Change the default RPC timeout. Call the MetDB freepn server. If there is
 * a failure at the server, return -99 to the calling program, otherwise,
 * free the memory and return the program number.
 *----------------------------------------------------------------------------*/

    timeOut.tv_sec  = BROKER_TIMEOUT;                                  /* 1.4 */
    timeOut.tv_usec = 0;
    
    if (clnt_control(cl,CLSET_TIMEOUT,(char *)&timeOut) == FALSE) {    /* 1.5 */
       timtxt();
       fprintf(stdout,"In msrpc_call_freepn: TIMEOUT change error\n");
    }

    freepn_input->sv_prognum = sv_prognum;
    freepn_input->TimeStamp  = TimeStamp;

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_freepn: about to call " \
                           "msrpc_call_freepnp_1\n");
    }

    freepn_output = msrpc_call_freepnp_1(freepn_input,cl);

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_freepn: after msrpc_call_freepnp_1 " \
                           "call\n");
    }

    if (freepn_output == NULL) {
       ErrorFreepnCall(cl);
       return(-99);
    }

    status = freepn_output->status;
        
/*----------------------------------------------------------------------------*
 * Free memory. Neatly close socket & return to calling program.
 *----------------------------------------------------------------------------*/

    free(freepn_input);

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_freepn: about to call xdr_free, " \
                           "clnt_destroy\n");
    }

    xdr_free(xdr_freepn_op_param, freepn_output);
       
    clnt_destroy(cl);

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_freepn: after xdr_free, clnt_destroy, " \
                           "return to calling program\n");
    }

    return(status);
} 
