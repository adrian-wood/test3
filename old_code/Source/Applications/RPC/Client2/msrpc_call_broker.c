/*----------------------------------------------------------------------------*
 * Program        : msrpc_call_broker  (msrpc_call_broker)                                                
 *
 * Language       : C
 *
 * Description    : This is the RPC C MDB broker client program on the HP. It
 *                : calls the broker server on ukmet-open to start a main MDB
 *                : RPC server and returns the program number to the calling
 *                : program.
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
 * Revision info  :
 *
 * $Revision: 1$
 * $Date: 09/11/2009 10:44:12$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_broker.c,v $
 *
 * Change history :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
 * Revision 1.6  2003/03/14 12:34:46  usmdb
 * Added (char *) to clnt_control call - S.Cox
 *
 * Revision 1.5  2000/09/05  11:00:11  11:00:11  usmdb (Generic MDB account)
 * Server name and broker time out changed. Now set according
 * to global variables set in calling code - S.Cox
 * 
 * Revision 1.4  2000/08/08  11:26:48  11:26:48  usmdb (Generic MDB account)
 * Addition of a sleep(30) command after each client_create
 * attempt. This is to allow the broker servers time to
 * start up if they are down, or if the IBM is running very
 * slowly - S.Cox
 * 
 * Revision 1.3  2000/06/16  11:37:11  11:37:11  usmdb (Generic MDB account)
 * Addition of call to timtxt to output the time at each
 * fprintf - S.Cox
 * 
 * Revision 1.2  99/07/28  12:13:36 usmdb (Generic MDB account)
 * Addition of debug level info. Add a break statement to the "connect to
 * broker" loop, otherwise it performs 5 clnt_creates!
 * 
 * Revision 1.1  1998/02/02 16:01:06  usmdb
 * Initial revision
 *
 * 04-12-1997   : Some subprogram names, variables prefixed with msrpc_ to
 *              : prevent contention with single server RPC work. Also,
 *              : user_prognum (broker program number) passed in - S.Cox
 *
 * 04-04-1997   : written - S.Cox
 *----------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*
 * include files
 *----------------------------------------------------------------------------*/

#include <stdio.h>               /* for standard C i/o */            
#include <stdlib.h>              /* for malloc */            
#include <sys/time.h>            /* to set our own TIMEOUT through clnt_control */ 
#include <rpc/rpc.h>             /* for all the RPC/XDR calls */
#include "msrpc_call_broker.h"   /* local include for broker RPC server */
#include "constants.h"           /* local include for MetDB RPC constants */

#define ENV_VARNAME_TICINFO "METDB_SERVER_TICINFO"

/*----------------------------------------------------------------------------*
 * subroutine name. Takes int variable user_prognum (user program number) and 
 * long variable time_i (cliemt timestamp) as input and returns int 
 * variable PortDetails->ProgNum (main server program number)
 *----------------------------------------------------------------------------*/

int msrpc_call_broker(user_prognum, time_i)

int            user_prognum;     /* user server program number */
long           time_i;           /* client timestamp */
{

/*----------------------------------------------------------------------------*
 * declare local variables.
 *----------------------------------------------------------------------------*/

    CLIENT          *cl;                  /* client handle */
    msrpc_BrokerIn  *ClientDetails;       /* structure to pass to server */
    msrpc_BrokerOut *PortDetails;         /* structure to return from server */
    struct          timeval timeOut;      /* RPC timeout structure */
    int             ScanCount;            /* sscanf function output */
    int             broker_connects = 0;  /* no. of broker connect retries */
    char            TicString[60];        /* line from USER_TIC file */
    char            *char_ticinfo;
    int             sscanf_rc;
    char            tic_file[250];
    static          int first=1;
       
    FILE            *in_file;             /* file containing user TIC info */
    
/*----------------------------------------------------------------------------*
 * allocate memory for the structure ClientDetails, allocate space for the
 * the string "wait" and the users TIC info. Put the client timestamp into
 * the structure and copy the string WAITACTION from constants.h into the
 * structure to pass to the server procedure.
 *----------------------------------------------------------------------------*/

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_broker: start of routine\n");
    }

    ClientDetails=(msrpc_BrokerIn *) malloc(sizeof(msrpc_BrokerIn));

    ClientDetails->wait      = malloc(5);    /*  e.g. "wait"    */
    ClientDetails->Branch    = malloc(4);    /*  e.g. "M12"     */
    ClientDetails->UserId    = malloc(6);    /*  e.g. "T12DB"   */
    ClientDetails->Tic       = malloc(6);    /*  e.g. "CDS00"   */

    ClientDetails->TimeStamp = time_i;

    (void)strcpy(ClientDetails->wait,WAITACTION);

    if (first == 1) {

      first = 0;
          
/*----------------------------------------------------------------------------*
 * Read the users TIC info file name from environment variable. If there
 * is a problem with this, call error routine and return to calling routine 
 * with rc = -99.
 *----------------------------------------------------------------------------*/

      char_ticinfo = getenv(ENV_VARNAME_TICINFO);
      sscanf_rc = sscanf(char_ticinfo, "%s", &tic_file);
    
      if (sscanf_rc < 1) {
        ErrorNoUserTicInfo();
        return(-99);
      }

/*----------------------------------------------------------------------------*
 * Open the TIC info file. If there is a problem, call error routine and
 * return to calling routine with rc = -99
 *----------------------------------------------------------------------------*/

      in_file = fopen(tic_file,"r");

      if (in_file == NULL) {
        ErrorTicFileOpen(&tic_file);
        return(-99);
      }

/*----------------------------------------------------------------------------*
 * Read in TIC info. First 2 dummy lines, then UserId, Branch, Tic. Finally
 * close the file.
 *----------------------------------------------------------------------------*/

      (void)fgets(TicString, sizeof(TicString),in_file);
      (void)fgets(TicString, sizeof(TicString),in_file);

      (void)fgets(TicString, sizeof(TicString),in_file);
      ScanCount = sscanf(TicString,"%s",ClientDetails->UserId);
      (void)fgets(TicString, sizeof(TicString),in_file);
      ScanCount = sscanf(TicString,"%s",ClientDetails->Branch);
      (void)fgets(TicString, sizeof(TicString),in_file);
      ScanCount = sscanf(TicString,"%s",ClientDetails->Tic);

      (void) fclose(in_file);

    }
    
/*----------------------------------------------------------------------------*
 * Try and connect to the MetDB broker server. It should always be running,
 * however, if a there are a lot of client requests to the broker, the
 * clnt_create may take some time & can TIME-OUT! If it does time-out, try to
 * re-connect a max of MAX_BROKER_CONNECTS times. Sleep 30 seconds between
 * each connect retry.
 *----------------------------------------------------------------------------*/
    
    while (broker_connects < MAX_BROKER_CONNECTS) {

      broker_connects++;

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_broker: about to call clnt_create\n");
      }
      
      cl = clnt_create(server_ipname, user_prognum,                   /* 1.5 */
                       MSRPC_CALL_BROKERV,TRANSPORT);                 /* 1.5 */

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_broker: after clnt_create\n");
      }

      if (cl == NULL) {
        if (broker_connects < MAX_BROKER_CONNECTS) {
          timtxt();
          (void)fprintf(stdout,"msrpc_call_broker: clnt_create failed, try again\n");
          sleep(30);   /* 1.4 */
        }
      } else {
        break;
      }        
    }

    if (cl == NULL) {
       ErrorBrokerConnect(user_prognum);
       return(-99);
    }

/*----------------------------------------------------------------------------*
 * Change the default RPC timeout. Call the MetDB broker server. If there is 
 * a failure at the server, return -99 to the calling program, otherwise, free 
 * the memory and return the program number.
 *----------------------------------------------------------------------------*/

    timeOut.tv_sec  = BROKER_TIMEOUT;                                  /* 1.5 */
    timeOut.tv_usec = 0;
    
    if (clnt_control(cl,CLSET_TIMEOUT,(char *)&timeOut) == FALSE) {    /* 1.6 */
       timtxt();
       fprintf(stdout,"In call_broker: TIMEOUT change error\n");
    }
             
    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_broker: about to call " \
                           "msrpc_call_brokerp_1\n");
    }

    PortDetails = msrpc_call_brokerp_1(ClientDetails,cl);

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_broker: after msrpc_call_brokerp_1 " \
                           "call\n");
    }

    if (PortDetails == NULL) {
       ErrorBrokerCall(cl);
       return(-99);
    }

/*----------------------------------------------------------------------------*
 * free malloc'd char strings and xdr free server output. Neatly close the
 * socket and return the program number.
 *----------------------------------------------------------------------------*/

    free(ClientDetails->wait);    
    free(ClientDetails->Branch);    
    free(ClientDetails->UserId);    
    free(ClientDetails->Tic);    

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_broker: about to call xdr_free, " \
                           "clnt_destroy\n");
    }

    xdr_free(xdr_msrpc_BrokerOut, PortDetails);
       
    clnt_destroy(cl);

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_broker: after xdr_free, clnt_destroy, " \
                           "return to calling program\n");
    }

    return(PortDetails->ProgNum);
} 
