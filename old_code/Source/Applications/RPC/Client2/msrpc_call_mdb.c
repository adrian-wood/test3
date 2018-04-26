/*---------------------------------------------------------------------------*
 * Program        : msrpc_call_mdb
 *
 * Language       : C
 *
 * Description    : This is the multi RPC server MDB client program.
 *                : It receives MDB arguments from mdb_rpc_main and passes
 *                : them to the RPC C client stub (produced using RPCGEN)
 *
 * Called by      : mdb_rpc.main
 *
 * Calls          : clnt_create        : RPC routine to create the CLIENT 
 *                :                    : structure for the specified server 
 *                :                    : host, program and version numbers 
 *                :                    : and transport protocol.
 *                : clnt_control       : RPC routine to change the CLIENT
 *                :                    : TIMEOUT period.
 *                : msrpc_call_broker  : Routine to call RPC broker
 *                : msrpc_call_mdbp_1  : RPC C mdb client stub to call server 
 *                : killserverp_1      : RPC C mdb client stub to kill server
 *                : msrpc_call_freepn  : Routine to free program number
 *                : timtxt             : Print time & diagnostic
 *
 * Revision info  :
 *
 * $Revision: 2$
 * $Date: 21/09/2011 10:34:33$
 * $Source: /home/us0400/mdb/op/lib/RPC/client/RCS/msrpc_call_mdb.c,v $
 * 
 * Change history : 
 *
 * $Log:
 *  2    Met_DB_Project 1.1         21/09/2011 10:34:33    Sheila Needham
 *       Added time.h
 *  1    Met_DB_Project 1.0         09/11/2009 10:44:12    Sheila Needham
 *       Initial check-in of revised client software (previously under client
 *       directory)
 * $
 * Revision 2.0  2003/03/14 12:52:32  usmdb
 * Combined HP/T3E/SX6 version using pre-processor statements - S.Cox
 *
 * Revision 1.5  2000/09/05  11:06:25  11:06:25  usmdb (Generic MDB account)
 * server name and timeout variables changed. They are now set
 * earlier in a calling program - S.Cox
 * 
 * Revision 1.4  2000/06/16  11:44:24  11:44:24  usmdb (Generic MDB account)
 * Addition of code to allow binary RPC transfer of
 * data in crep_binary structure - S.Cox
 * 
 * Revision 1.3  99/07/28  12:14:54  12:14:54  usmdb (Generic MDB account)
 * Addition of debug output. Switched on by global variable debug_level in 
 * constants.h. Also correct code so if routine called with ISTAT=99, 
 * clnt_create is only done once - S.Cox.
 * 
 * Revision 1.2  1998/02/10 15:06:29  usmdb
 * Code added to write the main server program number to a temporary file 
 * specified in environment variable. If this routine is later called with 
 * ISTAT=99 to kill the server, and it is the first call to this routine, 
 * try to read the program number from the file specified in the 
 * environment variable and kill with this program number. - S.Cox
 *
 * Revision 1.1  1998/02/02 16:02:28  usmdb
 * Initial revision
 *
 * 23-12-1997   : Server killed by RPC call to killserverp_1 on server and
 *              : program number freed by RPC call to free_prognump_1 on
 *              : server - S.Cox 
 *
 * 04-12-1997   : Now called by mdb_rpc_main - S.Cox 
 *
 * 04-04-1997   : Based on ssrpc_call_mdb - S.Cox 
 *
 *---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*
 * include files
 *---------------------------------------------------------------------------*/

#include <stdio.h>           /* for standard C i/o */            
#include <stdlib.h>          /* for malloc */
#include <string.h>          /* for memcpy function */
#include <time.h>            /* standard time functions */
#include <sys/time.h>        /* to set our own TIMEOUT through clnt_control */ 
#include <rpc/rpc.h>         /* for all the RPC/XDR calls */
#include "msrpc_call_mdb.h"  /* local include for main RPC server */
#include "constants.h"       /* local include for MetDB RPC constants */
 
#if defined (T3E)
#include <fortran.h>                      /* for C/Fortran conversion on T3E */
#endif

#define ENV_VARNAME_TEMPFILE1 "METDB_TEMPFILE1"

/*---------------------------------------------------------------------------*
 * Subroutine name msrpc_call_mdb.
 * 
 * Receives arguments from the mdb_rpc_main. Variables are passed
 * by reference, so changes may be made to them, so the input arguments are
 * also the output arguments. The return argument from the subroutine is 
 * therefore of type void. See the below description of arguments for more
 * details.
 *---------------------------------------------------------------------------*/

#if defined (T3E)
  void msrpc_call_mdb(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,
                      CSTR,CREP,user_prognum,user_freepn_prognum,
		      client_contact,hostname,username)               /* ST3 */
#else
  void msrpc_call_mdb(CSUBT,CREQ,ARRAY,NOBS,NELEM,ISTAT,
                      CSTR,CREP,LCSUBT,LCREQ,LCSTR,LCREP,
                      user_prognum,user_freepn_prognum,
		      client_contact,hostname,username)               /* ST3 */
#endif /* T3E */
  
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

 
char *client_contact;     /* ip   string containing user e-mail address   ST3 */
char *hostname;           /* ip   string containing client machine name   ST3 */
char *username;           /* ip   string containing user id               ST3 */
int  user_prognum;        /* ip      RPC broker program number set by user  */
int  user_freepn_prognum; /* ip      RPC freepn program number set by user  */

{    

/*---------------------------------------------------------------------------*
 * Declare local variables 
 *
 * *cl            : cl is type pointer to structure CLIENT which is defined in 
 *                  the <rpc/rpc.h> standard include file. It is the "client
 *                  handle", and is unique for each client/server connection.
 *
 * *input_arg     : input_arg is type pointer to structure msrpc_ip_param 
 *                  which is defined in the "msrpc_call_mdb.h" local include 
 *                  file. This structure contains the ip arguments passed 
 *                  from the Fortran calling program. 
 *
 * *output_arg    : output_arg is type pointer to structure msrpc_op_param 
 *                  which is defined in the "msrpc_call_mdb.h" local include 
 *                  file. This structure contains the op arguments to be 
 *                  passed back to the Fortran calling program.
 *
 * *killsv_input  : input_arg is type pointer to structure 
 *                  msrpc_killsv_ip_param which is defined in the 
 *                  "msrpc_call_mdb.h" local include file. This structure 
 *                  contains the ip arguments to pass to the server to kill
 *                  the server. 
 *
 * timeOut        : timeOut is of type structure timeval which is defined in
 *                  the <sys/time.h> standard include file. This structure
 *                  is used to tell the RPC, how long to attempt the remote
 *                  call before timing-out.
 *
 * time_i         : system time (integer form) from function time. Used to
 *                : produce TimeStamp.
 *
 * TimeStamp      : character time stamp - this is passed to the server
 *                : for reference (matching client/server jobs)
 *
 * first          : first is type int, and is static. When call_mdb.c is first
 *                  called, first=1 to indicate that it is the first call to
 *                  the subroutine. On subsequent calls, first=0.
 *
 * SubCalls       : This is a counter to keep track of how many times, this
 *                  RPC client subroutine has been called by the users Fortran
 *                  calling program.
 *
 * ProgNum        : Program Number to connect with main RPC server. It is
 *                  returned by msrpc_call_broker.
 *
 * NoTICfile      : 0 if TIC file found, 1 if not.
 *            
 * freepn_status  : Returned from the call to free_prognum.
 *
 * *temp_csubt    : pointer to character. This will temporarily hold the 
 *                  subtype passed by the Fortran program.
 *
 * *temp_creq     : pointer to character. This will temporarily hold the 
 *                  request string passed by the Fortran program.
 *
 * *char_file     : temporary file to hold main program number. This
 *                  is read from the environment variable METDB_TEMPFILE1 set
 *                  by the user (OPS)
 *
 * *prognum_file  : pointer to FILE. Hold file details for char_file.
 *
 * text           : text line to read/write from/to char_file.
 *
 * sscanf_rc      : return code from sscanf function. 
 *
 * request_binary : will be set to 1 if user wishes to retrieve binary e.g.
 *                : BUFR or GRIB data from the MetDB.
 *
 * time_now       : current time.     
 * 
 *---------------------------------------------------------------------------*/

  CLIENT                 *cl;
  msrpc_ip_param         *input_arg;               
  msrpc_op_param         *output_arg;
  msrpc_killsv_ip_param  *killsv_input;               
  struct                 timeval timeOut;
  static                 long time_i;
  static                 char *TimeStamp;
  static                 int first=1;
  static                 int SubCalls=0;
  static                 int ProgNum;
  static                 int NoTICfile=0;
  int                    freepn_status;
  char                   *temp_csubt;
  char                   *temp_creq;
  char                   *char_file;
  FILE                   *prognum_file;
  char                   text[40];
  int                    sscanf_rc;
  int                    request_binary;                              /* 1.4 */
  time_t                 time_now;                                    /* 2.0 */
#if defined (T3E) || defined (L64)
    int                  iadd;                                        /* 1.3 */  
#endif  
    
/*---------------------------------------------------------------------------*
 * Check for NoTICfile set = 1. If so, return to calling program.
 *---------------------------------------------------------------------------*/
 
  if (debug_level > 0) {
    timtxt();
    (void)fprintf(stdout,"msrpc_call_mdb: start of routine\n");
    timtxt();
    (void)fprintf(stdout,"msrpc_call_mdb: client contact %s\n",client_contact); /* ST3 */
    timtxt();
    (void)fprintf(stdout,"msrpc_call_mdb: hostname %s\n",hostname);  /* ST3 */
    timtxt();
    (void)fprintf(stdout,"msrpc_call_mdb: username %s\n",username);  /* ST3 */
   
    
  }
    
  if (NoTICfile == 1) {
    *ISTAT=99;
    return;
  }

/*---------------------------------------------------------------------------*
 * allocate memory for the structure input_arg. The type cast (ip_param *) is
 * needed in UNIX C to change the character pointer. In ANSI C this is not
 * necessary, but does no harm so it is left in. Put the ip values passed by
 * the Fortran calling program into the structure input_arg.
 *---------------------------------------------------------------------------*/

  SubCalls++;

  input_arg    = (msrpc_ip_param *) malloc(sizeof(msrpc_ip_param));

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
 * look for "RETBUFR" or "RETGRIB" or "BINARY" in the CREQ request string. 
 * If found, user wants binary data to be returned in string CREP
 *---------------------------------------------------------------------------*/

/* 1.4 */
  request_binary = 0;
  if (strstr(input_arg->creq,"RETBUFR") != NULL ||
      strstr(input_arg->creq,"RETGRIB") != NULL ||
      strstr(input_arg->creq,"BINARY") != NULL) {
    request_binary = 1;
  }

  if (debug_level > 0) {
    timtxt();
    (void)fprintf(stdout,"msrpc_call_mdb: request_binary = %d\n", \
    request_binary);
  }

/*****************************************************************************
 *****************************************************************************
 **
 **  This is the first call to the subroutine from the user Fortran calling
 **  program. An RPC server therefore needs to be submitted on the mainframe.
 **  An RPC manager/broker (always running on the mainframe) will be called
 **  which submits an RPC main server job and returns the corrersponding RPC
 **  program number. first is set to zero so that this step will not be run
 **  again.
 **
 *****************************************************************************
 ****************************************************************************/

/*---------------------------------------------------------------------------*
 * Call system time routines time and ctime to get a TimeStamp which is passed
 * to the server for reference.
 *---------------------------------------------------------------------------*/
  
  if (first == 1) {

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: first time in routine\n");
    }

    first     = 0;
    time_now  = time(0);                                              /* 2.0 */
    time_i    = time_now;                                             /* 2.0 */
    TimeStamp = ctime(&time_now);                                     /* 2.0 */

    timtxt();
    (void)fprintf(stdout,"msrpc_call_mdb: Client timestamp = %d\n",time_i);
    timtxt();
    (void)fprintf(stdout,"msrpc_call_mdb: Client timestamp = %s\n",TimeStamp);

    input_arg->TimeStamp = time_i;
    input_arg->userid = username;                                    /* ST3 */
    input_arg->client_ip = hostname;                                 /* ST3 */ 
    input_arg->contact = client_contact;                             /* ST3 */
    
/*---------------------------------------------------------------------------*
 * If this is the 1st call to the broker and ISTAT = 99, then try to read the
 * main server program number from the temporary file held in environment
 * variable METDB_TEMPFILE1. This is set by the user (OPS).
 *---------------------------------------------------------------------------*/

    if (input_arg->istat == 99) {

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: istat=99, will read prognum from tempfile\n");
      }

      char_file = getenv(ENV_VARNAME_TEMPFILE1);

      if (char_file == NULL) {
        ErrorNoUserTempFile1();
      } else {
        prognum_file = fopen(char_file, "r");
        if (prognum_file == NULL) {
          ErrorTempFile1Open(char_file);
        } else {
          (void)fgets(text, sizeof(text), prognum_file);
          sscanf_rc = sscanf(text, "%d", &ProgNum);
          fclose(prognum_file);
        }
      }

    } else {  
          
/*---------------------------------------------------------------------------*
 * Call the RPC broker and get a program number back. If it is <= 0 there was
 * a problem, so return to the user's calling program with an ISTAT = 99 
 *---------------------------------------------------------------------------*/

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: about to call "\
         "msrpc_call_broker to get program number\n");
      }

      ProgNum = msrpc_call_broker(user_prognum, time_i);

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: back from msrpc_call_broker\n");
      }

      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: ProgNum from broker = %d\n",
            ProgNum);

      if (ProgNum <= 0) {
        if (ProgNum == 0) {
          ErrorFirstNoProgNum();
        }
        NoTICfile=1;
        *ISTAT=99;
        return;
      }

/*---------------------------------------------------------------------------*
 * Put ProgNum into temporary file (name held in environment variable
 * METDB_TEMPFILE1). There is a possibility that this will be read from if
 * this routine is called for the 1st time with ISTAT=99 (e.g. in the OPS)
 *---------------------------------------------------------------------------*/
 
      char_file = getenv(ENV_VARNAME_TEMPFILE1);

      if (char_file != NULL) {
        prognum_file = fopen(char_file, "w");
        if (prognum_file != NULL) {
          sprintf(text,"%d",ProgNum);
          (void)fputs(text,prognum_file);
          fclose(prognum_file);
        }
      }

/*---------------------------------------------------------------------------*
 * Now try and connect to the RPC main server with the program number. It
 * should be running. If not there is a serious problem
 *---------------------------------------------------------------------------*/

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: about to call clnt_create " \
                             "for main server\n");
      }

      cl = clnt_create(server_ipname, ProgNum,                        /* 1.5 */
                       MSRPC_CALL_MDBV, TRANSPORT);                   /* 1.5 */

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: back from clnt_create\n");
      }

      if (cl == NULL) {
        ErrorFirstMainConnect(ProgNum);
        *ISTAT=99;
        return;
      }

    }  /* end of input_arg->istat == 99 if/else block */

  }  /* end of first == 1 if block */

/*****************************************************************************
 *****************************************************************************
 **
 **  It is not the first call to the subroutine from the user Fortran calling
 **  program. An RPC main server should still be running on the mainframe
 **  (submitted first time into this subroutine) - This is determined by
 **  trying to connect with it by creating a client handle. If the RPC main
 **  server is still up and running, communication with it can continue. If
 **  it is no longer running, it must have shut-down (the RPC main server
 **  procedure has a timeout assigned to it. If a call has not been made to
 **  it for the set number of seconds, it will shut down). In this instance,
 **  there are 2 possible scenarios:
 **
 **  1) user requesting more data (ISTAT=4). In this case, we can't
 **     continue. Report an error and return to calling program.
 **
 **  2) user requesting new data (ISTAT=0). In this case, we can continue,
 **     but it means starting up a new RPC main server procedure by calling
 **     the RPC manager/broker again.
 **
 *****************************************************************************
 ****************************************************************************/

  else {   

/*---------------------------------------------------------------------------*
 * Try and call the RPC main server. It should be up and running as this is
 * not the first call.
 *---------------------------------------------------------------------------*/

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: call to routine = %d\n",SubCalls);
    }

    input_arg->TimeStamp = time_i;
    input_arg->userid = username;
    input_arg->client_ip = hostname;
    input_arg->contact = client_contact;

    if (*ISTAT != 99) {
  
      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: about to call clnt_create " \
                             "for main server\n");
      }

      cl = clnt_create(server_ipname, ProgNum, MSRPC_CALL_MDBV,      /* 1.5 */
                       TRANSPORT);

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: back from clnt_create\n");
      }

      if (cl == NULL) {

        if (debug_level > 0) {
          timtxt();
          (void)fprintf(stdout,"msrpc_call_mdb: CANNOT CREATE CLIENT HANDLE!\n");
        }

/*---------------------------------------------------------------------------*
 * scenario 1) ISTAT=4. User trying to get more data. Server has shut-down,
 *             so we can't continue. In this instance set ISTAT to 99 and
 *             return to the user Fortran calling program. 
 *---------------------------------------------------------------------------*/

        switch (*ISTAT) {
      
          case 4:

            ErrorNotFirstMainConnectI4(SubCalls,ProgNum);
            *ISTAT=99;
            break;
	  
/*---------------------------------------------------------------------------*
 * scenario 2) ISTAT=0. We can continue, but we need to start a new RPC main
 *             server, so call the RPC broker and get a program number back.
 *             If it is <= 0 there was a problem, so return to the user's
 *             calling program with ISTAT=99
 *---------------------------------------------------------------------------*/

          case 0:

            if (debug_level > 0) {
              timtxt();
              (void)fprintf(stdout,"msrpc_call_mdb: istat=0 start new main " \
                                   "server\n");
              timtxt();
              (void)fprintf(stdout,"msrpc_call_mdb: about to call " \
                                   "msrpc_call_broker to get program number\n");
            }

            ProgNum = msrpc_call_broker(user_prognum, time_i);

            if (debug_level > 0) {
              timtxt();
              (void)fprintf(stdout,"msrpc_call_mdb: back from msrpc_call_broker\n");
            }

            timtxt();
            (void)fprintf(stdout,"msrpc_call_mdb: ProgNum from broker = %d\n",
            ProgNum);
 
            if (ProgNum <= 0) {
              if (ProgNum == 0) {
                ErrorNotFirstNoProgNum();
              }
              *ISTAT=99;
            }
            else {
  
/*---------------------------------------------------------------------------*
 * Now try and connect to the RPC main server with the program number. It
 * should be running. If not there is a serious problem 
 *---------------------------------------------------------------------------*/

              if (debug_level > 0) {
                timtxt();
                (void)fprintf(stdout,"msrpc_call_mdb: about to call " \
                                     "clnt_create for main server\n");
              }
            
              cl = clnt_create(server_ipname, ProgNum,                /* 1.5 */ 
                               MSRPC_CALL_MDBV,TRANSPORT);            /* 1.5 */

              if (debug_level > 0) {
                timtxt();
                (void)fprintf(stdout,"msrpc_call_mdb: back from clnt_create\n");
              }

              if (cl == NULL) {
                ErrorNotFirstMainConnectI0(SubCalls,ProgNum);
                *ISTAT=99;
              }
            }
            break;

/*---------------------------------------------------------------------------*
 * ISTAT not 0 or 4. Any value not equal to 0 or 4 is invalid , so set 
 * ISTAT to 99. 
 *---------------------------------------------------------------------------*/
      
          default:
   
            *ISTAT=99;
            break;
        }             /* end of switch *ISTAT block */
      }             /* end of cl == NULL if block */
    }             /* end of *ISTAT != 99 if block */
  }             /* end of first == 0 if block */

/*****************************************************************************
 *****************************************************************************
 **
 ** Call the main RPC server if *ISTAT != 99
 **
 *****************************************************************************
 ****************************************************************************/

  if (input_arg->istat != 99) {

    input_arg->prognum = ProgNum;
    

/*---------------------------------------------------------------------------*
 * First, overide the default RPC TIMEOUT period. The RPC default TIMEOUT 
 * period is 25 seconds. This is not long enough for many MetDB calls. This 
 * TIMEOUT period is overridden with metdb_timeout using the clnt_control 
 * RPC function.
 *---------------------------------------------------------------------------*/

    timeOut.tv_sec  = metdb_timeout;                                  /* 1.5 */
    timeOut.tv_usec = 0;
   
    if (clnt_control(cl,CLSET_TIMEOUT,(char *)&timeOut) == FALSE) {   /* 2.0 */
       ErrorCantChangeTimeout(SubCalls);                              /* 1.5 */
    }

/*---------------------------------------------------------------------------*
 * CALL THE CLIENT C RPC STUB - calls the Remote MetDB!!
 * ==========================
 *
 * This function was produced using RPCGEN. The structure input_arg and the
 * structure cl (CLIENT handle) are passed to this function and the structure
 * output_arg is returned. Neatly close the socket that was opened by the
 * clnt_create call in this program.
 *---------------------------------------------------------------------------*/

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: about to call main server\n");
    }
    
    output_arg = msrpc_call_mdbp_1(input_arg,cl);

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: back from main server, call " \
                          "clnt_destroy\n");
    }

    clnt_destroy(cl);

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: back from clnt_destroy\n");
    }

/*---------------------------------------------------------------------------*
 * Check to see that the Remote call was successful. If the returned structure
 * output_arg is NULL, there was a problem calling the server. In this
 * instance, call an internal error reporting subprogram, set ISTAT to 99 and
 * return to the user Fortran calling program.
 *---------------------------------------------------------------------------*/

    if (output_arg == NULL) {
      ErrorMainCall(cl,SubCalls);
      *ISTAT=99;
      return;
    }
    
/*---------------------------------------------------------------------------*
 * Put arguments returned from the Remote procedure into the variables
 * to pass back to the Fortran calling routine. For variables CSTR, CREP and
 * ARRAY, we have to perform a "memory copy" to copy one block of address
 * space to another. The memcpy routine takes 3 arguments:
 *
 * arg1 : the destination area of memory (pointer to address space)
 * arg2 : the source area of memory (pointer to address space)
 * arg3 : the size of memory to copy (bytes)
 *---------------------------------------------------------------------------*/

    *ISTAT = output_arg->istat;
    *NOBS  = output_arg->nobs;

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: About to memcpy CSTR, CREP " \
                           "& ARRAY\n");
    }
    
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
#endif /* T3E */
    
/*---------------------------------------------------------------------------* 
 * CREP : copy the string in output_arg->crep of length input_arg->lcrep     * 
 * output_arg->nobs*sizeof(char) into CREP                                   *
 * 1.4 - Added request_binary                                                *
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
      memcpy(_fcdtocp(CREP),output_arg->crep,
      input_arg->lcrep*output_arg->nobs*sizeof(char));
#else
      memcpy(CREP,output_arg->crep,
      input_arg->lcrep*output_arg->nobs*sizeof(char));
#endif /* T3E */
    }
    
/*----------------------------------------------------------------------------* 
 * ARRAY : If T3E or L64, loop over the obs in output_arg->array.array_val
 * and put them in ARRAY. This is necessary as output_arg->array.array_val
 * is type float, but ARRAY is type double. If float to float copy required,
 * just perform a memcpy.             
 *----------------------------------------------------------------------------*/

#if defined (T3E) || defined (L64)
    iadd=0;                                                            /* 2.0 */
    while (iadd<output_arg->array.array_len) {                         /* 2.0 */
      ARRAY[iadd]=output_arg->array.array_val[iadd];                   /* 2.0 */
      iadd=iadd+1;                                                     /* 2.0 */
    }                                                                  /* 2.0 */ 
#else
    memcpy(ARRAY,output_arg->array.array_val,
    output_arg->array.array_len*sizeof(float));
#endif
    
    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: After memcpy CSTR, CREP & ARRAY\n");
    }

/*---------------------------------------------------------------------------*
 * free memory.
 *---------------------------------------------------------------------------*/

    free(temp_csubt);
    free(temp_creq);

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: About to call xdr_free\n");
    }

    xdr_free(xdr_msrpc_op_param, output_arg);

    if (debug_level > 0) {
      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: After xdr_free\n");
    }
  }

/*****************************************************************************
 *****************************************************************************
 **
 ** Kill the main RPC server if *ISTAT == 99 and ProgNum from broker > 0 (means
 ** a "main" server did get used), then free the program number.
 **
 *****************************************************************************
 ****************************************************************************/

  else {

    if (ProgNum > 0) {

/*---------------------------------------------------------------------------*
 * allocate memory for the structure killsv_input and put timestamp and the
 * "main" server program number into it.
 *---------------------------------------------------------------------------*/

      killsv_input = (msrpc_killsv_ip_param *) \
                      malloc(sizeof(msrpc_killsv_ip_param));

      killsv_input->TimeStamp = time_i;
      killsv_input->prognum   = ProgNum;

      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: Kill server %d\n",
                    killsv_input->prognum);

/*---------------------------------------------------------------------------*
 * Check that the main server is still running by trying to connect to it.
 * If it is still running, call the killserverp_1 to kill the server and
 * clnt_destroy to close the socket.
 *---------------------------------------------------------------------------*/

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: about to call clnt_create " \
                             "for main server\n");
      }

      cl = clnt_create(server_ipname, ProgNum,                       /* 1.5 */
                       MSRPC_CALL_MDBV, TRANSPORT);                  /* 1.5 */

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: after clnt_create\n");
      }

      if (cl != NULL) {

        if (debug_level > 0) {
          timtxt();
          (void)fprintf(stdout,"msrpc_call_mdb: about to call killserverp_1\n");
        }

        killserverp_1(killsv_input,cl);

        if (debug_level > 0) {
          timtxt();
          (void)fprintf(stdout,"msrpc_call_mdb: after killserverp_1, " \
                               "call clnt_destroy\n");
        }

        clnt_destroy(cl);

        if (debug_level > 0) {
          timtxt();
          (void)fprintf(stdout,"msrpc_call_mdb: after clnt_destroy\n");
        }
      }

/*---------------------------------------------------------------------------*
 * Call msrpc_call_freepn to free the program number on server.
 *---------------------------------------------------------------------------*/

      timtxt();
      (void)fprintf(stdout,"msrpc_call_mdb: Free program number %d\n",ProgNum);

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: about to call msrpc_call_freepn\n");
      }

      freepn_status = msrpc_call_freepn(user_freepn_prognum, ProgNum, time_i);

      if (debug_level > 0) {
        timtxt();
        (void)fprintf(stdout,"msrpc_call_mdb: after msrpc_call_freepn call\n");
      }

    }

    timtxt();
    (void)fprintf(stdout,"msrpc_call_mdb: RPC completed\n");
  }

  if (debug_level > 0) {
    timtxt();
    (void)fprintf(stdout,"msrpc_call_mdb: Return to calling program\n");
  }

} 
