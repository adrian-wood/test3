/*--------------------------------------------------------------------*
 *
 * Program      : rpclog
 *
 * Language     : C
 *
 * Description  : This logs client information on the server. Called by
 *              : MSRPC on the first entry.
 *              : Outputs to DIAG_PATH_PREFIX/rpclog_<clientTimestamp>
 *
 * Called by    : msrpc  (msrpc.c)
 *
 * Calls        : print_current_time   : print the current time
 *
 *
 * Arguments    : struct *ip_param (ip)   :
 *                FILE   * svout    (ip)
 *
 * $Revision: 1$
 * $Date: 09/11/2009 12:45:27$
 * $Source: $
 *
 * Changes      :
 *
 * $Log:
 *  1    Met_DB_Project 1.0         09/11/2009 12:45:27    Sheila Needham  RPC
 *       server upgrade
 * $
 *
 * 10-10-2009   : Written - S.Needham
 *--------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define DIAG_PATH_PREFIX "/tmp/mdb"

/*--------------------------------------------------------------------*
 * Function rpclog
 *--------------------------------------------------------------------*/

void rpclog(ip_param *input_arg,FILE *svout)

{


/*--------------------------------------------------------------------*
 * Declare variables.
 *--------------------------------------------------------------------*/

/* FILE *svout;           diagnostic o/p file struct */
  char                   text[120];     /* line to write to outfile   */

/*--------------------------------------------------------------------*
 * Output function input details.
 *--------------------------------------------------------------------*/

  print_current_time(svout);

  sprintf(text,"TimeStamp = %d\n",
  input_arg->TimeStamp);
  (void)fputs(text,svout);

  sprintf(text,"Prognum         = %d\n",
  input_arg->prognum);
  (void)fputs(text,svout);

  sprintf(text,"userid          = %s\n",
  input_arg->userid);
  (void)fputs(text,svout);

  sprintf(text,"client          = %s\n",
  input_arg->client_ip);
  (void)fputs(text,svout);

  sprintf(text,"contact         = %s\n",
  input_arg->contact);
  (void)fputs(text,svout);

  sprintf(text,"subtype         = %s\n",
  input_arg->csubt);
  (void)fputs(text,svout);

  sprintf(text,"nobs            = %d\n",
  input_arg->nobs);
  (void)fputs(text,svout);

  sprintf(text,"nelem           = %d\n",
  input_arg->nelem);
  (void)fputs(text,svout);

  sprintf(text,"istat (ip)= %d\n",
  input_arg->istat);
  (void)fputs(text,svout);

  (void)fputs(input_arg->creq,svout);

  sprintf(text,"\n");
  (void)fputs(text,svout);
/*--------------------------------------------------------------------*
 * Close diagnostics file
 *--------------------------------------------------------------------*/

  print_current_time(svout);


}
