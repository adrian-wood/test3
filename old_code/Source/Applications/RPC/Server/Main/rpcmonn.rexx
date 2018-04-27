/* REXX --------------------------------------------------------------
 *
 * program     : RPCMONN ( runs on BRPD !!!!!!!!!!!! )
 *
 * description : Started task to inquire the status of MetDB
 *             : multi-user RPC server MDBOC and MDBOD.
 *             : MDBOC and MDBOD are
 *             : interregated in turn every 30 seconds to see that
 *             : that they are still available for client requests.
 *             : If not, console commands are submitted to restart
 *             : the pair of servers.If a server is not
 *             : responding - a max of 10 retries (30 seconds apart)
 *             : is attempted before a restart is issued. This is in
 *             : case the server is genuinely busy actioning a client
 *             : request.
 *
 *             : The MDBOC server is interregated 1st. If this is not
 *             : Ok, both MDBOC and MDBOD are restarted. If MDBOC is
 *             : Ok, the MDBOD server is interregated. If this is
 *             : not Ok, both MDBOC and MDBOD are restarted. So if
 *             : either server is not responding to requests, they
 *             : will both be restarted.
 *
 * author      : Simon Cox
 *
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:36:14    Kudsia Gwangwaa 
 * $
 * Revision 1.5  2003/06/03 09:20:38  usmdb
 * Added check for monitoring job running on wrong system - S.Cox
 *
 * Revision 1.4  2002/07/12 14:32:16  usmdb
 * Check for 'ready and waiting' in all lines of SYSPRINT output - S.Cox
 *
 * Revision 1.3  2001/09/18 14:19:50  usmdb
 * Added allocate TCPIP library - S.Cox
 *
 * Revision 1.2  2001/09/18 09:52:34  usmdb
 * Added RO BRPD, to console commands - S.Cox
 *
 * Revision 1.1  2001/09/17 11:46:01  usmdb
 * Initial revision
 *
 * $Source: /home/us0400/mdb/op/lib/RPC/server/main/RCS/rpcmonn.rexx,v $
 * $Revision: 1$
 * $Date: 11/10/2006 11:36:14$
 *
 *--------------------------------------------------------------------*/

datim = date('O') time('N')
say datim 'MetDB RPC monitor job submitted'

/*--------------------------------------------------------------------*/
/* Check this job is running on the right system. If it is not, enter */
/* a "forever" loop, reporting the problem every 10 minutes to the    */
/* operators.                                                         */
/*--------------------------------------------------------------------*/

system = MVSVAR('sysname')
if system /= BPRD then do
  do forever
    say '- Job running on wrong system. Job should run on BPRD'
    say '  but is running on : 'system
    call CONSMSG "MDBON01 For info: MDBON running on wrong system"
    sleep 600
  end
end

/*--------------------------------------------------------------------*/
/* Allocate rpcinfo stdout and stderr output files.                   */
/* Initialise global variables.                                       */
/*--------------------------------------------------------------------*/

"alloc FI(SYSPRINT) NEW DELETE"
"alloc FI(SYSOUT) NEW DELETE"

broker_job      = MDBOC
freepn_job      = MDBOD
broker_prognum  = 33556338   /* broker RPC program number */
freepn_prognum  = 33556339   /* freepn RPC program number */
rpcinfo_retries = 10         /* rpcinfo retries before action taken */
rpcinfo_sleep   = 30         /* sleep (secs) between rpcinfo retries */
main_sleep      = 30         /* sleep (secs) between server checks */

/*--------------------------------------------------------------------*/
/* Loop forever. Check the status of the broker server. If            */
/* broker_okay = 1, server Okay - so go on to check the status of     */
/* the freepn server.                                                 */
/*--------------------------------------------------------------------*/

do forever

  broker_okay = 1
  freepn_okay = 1

  call check_server broker_job, broker_prognum
  broker_okay = RESULT

  if broker_okay = 1 then do
    call check_server freepn_job, freepn_prognum
    freepn_okay = RESULT
  end

/*--------------------------------------------------------------------*/
/* If either server not responding - issue WTO.                       */
/*--------------------------------------------------------------------*/

  select
  when broker_okay = 0 then do
    call output_server_failure broker_job,broker_job,freepn_job
    call CONSMSG "MDBON01 For info: MDBON is restarting MDBOC & MDBOD"
    call restart_servers broker_job,freepn_job
  end
  when freepn_okay = 0 then do
    call output_server_failure freepn_job,broker_job,freepn_job
    call CONSMSG "MDBON01 For info: MDBON is restarting MDBOC & MDBOD"
    call restart_servers broker_job,freepn_job
  end
  otherwise
    datim = date('O') time('N')
    say datim broker_job', 'freepn_job' servers OK - no action necessary'
  end

/*--------------------------------------------------------------------*/
/* sleep before returning to top of loop                              */
/*--------------------------------------------------------------------*/

  datim = date('O') time('N')
  say datim 'main sleep - sleeping 'main_sleep' seconds'

  "sleep "main_sleep""

end

"free FI(SYSPRINT)"
"free FI(SYSOUT)"

exit

/*====================================================================*/
/*                                                                    */
/* perform rpcinfo of program number                                  */
/*                                                                    */
/*====================================================================*/

prognum_status:

parse arg prognum

"rpcinfo -u bprd "prognum""

'execio * diskr SYSPRINT (stem info_sysprint. finis) '

'execio * diskr SYSOUT (stem info_sysout. finis) '

return

/*====================================================================*/
/*                                                                    */
/* restart servers                                                    */
/*                                                                    */
/*====================================================================*/

restart_servers:

parse arg server1_name, server2_name

say 'in restart_user_servers - consprof'
"consprof soldisp(no) unsoldisp(no)"
say 'in restart_user_servers - console activate'
"console activate"

say 'in restart_user_servers - C 'server1_name
address console "RO BPRD,C "server1_name""
say 'in restart_user_servers - C 'server2_name
address console "RO BPRD,C "server2_name""
say 'in restart_user_servers - sleep 20'
"sleep 20"
say 'in restart_user_servers - S 'server1_name
address console "RO BPRD,S "server1_name""
say 'in restart_user_servers - S 'server2_name
address console "RO BPRD,S "server2_name""

say 'in restart_user_servers - console deactivate'
"console deactivate"
say 'in restart_user_servers - return'

return

/*====================================================================*/
/*                                                                    */
/* output server failure                                              */
/*                                                                    */
/*====================================================================*/

output_server_failure:

parse arg server_down, server1_name, server2_name

datim = date('O') time('N')
say
say '-------------------------------------------------------------'
say datim 'RPCMON: 'server_down' not responding !!'
say
say info_sysprint.1
say info_sysout.1
say
say 'Operator action = Nil'
say
say 'MDBON will now restart 'server1_name' & 'server2_name
say '-------------------------------------------------------------'
say

return

/*====================================================================*/
/*                                                                    */
/* Check status of server                                             */
/*                                                                    */
/*====================================================================*/

check_server:

parse arg server_name, prognum

datim = date('O') time('N')
say datim 'checking 'server_name' server status'

do loop = 0 to rpcinfo_retries
  if loop > 0 then do
    datim = date('O') time('N')
    say datim 'server not responding - sleeping 'rpcinfo_sleep' seconds'
    "sleep "rpcinfo_sleep""
    datim = date('O') time('N')
    say datim 'try to connect again - retry 'loop' of 'rpcinfo_retries
  end
  call prognum_status prognum
  check_server_rc = 0
  do i = 1 to info_sysprint.0                                  /* 1.4 */
    if wordpos('ready and waiting',info_sysprint.i) /= 0 then do
      check_server_rc = 1
      datim = date('O') time('N')
      say datim server_name' server Okay'
    end
  end                                                          /* 1.4 */
  if check_server_rc = 1 then do                               /* 1.4 */
    leave                                                      /* 1.4 */
  end                                                          /* 1.4 */
end

return check_server_rc
