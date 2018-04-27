/* REXX -------------------------------------------------------------
 *
 * routine     : metdb_rpc_ops_timings.rexx
 *
 * purpose     : To produce statistics of OPS operational extraction
 *             : timings based on MetDB RPC broker/freepn log files.
 *
 * description : See comments in code.
 *
 * called by   : cronjob
 *
 * author      : Simon Cox MetDB Team 10 Nov 2000
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:35:25$
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/metdb_rpc_ops_timings.rexx,v $
 *
 * ------------------------------------------------------------------
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:25    Kudsia Gwangwaa 
 * $
 * Revision 1.4  2000/11/21  14:19:19  14:19:19  usmdb (Generic MDB account)
 * Ignore runs where there is only a start time - S.Cox
 * 
 * Revision 1.3  2000/11/15  11:44:29  11:44:29  usmdb (Generic MDB account)
 * Write stats to output - S.Cox
 * 
 * Revision 1.2  2000/11/15  10:50:35  10:50:35  usmdb (Generic MDB account)
 * Addition of links after table - S.Cox
 * 
 * Revision 1.1  2000/11/13  15:28:54  15:28:54  usmdb (Generic MDB account)
 * Initial revision
 * ------------------------------------------------------------------ */

WORKDIR='/u/os/t12db/public_html/rpc_stats'

/* ------------------------------------------------------------------
 * Open file for daily HTML output
 * ------------------------------------------------------------------ */

file1='output.html'
address syscall 'open' WORKDIR'/'file1 O_rdwr+O_creat+O_trunc 660
fd1 = retval
if fd1 = -1 then do
  say 'error opening output file 'WORKDIR'/'file1
  exit 0
end

/* ------------------------------------------------------------------
 * Open file for daily comma separated output
 * ------------------------------------------------------------------ */

file2='daily_data.txt'
address syscall 'open' WORKDIR'/'file2 O_rdwr+O_creat+O_trunc 660
fd2 = retval
if fd2 = -1 then do
  say 'error opening output file 'WORKDIR'/'file2
  exit 0
end

/* ------------------------------------------------------------------
 * HTML information
 * ------------------------------------------------------------------ */

rec='<html>' || esc_n
address syscall 'write' fd1 'rec'
rec='<head>' || esc_n
address syscall 'write' fd1 'rec'
rec='<title>' || esc_n
address syscall 'write' fd1 'rec'
rec='MetDB RPC - Operational Model extraction timings' || esc_n
address syscall 'write' fd1 'rec'
rec='</title>' || esc_n
address syscall 'write' fd1 'rec'
rec='</head>' || esc_n
address syscall 'write' fd1 'rec'

rec='<body bgcolor="#FFFFFF" link="#0000FF" vlink="#800080">' || esc_n
address syscall 'write' fd1 'rec'
rec='<center>' || esc_n
address syscall 'write' fd1 'rec'
rec='<h1>MetDB RPC - Operational Model extraction timings</h1>' || esc_n
address syscall 'write' fd1 'rec'
datim = date('E') time('N')
rec='run at 'datim 'to summarize most recent 24 hours data<br><br>' || esc_n
address syscall 'write' fd1 'rec'
rec='<table border=1>' || esc_n
address syscall 'write' fd1 'rec'
rec='<tr><th>Date</th><th>Start Time</th><th>Duration<br>(secs)</th><th>Accounting</th></tr>' || esc_n
address syscall 'write' fd1 'rec'

/* ------------------------------------------------------------------
 * read RPC broker/freepn filenames into stem file_name. The shcmd
 * finds only those files modified in the last 24 hours and only
 * those that contain the text 'OP1'
 * ------------------------------------------------------------------ */

pathname='/tmp/mdb/'

call shcmd 'find 'pathname' -mtime -1 -exec grep -l OP1 {} \;',,'file_name.'
if file_name.0 < 1 then
do
 say "no input files"
 exit 0
end

/* ------------------------------------------------------------------
 * read contents of file into stem file_contents.
 * ------------------------------------------------------------------ */

do i = 1 to file_name.0

  if pos('mdbrpc',file_name.i) > 0 then
  do
    address syscall "readfile (file_name.i) file_contents."

/* ------------------------------------------------------------------
 * Initialise variables.
 * ------------------------------------------------------------------ */

    broker_start.     = ' '
    broker_prognum.   = ' '
    broker_branch.    = ' '
    broker_userid.    = ' '
    broker_tic.       = ' '
    freepn_end.       = ' '
    freepn_prognum.   = ' '
    freepn_matched.   = 'F'

    broker_start.     = justify(broker_start.,17)
    broker_prognum.   = justify(broker_prognum.,6)
    broker_branch.    = justify(broker_branch.,5)
    broker_userid.    = justify(broker_userid.,5)
    broker_tic.       = justify(broker_tic.,5)
    freepn_prognum.   = justify(freepn_prognum.,6)
    freepn_end.       = justify(freepn_end.,17)

    bstore = 0
    fstore = 0

/* ------------------------------------------------------------------
 * Loop over lines in file, extracting info.
 * ------------------------------------------------------------------ */

    if file_contents.0 > 0 then do

      do j = 1 to file_contents.0

/* ------------------------------------------------------------------
 * time broker called. New client request for prognum.
 * ------------------------------------------------------------------ */

        select
        when pos('msrpc_call_brokerp_1: start of routine', ,
        file_contents.j) == 1 then
        do
          k=j+1
          call convert_date_time subword(file_contents.k,2)
          bstore = bstore + 1
          broker_start.bstore = RESULT
        end

/* ------------------------------------------------------------------
 * client branch, userid & tic info.
 * ------------------------------------------------------------------ */

        when pos('msrpc_call_brokerp_1: Branch', ,
        file_contents.j) == 1 then
        do
          broker_branch.bstore = subword(file_contents.j,4)
          broker_branch.bstore = justify(broker_branch.bstore,5)
        end

        when pos('msrpc_call_brokerp_1: UserId', ,
        file_contents.j) == 1 then
        do
          broker_userid.bstore = subword(file_contents.j,4)
          broker_userid.bstore = justify(broker_userid.bstore,5)
        end

        when pos('msrpc_call_brokerp_1: Tic', ,
        file_contents.j) == 1 then
        do
          broker_tic.bstore = subword(file_contents.j,4)
          broker_tic.bstore = justify(broker_tic.bstore,5)
        end

/* ------------------------------------------------------------------
 * prognum allocated by broker.
 * ------------------------------------------------------------------ */

        when pos('get_prognum: will use prognum =', ,
        file_contents.j) == 1 then
        do
          broker_prognum.bstore = subword(file_contents.j,6)
        end

/* ------------------------------------------------------------------
 * prognum to free by freepn.
 * ------------------------------------------------------------------ */

        when pos('free_prognum: prognum to free =', ,
        file_contents.j) == 1 then
        do
          fstore = fstore + 1
          freepn_prognum.fstore = subword(file_contents.j,6)
        end

/* ------------------------------------------------------------------
 * time freepn ended. Prognum now free for further use.
 * ------------------------------------------------------------------ */

        when pos('free_prognum: end of routine', ,
        file_contents.j) == 1 then
        do
          k=j+1
          call convert_date_time subword(file_contents.k,2)
          freepn_end.fstore = RESULT
        end

        otherwise
          nop
        end

      end     /* j */

/* ------------------------------------------------------------------
 * Loop over broker_prognum entries. Try to match a freepn_prognum 
 * entry. If match, output broker & freepn details. If no match, 
 * output broker details & blank freepn details
 * ------------------------------------------------------------------ */

      if bstore > 0 then do
        do j = 1 to bstore
          match = 0
          if fstore > 0 then do
            do k = 1 to fstore
              if broker_prognum.j == freepn_prognum.k then do
                match = k
                freepn_matched.k = 'T'
              end /* if broker_prognum.j == freepn_prognum.k */
            end /* k */
          end /* if fstore */
                   
/* ------------------------------------------------------------------
 * If match > 0 then a program number match has been found. Also
 * check that the details really are for a 'OP1' retrieval.
 * We have to do the OP1 check , because more than 1 set of output can
 * appear in a mdbrpc_ file, so the initial find & grep will not have
 * weeded out all of the non-OP1 data.
 * ------------------------------------------------------------------ */
 
          if match > 0 & pos('OP1',broker_UserId.j) > 0 then do            

/* ------------------------------------------------------------------
 * calculate time taken for extraction (in seconds)
 * If sec_start < sec_end - simple calculation.
 * if sec_end < sec_start, extraction soans midnight. Cakculation is
 * (sec_end - 0) + (midnight (86400 secs) - sec_start)
 *
 * NOTE - THE BELOW ASSUMES THAT NO RETRIEVALS TAKE 24 HOURS OR MORE
 * ------------------------------------------------------------------ */

            sec_start = 3600*substr(broker_start.j,1,2) + ,
                          60*substr(broker_start.j,4,2) + ,
                             substr(broker_start.j,7,2)
                      
            sec_end   = 3600*substr(freepn_end.match,1,2) + ,
                          60*substr(freepn_end.match,4,2) + ,
                             substr(freepn_end.match,7,2)

            select
            when (sec_start < sec_end) then do
              total_time = sec_end - sec_start                           
            end
            otherwise
              total_time = sec_end + (86400 - sec_start)                                      
            end

/* ------------------------------------------------------------------
 * Output data to various files.
 * ------------------------------------------------------------------ */

            select
            when total_time <= 180 then do
              rec='<tr><td align=center>'substr(broker_start.j,10,8)'</td>' ,
                  '<td align=center>'substr(broker_start.j,1,8)'</td>' ,
                  '<td align=center>'total_time'</td>' ,
                  '<td>'broker_branch.j broker_userid.j broker_tic.j'</td>' || esc_n
              address syscall 'write' fd1 'rec'
            end /* total_time<= 180 */
            otherwise
              rec='<tr><td align=center>'substr(broker_start.j,10,8)'</td>' ,
                  '<td align=center>'substr(broker_start.j,1,8)'</td>' ,
                  '<td align=center><font color=red>'total_time'</font></td>' ,
                  '<td>'broker_branch.j broker_userid.j broker_tic.j'</td>' ,
                  || esc_n
              address syscall 'write' fd1 'rec'
            end /* total_time<= 180 */
            
            rec=substr(broker_start.j,10,8)','substr(broker_start.j,1,8)','total_time || esc_n
            address syscall 'write' fd2 'rec'
            
          end /* if match > 0 && pos('OP1',broker_UserId.j) > 0 */
          
        end /* j */
      end /* if bstore */
    end /* if file_contents */
  end /* index(file_name.i,'mdbrpc') > 0 */
end /* i */

/* ------------------------------------------------------------------
 * HTML information
 * ------------------------------------------------------------------ */

rec='</table>' || esc_n
address syscall 'write' fd1 'rec'
rec='<br><a href="http://www-cf/~cfos/core_suite/schedule.html">Operational Schedule</a>' || esc_n 
address syscall 'write' fd1 'rec'
rec='<br><a href="http://ukmet:8013/infoweb/helpdesk.html">SPOUT</a>' || esc_n
address syscall 'write' fd1 'rec'
rec='<br><a href="http://ukmet/~t12db/rpc_stats/daily_data.txt">Daily data (CSV)</a>' || esc_n
address syscall 'write' fd1 'rec'
rec='<br><a href="http://ukmet/~t12db/rpc_stats/archive">Archive</a>' || esc_n
address syscall 'write' fd1 'rec'
rec='<br><hr>MetDB Team x6953/4/5 metdb@meto.gov.uk' || esc_n
address syscall 'write' fd1 'rec'
rec='</center>' || esc_n
address syscall 'write' fd1 'rec'
rec='</pre>' || esc_n
address syscall 'write' fd1 'rec'
rec='</body>' || esc_n
address syscall 'write' fd1 'rec'
rec='</html>' || esc_n
address syscall 'write' fd1 'rec'

/* ------------------------------------------------------------------
 * close daily output files
 * ------------------------------------------------------------------ */

address syscall 'close' fd1  
address syscall 'close' fd2

/* ------------------------------------------------------------------
 * Copy files to archived locations - obtain current dat in dd Mon YYYY
 * format
 * ------------------------------------------------------------------ */

datim = date()
day   = word(datim,1)
month = word(datim,2)
year  = word(datim,3)

/* ------------------------------------------------------------------
 * Create archive directory if necessary.
 * ------------------------------------------------------------------ */

call shcmd 'if [ ! -d 'WORKDIR'/archive/'year' ] ; 
            then ; 
            mkdir 'WORKDIR'/archive/'year' ; 
            fi ',,,

call shcmd 'if [ ! -d 'WORKDIR'/archive/'year'/'month' ] ; 
            then ; 
            mkdir 'WORKDIR'/archive/'year'/'month' ; 
            fi ',,,

/* ------------------------------------------------------------------
 * Copy daily html file to archive. Append daily comma-separated file
 * to monthly comma-separated file.
 * ------------------------------------------------------------------ */

file3='output_'date('S')'.html'
call shcmd 'cp 'WORKDIR'/'file1' 'WORKDIR'/archive/'year'/'month'/'file3'',,,

call shcmd 'more 'WORKDIR'/'file2' >> 'WORKDIR'/archive/'year'/'month'/monthly_data.txt',,,

EXIT 0

/* ==================================================================
 * function to convert mdbrpc_xxxxxxxx.txt date/time to a shorter 
 * format & return
 * ================================================================== */

convert_date_time:

parse arg input_date_time

temp_date = subword(input_date_time,3,1) || ' ' || ,
            subword(input_date_time,2,1) || ' ' || ,
            subword(input_date_time,5,1)

output_date_time = subword(input_date_time,4,1) || ' ' || ,
                   date('E',temp_date,'N')

return output_date_time
