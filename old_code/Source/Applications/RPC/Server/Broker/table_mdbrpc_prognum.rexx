/* REXX -------------------------------------------------------------
 *
 * routine     : table_mdbrpc_prognum.rexx
 *
 * purpose     : to produce a summary table of details in the MetDB
 *             : RPC broker/freepn output datasets.
 *
 * description : CGI script, called by html form. This script 1st
 *             : parses the query_string passed to it to get the
 *             : variable files_to_read. It then reads the directory
 *             : /tmp/mdb/ into an array. It then loops over the
 *             : most recent 'files_to_read' files, extracting
 *             : information from them and summarizing this in a
 *             : table. See program comments for more detailed
 *             : description.
 *
 * called by   : /home/us0400/mdb/scripts/mdb_rpc_tools.html
 *
 * arguments   : files_to_read : max number of files to read
 *
 * author      : Simon Cox MetDB Team 21 Feb 2000
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:35:30$
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/table_mdbrpc_prognum.rexx,v $
 *
 * ------------------------------------------------------------------
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:30    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2000/02/24  09:56:41  09:56:41  usmdb (Generic MDB account)
 * Addition of column descriptions and a REXX trace facility if
 * files_to_read <= 50 - S.Cox
 * 
 * Revision 1.1  2000/02/22  09:46:05  09:46:05  usmdb (Generic MDB account)
 * Initial revision
 *
 * ------------------------------------------------------------------ */

/* ------------------------------------------------------------------
 * Get the variable query_string. Translate funny characters.
 * ------------------------------------------------------------------ */

do i = 1 to __environment.0
  if substr(__environment.i,1,13)='QUERY_STRING=' then
  do
    tablein = '%28 %29 %42 %25 %2F %3B %2C %3D %09'
    tableout = '( ) * % / : , =  '
    query_string=Transtr(substr(__environment.i,14),tableout,tablein)
    leave
  end
end

/* ------------------------------------------------------------------
 * Parse the variable query_string, extracting number of files to
 * read. If files_to_read <=50, turn on REXX tracing
 * ------------------------------------------------------------------ */

parse upper var query_string 1 . 'FILES_TO_READ=' files_to_read '&' .

if files_to_read <= 50 then do
  trace r
end

/* ------------------------------------------------------------------
 * HTML information
 * ------------------------------------------------------------------ */

say 'Content-Type: text/html'
say

say '<html>'
say '<head>'
say '<title>'
say 'MetDB RPC - summary table of broker/freepn output files'
say '</title>'
say '<META HTTP-EQUIV="expires" CONTENT="0">'
say '</head>'

say '<body bgcolor="#FFFFFF" link="#0000FF" vlink="#800080">'
say '<pre>'
say 'MetDB RPC - summary table of broker/freepn output files'
say
say 'files_to_read = 'files_to_read

/* ------------------------------------------------------------------
 * read filenames into stem file_name.
 * ------------------------------------------------------------------ */

pathname='/tmp/mdb/'

address syscall "readdir "pathname" file_name."
say 'number of files in 'pathname' = 'file_name.0
say

if file_name.0 < 1 then
do
 say 'Error listing files'
 exit 0
end

file_start = 1
if file_name.0 > files_to_read then do
  file_start = file_name.0 - files_to_read + 1
end

/* ------------------------------------------------------------------
 * output column headings
 * ------------------------------------------------------------------ */

say
say '1 : broker RPC program number'
say '2 : broker RPC program number forced = "F"'
say '3 : RPC start time/date (broker call starts)'
say '4 : RPC end time/date (freepn call ends)'
say '5 : freepn RPC program number'
say '6 : user supplied info, (Branch, UserID, TIC)'
say '7 : diagnostics file (includes client timestamp in name)'
say '8 : number of lines in disgnostics file'
say

say '1..... 2 3................ 4................ 5.....' ,
    '6................ 7.............................8... '
say

/* ------------------------------------------------------------------
 * read contents of file into stem file_contents.
 * ------------------------------------------------------------------ */

do i = file_start to file_name.0

  if index(file_name.i,'mdbrpc') > 0 then
  do
    file_to_read = pathname||file_name.i
    address syscall "readfile (file_to_read) file_contents."

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
    force_prognum.    = ' '
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
        when index(file_contents.j, ,
        'msrpc_call_brokerp_1: start of routine') == 1 then
        do
          k=j+1
          call convert_date_time subword(file_contents.k,2)
          bstore = bstore + 1
          broker_start.bstore = RESULT
        end

/* ------------------------------------------------------------------
 * client branch, userid & tic info.
 * ------------------------------------------------------------------ */

        when index(file_contents.j, ,
        'msrpc_call_brokerp_1: Branch') == 1 then
        do
          broker_branch.bstore = subword(file_contents.j,4)
          broker_branch.bstore = justify(broker_branch.bstore,5)
        end

        when index(file_contents.j, ,
        'msrpc_call_brokerp_1: UserId') == 1 then
        do
          broker_userid.bstore = subword(file_contents.j,4)
          broker_userid.bstore = justify(broker_userid.bstore,5)
        end

        when index(file_contents.j, ,
        'msrpc_call_brokerp_1: Tic') == 1 then
        do
          broker_tic.bstore = subword(file_contents.j,4)
          broker_tic.bstore = justify(broker_tic.bstore,5)
        end

/* ------------------------------------------------------------------
 * prognum allocated by broker.
 * ------------------------------------------------------------------ */

        when index(file_contents.j, ,
        'get_prognum: will use prognum =') == 1 then
        do
          broker_prognum.bstore = subword(file_contents.j,6)
        end

/* ------------------------------------------------------------------
 * prognum forced?
 * ------------------------------------------------------------------ */

        when index(file_contents.j, ,
        'force_free_prognum: max_diff_time_pn') == 1 then
        do
          force_prognum.bstore = 'F'
          broker_prognum.bstore = subword(file_contents.j,4)
          broker_prognum.bstore = justify(broker_prognum.bstore,6)
        end

/* ------------------------------------------------------------------
 * prognum to free by freepn.
 * ------------------------------------------------------------------ */

        when index(file_contents.j, ,
        'free_prognum: prognum to free =') == 1 then
        do
          fstore = fstore + 1
          freepn_prognum.fstore = subword(file_contents.j,6)
        end

/* ------------------------------------------------------------------
 * time freepn ended. Prognum now free for further use.
 * ------------------------------------------------------------------ */

        when index(file_contents.j, ,
        'free_prognum: end of routine') == 1 then
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
 * output details. Loop over broker_prognum entries. Try to match
 * a freepn_prognum entry. If match, output broker & freepn details.
 * If no match, output broker details & blank freepn details
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
          say broker_prognum.j force_prognum.j broker_start.j ,
              freepn_end.match freepn_prognum.match ,
              broker_branch.j broker_userid.j broker_tic.j ,
              file_to_read '('file_contents.0')'
        end /* j */
      end /* if bstore */

/* ------------------------------------------------------------------
 * Now loop over freepn_prognum entries. If not already matched
 * above, output the details.
 * ------------------------------------------------------------------ */

      if fstore > 0 then do
        do k = 1 to fstore
          if freepn_matched.k == 'F' then do
            say broker_prognum.0 force_prognum.0 broker_start.0 ,
                freepn_end.k freepn_prognum.k ,
                broker_branch.0 broker_userid.0 broker_tic.0 ,
                file_to_read '('file_contents.0')'
          end /* freepn_match.k == 'F' */
        end /* k */
      end /* fstore > 0 */

    end /* file_contents.0 > 0 */
    else say broker_prognum.0 force_prognum.0 broker_start.0 ,
             freepn_end.0 freepn_prognum.0 ,
             broker_branch.0 broker_userid.0 broker_tic.0 ,
             file_to_read '('file_contents.0')'

  end /* index(file_name.i,'mdbrpc') > 0 */
end /* i */

say '</pre>'
say '</body>'
say '</html>'

EXIT 0

/* ------------------------------------------------------------------
 * convert mdbrpc_xxxxxxxx.txt date/time to a shorter format & return
 * ------------------------------------------------------------------ */

convert_date_time:

parse arg input_date_time

temp_date = subword(input_date_time,3,1) || ' ' || ,
            subword(input_date_time,2,1) || ' ' || ,
            subword(input_date_time,5,1)

output_date_time = subword(input_date_time,4,1) || ' ' || ,
                   date('E',temp_date,'N')

return output_date_time
