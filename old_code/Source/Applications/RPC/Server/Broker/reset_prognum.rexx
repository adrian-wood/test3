/* REXX -------------------------------------------------------------
 *
 * routine     : reset_prognum.rexx
 *
 * purpose     : To reset the status flag in RPC program number files
 *             : if they have not been used for 24 hours or more.
 *
 * description : See comments in code.
 *
 * called by   : SYS1.OPSPROD.JOBLIB(MDBRPCAN)
 *
 * author      : Sheila Needham MetDB 2 March 2004
 *
 * $Revision: 1$
 * $Date: 11/10/2006 11:35:30$
 * $Source: /home/us0400/mdb/op/lib/RPC/server/broker/RCS/reset_prognum.rexx,v $
 *
 * ------------------------------------------------------------------
 * $Log:
 *  1    Met_DB_Project 1.0         11/10/2006 11:35:30    Kudsia Gwangwaa 
 * $
 * Revision 1.2  2005/08/02 11:04:59  usmdb
 * 1.2 18 July 2005. Sheila Needham
 * Increase the number of server programs from 50 to 80.
 *
 * Revision 1.1  2004/03/04 09:09:08  usmdb
 * Initial revision
 *
                                                                 */
indir = '/usr/local/mdb/bin'
say ' working dir is:'indir
address syscall "readdir "indir" filelist."  /* ls output in filelist */

/*  get current date and time  */
cdate = date('B')
ctime = time('H')

/* loop over files in this directory    */

say filelist.0 'files in ' indir
do i = 1 to filelist.0

/* only process files with "prognum" in their name */
  if index(filelist.i,'prognum') > 0 then
    do
/* ...and only pick the general user ones   */
/*  1.2 extend the range from 200070 to 200100  */
      prog = substr(filelist.i,9,6)
      if (prog >= 200021) & (prog <= 200100) then
        do
          say ' checking 'filelist.i prog
          infile=indir'/'filelist.i
          address syscall "readfile (infile) ln."
          if ln.2 = 1 then               /* status flag  */
            do
/* get date and time last used and convert to BASE day and hour */
          edate=right(ln.3,4,'0')||right(ln.4,2,'0')||right(ln.5,2,'0')
            bdate = DATE('B',edate,'S')
            btime=right(ln.6,2.'0')

/* returns RESULT 0 or 8            */
            CALL CHEKDATE cdate ctime bdate btime

            if (RESULT) = 8 then
              do
                say 're-setting 'ln.1' to zero. Last used 'ln.3,
                      ln.4 ln.5 ln.6 ln.7
                ln.2 = 0
                address syscall "writefile (infile) 777 ln."
              end
            end
        end
    end
end
exit 0
/*                     */
CHEKDATE:

arg nowdate nowtime usedate usetime
daydiff = nowdate - usedate
select
  when daydiff = 0 then
    result = 0          /* same day */
  when daydiff = 1 then
    do                                 /* yesterday  */
      if (24 - usetime + nowtime) >= 24 then
        result = 8
      else
        result = 0
    end
  when daydiff > 1 then
    result = 8                  /* more than 1 day old */
  otherwise
    result = 0
  end


RETURN result
