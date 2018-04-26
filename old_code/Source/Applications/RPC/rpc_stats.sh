#!/bin/sh

# set up locations

  WORKDIR=/u/os/t12db/rpc_stats
  FILE=rpc_monitor.html

#  write out headers for html file

  echo " "> $WORKDIR/$FILE
  echo " "> $WORKDIR/daily_data.txt
  echo "<HTML><body bgcolor=#FFFFF0>">$WORKDIR/$FILE
  echo "<META HTTP-EQUIV="expires" CONTENT="0">">>$WORKDIR/$FILE
  echo "<title>MetDB RPC Timings Monitor</title>">>$WORKDIR/$FILE
  echo "<body>">>$WORKDIR/$FILE
  echo "<h2><center> MetDB RPC Timings Monitor</center></h2>">>$WORKDIR/$FILE
  set `date`
  date_run=`echo $3`
  echo "<center>This Page updated :  " $1 $2 $3 $6 $4 $5 >>$WORKDIR/$FILE
  echo "<BR><BR>This page displays the total duration of operational RPC jobs for the previous day">>$WORKDIR/$FILE
  echo "</center>">>$WORKDIR/$FILE

  echo "<BR><BR>">>$WORKDIR/$FILE
  echo "<center><TABLE border=1>">>$WORKDIR/$FILE
  echo "<TR><TH>Start Time</TH><TH>End Time</TH><TH>Duration(s)</TH></TR>" >>$WORKDIR/$FILE

set `date '+%m %d %Y'`

# $1=month $2=date $3=year
  month=$1
  date=$2
  year=$3

#######################################################

  count=0

# loop through all previous days rpc output

  for i in `find /tmp/mdb -mtime 1`

  do

    file=`grep -l OP1 $i`

    if  [ $file ]
    then
      count=$count+1

# set variable types for algebra
#     typeset -i tot_sec2
#     typeset -i tot_sec1
#     typeset -i total_time

# extract start and end time.  Convert to seconds.
      line1=`head -3 $file | tail -1`
      start_time=`echo $line1 | awk '{print $5}'`
      line2=`tail -4 $i | head -1`
      end_time=`echo $line2 | awk '{print $5}'`

      hour1=`echo $start_time| awk -F: '{print $1}'`
      min1=`echo $start_time| awk -F: '{print $2}'`
      sec1=`echo $start_time| awk -F: '{print $3}'`

#     tot_sec1=$hour1*3600+$min1*60+$sec1

      hour2=`echo $end_time| awk -F: '{print $1}'`
      min2=`echo $end_time| awk -F: '{print $2}'`
      sec2=`echo $end_time| awk -F: '{print $3}'`

#     tot_sec2=$hour2*3600+$min2*60+$sec2

#     total_time=$tot_sec2-$tot_sec1

      echo "<TR>">>$WORKDIR/$FILE
      echo "<TD>$start_time</TD><TD>$end_time</TD>">>$WORKDIR/$FILE

#     if [ $total_time -le 180 ]
#     then
#       echo "<TD>$total_time</TD>">>$WORKDIR/$FILE
#     else
#       echo "<TD><font color=red>$total_time<br></font></TD>">>$WORKDIR/$FILE
#     fi

      echo "</TR>">>$WORKDIR/$FILE
      echo "$year,$month,$date,$start_time,$end_time,$total_time" >>$WORKDIR/daily_data.txt
    fi

  done


# write html footers

  echo "</TABLE>">>$WORKDIR/$FILE
  echo "<BR><BR><A HREF="http://www-cf/~cfos/core_suite/schedule.html">Operational Schedule</A>">>$WORKDIR/$FILE
  echo "<BR><BR><A HREF="http://ukmet:8013/infoweb/helpdesk.html">SPOUT</A>">>$WORKDIR/$FILE
  echo "<BR><BR><A HREF="http://$WORKDIR/daily_data.txt">Daily data (CSV)</A>">>$WORKDIR/$FILE
  echo "<BR><BR><A HREF="http://$WORKDIR/archive">Archive</A>">>$WORKDIR/$FILE

  echo "</center></body></HTML>">> $WORKDIR/$FILE

############################################################

# copy html file to archived location

# check to see if directory exists
  if [ ! -d $WORKDIR/archive/$year ]
  then
    mkdir $WORKDIR/archive/$year
  fi

  if [ ! -d $WORKDIR/archive/$year/$month ]
  then
    mkdir $WORKDIR/archive/$year/$month
  fi

  cp $WORKDIR/$FILE $WORKDIR/archive/$year/$month/mdbrpc_$year$month$date.html
  more $WORKDIR/daily_data.txt >> $WORKDIR/archive/$year/$month/monthly_data.txt
exit
