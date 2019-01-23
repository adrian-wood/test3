#!/bin/bash
# ----------------------------------------------------------------------------
# Function:
# A simple script to raise a Nagios warning if cylc suites are not working 
# as expected.
#
# For each suite listed by "cylc print":
#   Add the name of the suite to the "running_suites" file
#   Do a "cylc ping" for the suite to see if it is running
#   Do a "cycl suite-state" to obtain the number of failed tasks
# Assemble a message to send to Nagios
# Report status to Nagios with the passive check script.
#
# Execution:
# Place this script in ~/bin and execute it with cron, every 10 mins is OK.
# NB this script relies on the presence of "nagios_vars.sh" to get values for
# the Nagios passive check script.
#
# Revision:
# MB-1908  Write out list of currently-running suites so they can be restarted
#          after a reboot.                                      Andy Moorhouse
# MB-1832  Amended to check for failed tasks also               Andy Moorhouse
# MB-576   Initial Version                                      Andy Moorhouse
# ----------------------------------------------------------------------------

PATH=/data/local/fcm/bin:$PATH

# Load/define global Nagios variables...
. /home/moodsf/bin/nagios_vars.sh
host=mdb-apps
checkname=cylc_status
rc=0

# Local vars...
nag_nr_msg="Suites Not Running:"
nag_f_msg="Suites with Failed Tasks: "
nag_nr_flag=0
nag_f_flag=0
running_suites=/var/moods/.running_suites_list

echo "# List of currently-running cylc suites" > $running_suites
echo "# Automatically generated by $0 at `date "+%Y/%m/%d %H:%M:%S"`" >> $running_suites
echo "# DO NOT edit this file" >> $running_suites

echo "`date "+%Y/%m/%d %H:%M:%S"`: $0 running... "

for SUITE in `cylc print -xy`; do
  echo "$SUITE" >> $running_suites
  echo "Checking suite $SUITE ... "
  cylc ping $SUITE 2> /dev/null
  exit_code=$?
  if [ $exit_code -eq 0 ]; then
    echo "> it is running"
  else
    echo "> it is NOT running"
    nag_nr_msg="$nag_nr_msg $SUITE"
    nag_nr_flag=1
    rc=1
  fi
  FAILED=`cylc suite-state $SUITE -S failed 2>/dev/null|wc -l`
  echo "> it has $FAILED failed tasks"
  if [ $FAILED -gt 0 ]; then
    nag_f_msg="$nag_f_msg$SUITE ($FAILED) "
    nag_f_flag=1
    rc=1
  fi
done

# Assemble message to send to Nagios...
if [ $rc -eq 0 ]; then
  nagios_msg="OK: All suites appear to be running correctly"
else
  nagios_msg="ERROR: "
  if [ $nag_nr_flag -eq 1 ]; then
    nagios_msg="$nagios_msg$nag_nr_msg;"
  fi
  if [ $nag_f_flag -eq 1 ]; then
    nagios_msg="$nagios_msg $nag_f_msg"
  fi
fi
echo "Sending following message to Nagios: $nagios_msg"

# Raise Nagios passive check NB must have sourced nagios_vars.sh
echo -e "$host\t$checkname\t$rc\t$nagios_msg" | $NAG_SCRIPT -u $NAG_URL -t $NAG_TOKEN

echo "`date "+%Y/%m/%d %H:%M:%S"`: $0 finished."
exit $rc
