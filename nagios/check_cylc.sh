#!/bin/bash
# ----------------------------------------------------------------------------
# Function:
# A simple script to raise a Nagios warning if cylc suites are not running.
# Obtains a list of all suites for the current user with "cylc print" and does
# "cylc ping" for each suite. Non-zero return code from cylc ping indicates
# a problem. Report status to Nagios.
#
# Execution:
# Place this script in ~/bin and execute it with cron, hourly should be OK.
#
# Revision:
# MB-576   Initial Version                                      Andy Moorhouse
# ----------------------------------------------------------------------------

PATH=/data/local/fcm/bin:$PATH
host=mdb-apps
checkname=cylc_status
rc=0
nagios_msg="Suites with Errors: "

for SUITE in `cylc print -xy`; do
  echo -ne "Checking $SUITE... "
  cylc ping $SUITE
  exit_code=$?
  if [ $exit_code -eq 0 ]; then
    echo "OK"
  else
    nagios_msg="$nagios_msg$SUITE "
    rc=$exit_code
  fi
done

if [ $rc -eq 0 ]; then
  nagios_msg="All suites OK"
fi

# Raise Nagios passive check
echo -e "$host\t$checkname\t$rc\t$nagios_msg" | /usr/local/nrdp/clients/send_nrdp.sh -u https://exvnagxiprd01/nrdp/ -t "mdb-token-8*Gn3.TKnm{3Cr"

exit $rc
