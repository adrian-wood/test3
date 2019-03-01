#!/bin/bash
#-----------------------------------------------------------------------
#
# SCRIPT        : run_all_servicehub.sh
#
# PURPOSE       : Starts all servicehub suites in the /var/moods/cylc-run   
#                 directory.
#
#------------------------------------------------------------------------
# MB-1810: New                                             Sheila Needham
#------------------------------------------------------------------------

CYLC_RUN=/var/moods/cylc-run

suites=$(find $CYLC_RUN/servicehub -mindepth 1 -maxdepth 1 -type d)
for suite in $suites; do
    echo "Starting $suite..."
    cylc run $suite > /dev/null 2>&1
done
cylc scan
