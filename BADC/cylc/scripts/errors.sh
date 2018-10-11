#!/bin/bash

# Source global environment vars...
. /var/moods/BADC/scripts/global_vars.sh

# Run this script if there are any errors...
RUNTIME=`date "+%Y%m%d_%H%M%S"`
cat << eof > ${OUT_DIR}/errors_${DTYPE}_${RUNTIME}.out
*** There are Failed Tasks in ${CYLC_SUITE_NAME} run at cycle point ${CYLC_TASK_CYCLE_POINT}; please investigate ***

http://www-mdb-apps/rose-bush/suites?user=moodsf

http://www-mdb-apps/rose-bush/taskjobs/moodsf?&suite=${CYLC_SUITE_NAME}&cycles=${CYLC_TASK_CYCLE_POINT}

eof

mailx -s "Failed Tasks in cylc suite ${CYLC_SUITE_NAME} ${CYLC_TASK_CYCLE_POINT}" andrew.moorhouse@metoffice.gov.uk < ${OUT_DIR}/errors_${DTYPE}_${RUNTIME}.out

echo "Errors"

