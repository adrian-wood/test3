#!/bin/bash

# Source global environment vars...
. /var/moods/BADC/scripts/global_vars.sh

RUNTIME=`date "+%Y%m%d_%H%M%S"`

# Required environment variables
export METDB_SERVER_NUMBER=33556618
export METDB_FREEPN_NUMBER=33556619
export METDB_SERVER_IPNAME=mdbapop-prod
export METDB_CLIENT_CONTACT=metdb@metoffice.gov.uk

# Optional environment variables
export METDB_RPC_TIMEOUT=5400
export METDB_DEBUG_LEVEL=0

# Record state of Control File...
echo "--------------------------------------------------"
echo "${DTYPE} control file BEFORE execution of retclm :"
cat ${CNTL_DIR}/MDB.BADC.${DTYPE}.CONTROL
echo "--------------------------------------------------"

# Tidy up from previous run

if [ -L ${RUN_DIR}/FT22F001 ]; then
    rm ${RUN_DIR}/FT22F001
fi

if [ -L ${RUN_DIR}/FT10F001 ]; then
    rm ${RUN_DIR}/FT10F001
fi

# Create symlinks

ln -s ${CNTL_DIR}/MDB.BADC.${DTYPE}.CONTROL ${RUN_DIR}/FT22F001
ln -s ${OUT_DIR}/MDB.BADC.${DTYPE}.OUT ${RUN_DIR}/FT10F001

# Run the program
cd ${RUN_DIR}
${EXEC_DIR}/retclm.exe
RC=$?

# If nothing retrieved, i.e. RC=8, then this is OK if today is in the latter
# half of the month (day is >= 13)...
if [[ "$RC" -eq 8 && `date +%d` -ge 13 ]]; then
    echo "retclm.exe return code 8, but acceptable as late in the month"
    cylc message "${CYLC_SUITE_NAME}" "${CYLC_TASK_JOB}" "NO_DATA_BUT_LATE"
    RC=0
fi

# Record state of Control File again
echo "--------------------------------------------------"
echo "${DTYPE} control file AFTER execution of retclm :"
cat ${CNTL_DIR}/MDB.BADC.${DTYPE}.CONTROL
echo "--------------------------------------------------"

exit $RC
