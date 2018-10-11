#!/bin/bash

# Source global environment vars...
. /var/moods/BADC/scripts/global_vars.sh

echo "--------------------------------------------------"
echo "${DTYPE} control file before execution of cntlchk:"
cat ${CNTL_DIR}/MDB.BADC.${DTYPE}.CONTROL
echo "--------------------------------------------------"

if [ -L ${RUN_DIR}/FT22F001 ]; then
    rm ${RUN_DIR}/FT22F001
fi

ln -s ${CNTL_DIR}/MDB.BADC.${DTYPE}.CONTROL ${RUN_DIR}/FT22F001

# Create input file to program - NB if a SENDONLY suite is required
# echo SENDONLY to FT05F001, otherwise a blank line
cat << eof > ${RUN_DIR}/FT05F001

eof

cd ${RUN_DIR}
${EXEC_DIR}/cntlchk.exe < ${RUN_DIR}/FT05F001
RC=$?

echo "--------------------------------------------------"
echo "${DTYPE} control file AFTER execution of cntlchk:"
cat ${CNTL_DIR}/MDB.BADC.${DTYPE}.CONTROL
echo "--------------------------------------------------"

if [ "$RC" -eq 10 ]; then
    echo "cntlchk.exe return code 10 (SENDONLY)"
    cylc message "${CYLC_SUITE_NAME}" "${CYLC_TASK_JOB}" "SENDONLY"
    RC=0
elif [ "$RC" -eq 0 ]; then
    echo "cntlchk.exe return code 0 so clear output file"
    > ${OUT_DIR}/MDB.BADC.${DTYPE}.OUT
fi
    
exit $RC
