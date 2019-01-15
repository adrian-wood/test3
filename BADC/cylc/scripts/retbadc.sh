#!/bin/bash

# Source global environment vars...
. /var/moods/BADC/scripts/global_vars.sh

RUNTIME=`date "+%Y%m%d_%H%M%S"`

# Determine type of run
if [ "$1" = "CATCHUP" ]; then
    RUNTYPE=CATCHUP
else
    RUNTYPE=NORMAL
fi

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
echo "${DTYPE} control file BEFORE execution of retbadc:"
cat ${CNTL_DIR}/MDB.BADC.${DTYPE}.CONTROL
echo "--------------------------------------------------"

# Tidy up from previous run

if [ -e ${RUN_DIR}/FT05F001 ]; then
    rm ${RUN_DIR}/FT05F001
fi

if [ -L ${RUN_DIR}/FT21F001 ]; then
    rm ${RUN_DIR}/FT21F001
fi

if [ -L ${RUN_DIR}/FT22F001 ]; then
    rm ${RUN_DIR}/FT22F001
fi

if [ -L ${RUN_DIR}/FT10F001 ]; then
    rm ${RUN_DIR}/FT10F001
fi

# Create symlinks

ln -s ${ELEM_DIR}/MDB.BADC.DATA.${DTYPE} ${RUN_DIR}/FT21F001
ln -s ${CNTL_DIR}/MDB.BADC.${DTYPE}.CONTROL ${RUN_DIR}/FT22F001
ln -s ${OUT_DIR}/MDB.BADC.${DTYPE}.OUT ${RUN_DIR}/FT10F001

# Create input file to program (NORMAL or CATCHUP)
echo ${RUNTYPE} > ${RUN_DIR}/FT05F001

# Run the program
cd ${RUN_DIR}
${EXEC_DIR}/retbadc.exe < ${RUN_DIR}/FT05F001 
RC=$?

# Record state of Control File again
echo "--------------------------------------------------"
echo "${DTYPE} control file AFTER execution of retbadc:"
cat ${CNTL_DIR}/MDB.BADC.${DTYPE}.CONTROL
echo "--------------------------------------------------"

exit $RC
