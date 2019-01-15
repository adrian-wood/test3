#!/bin/bash

# Source global environment vars...
. /var/moods/BADC/scripts/global_vars.sh

RUNTIME=`date "+%Y%m.%d.%H%M.txt"`
RC=0
LC=`echo ${DTYPE} | tr '[:upper:]' '[:lower:]'`

# Rename output file to timestamped name...
#mv ${OUT_DIR}/MDB.BADC.${DTYPE}.OUT ${OUT_DIR}/ukmo-metdb_${LC}_${RUNTIME}
mv ${OUT_DIR}/MDB.BADC.${DTYPE}.OUT ${OUT_DIR}/ukmo-metdb_${DTYPE}_${RUNTIME}

# FTP all *.txt files to dart...
# (not doing this yet)

# While parallel running:
# Get the file from the current z/OS run...
sleep 120
ftp ukmet << EOF
get 'MDB.BADC.${DTYPE}.OUT' ${OUT_DIR}/zos_${DTYPE}_${RUNTIME}
EOF

RC=$?

# Compare the output...
#WCL=`wc -c ${OUT_DIR}/ukmo-metdb_${LC}_${RUNTIME}|cut -f1 -d' '`
#WCL=`wc -c ${OUT_DIR}/ukmo-metdb_${DTYPE}_${RUNTIME}|cut -f1 -d' '`
#WCZ=`wc -c ${OUT_DIR}/zos_${DTYPE}_${RUNTIME}|cut -f1 -d' '`
#if [ $WCL -eq $WCZ ]; then
#    echo "Linux and z/OS Files are of same size, OK"
#else
#    echo "Linux and z/OS Files are not of same size, please investigate."
#    echo "--------------------------------------------------------------"
#    echo "Linux file contains ${WCL} observations"
#    echo "z/OS  file contains ${WCZ} observations"
#    RC=1
#fi

exit $RC
