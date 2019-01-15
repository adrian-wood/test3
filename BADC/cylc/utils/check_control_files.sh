#!/bin/bash -l
module load scitools
module display scitools

#DATATYPES=METARS
DATATYPES="AMDARS LNDSYN METARS OZONEPRF PILOT RASS SHPSYN TEMP WINPRO"

# Call the utility script to create a set of control files as they should be currently...
python /var/moods/BADC/utils/recreate_control_files.py > /dev/null

# Obtain the path of the latest generated control files from above...
GCFPATH=`ls -td1 /tmp/BADC_generated_control_files_*|head -1`

# Now check the current control file with the generated one...
for DATATYPE in $DATATYPES; do
    echo "--------------------------------------------------------------------------------"
    echo -e "Checking CURRENT control file (LHS) for ${DATATYPE} with GENERATED (RHS):"
    # NB only check top 4 lines
    sdiff <(head -4 /var/moods/BADC/control_files/MDB.BADC.${DATATYPE}.CONTROL) <(head -4 ${GCFPATH}/MDB.BADC.${DATATYPE}.CONTROL)
    if [ $? -eq 0 ]; then
        echo -e "   current control file for ${DATATYPE} matches generated, OK"
    else
        echo -e "   ERROR: current control file for ${DATATYPE} DOES NOT MATCH generated"
        RC=1
    fi
done
if [[ "$RC" -eq 1 ]]; then
    echo "--------------------------------------------------------------------------------"
    echo "----------- ERRORS COMPARING CONTROL FILES, PLEASE INVESTIGATE -----------------"
    echo "--------------------------------------------------------------------------------"
fi
exit $RC
