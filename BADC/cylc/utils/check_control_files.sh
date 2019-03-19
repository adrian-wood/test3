#!/bin/bash -l
# ----------------------------------------------------------------------------
# Function:
# A script to check that the BADC Control Files are as they should be.
#
# Calls the Python utility recreate_control_files.py to create a new set of
# files then does a diff of them to make sure the current set match the 
# generated. If not then exit with non-zero RC which will result in an email
# if the cron job is as follows:

# 00 09 * * * LOG=/tmp/BADC_Control_File_Check_`date "+\%Y\%m\%d"`.log; /var/moods/BADC/utils/check_control_files.sh > $LOG 2>&1 || mailx -s "Errors checking BADC Control Files" metdb@metoffice.gov.uk < $LOG
#
# Execution:
# Execute it with cron as above, daily at 9am should be OK.
#
# Revision:
# MB-576   Initial Version                                      Andy Moorhouse
# ----------------------------------------------------------------------------

module load scitools
module display scitools

# NB CLIMAT datatype not in list below  as its control file is different.
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
