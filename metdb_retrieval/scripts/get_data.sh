#!/bin/bash -l

#-----------------------------------------------------------------------
#
# SCRIPT        : get_data.sh
#
# PURPOSE       : Run python retrieval script to get data as specified
#                 by the config file for ServiceHub.
#
# CALLED BY     : cylc suite or from the command line
#
# ARGUMENTS     : (1) config file
#               : (2) cycle name (use a dummy name if running outside
#                     of cylc)
#
# EXAMPLE       : ./get_data.sh ../servicehub/aireps.cfg 20190101T1200Z
#
# ENVIRONMENT   : BASE_DIR set in cylc (or local export) points to
#                 metdb_retrieval directory
#
# REVISION INFO :
#
#-----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2018 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
#-----------------------------------------------------------------------
function finish () {
  # Always exit 0 to keep the suites running
  rc=$1
  echo "Finishing with return code $rc"
  if [ "$rc" -ne 0 ]; then
      mailx -s "Unknown error from $CYLC_SUITE_NAME : $CYLC_TASK_ID" \
             "$CONTACT"

  fi
  exit 0
}

trap "finish $?" EXIT

module load scitools/experimental_legacy-current
module display scitools/experimental_legacy-current 2>&1

: "${BASE_DIR:?Need to set BASE_DIR non-empty}"

. "$BASE_DIR"/scripts/config_utils.sh  || exit 1

echo "Using BASE_DIR $BASE_DIR"

# Check arguments

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <full_path_to_config_file> <cycle_point>"
  exit 1
fi
CONFIG=$1

if [[ ! -s $CONFIG ]]; then
  echo "Error: $CONFIG file not found"
  exit 1
fi

subtype=$(get_config "subtype")
check_config "subtype" "$subtype"

package=$(get_config "package")
check_config "package" "$package"

contact=$(get_config "contact")
check_config "contact" "$contact"

# Path to ecCodes code/flag tables
export ECCODES_DEFINITION_PATH=$ECCODES/share/eccodes/definitions:\
$BASE_DIR/local_defs/

export PYTHONPATH=$BASE_DIR/python

#
# Run the retrieval
#
python $BASE_DIR/python/get_data.py -c $CONFIG
rc=$?

if [ "$rc" -ne 0 ]; then
   echo "get_data.py failed"
   envsubst < "$BASE_DIR"/"package"/email.txt | \
   mailx -s "MetDB_retrieval error from $CYLC_SUITE_NAME : $CYLC_TASK_ID" \
            "$contact" 
fi
echo "Return code from get_data: $rc"
exit 0

