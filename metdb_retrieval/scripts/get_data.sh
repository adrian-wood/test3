#!/bin/bash -l

#-----------------------------------------------------------------------
#
# SCRIPT        : get_data.sh
#
# PURPOSE       : Run python retrieval script to get data as specified
#                 by the config file for ServiceHub.
#
# CALLED BY     : moodsf cron
#
# ARGUMENTS     : (1) config file
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

module load scitools/experimental_legacy-current
module display scitools/experimental_legacy-current

: "${BASE_DIR:?Need to set BASE_DIR non-empty}"

echo "Using BASE_DIR $BASE_DIR"

# Check arguments

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <full_path_to_config_file> <cycle_point>"
  exit 8
fi
CONFIG=$1

if [[ ! -s $CONFIG ]]; then
  echo "Error: $CONFIG file not found"
  exit 8
fi

echo "...config file $CONFIG"

# Path to ecCodes code/flag tables
export ECCODES_DEFINITION_PATH=$ECCODES/share/eccodes/definitions:\
$BASE_DIR/local_defs/

export PYTHONPATH=$BASE_DIR/python

#
# Run the retrieval
#
python $BASE_DIR/python/get_data.py -c $CONFIG
rc=$?

echo "Return code from get_data: $rc"
exit $rc

