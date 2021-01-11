#!/bin/bash -l   

#-----------------------------------------------------------------------
#
# SCRIPT        : get_data_for_wow.sh
#
# PURPOSE       : Run python retrieval script to get LNDSYN data for
#                 a list of African countries, format it as csv files
#                 and ftp it to DART for onward transmission to the WOW
#                 servers.
#                 This is a wrapper for the more generic get_data and 
#                 send_data scripts because WOW retrievals are not
#                 run by cylc.
#
# CALLED BY     : moodsf cron  
#
# ARGUMENTS     : (1) config file
#
# ENVIRONMENT   : BASE_DIR required - metdb_retrieval base location
#
# REVISION INFO :
# MB-1810: Feb 2019 Re-written to use generic scripts.
# MB-1827: Jan 2019 switch to experimental version for metdb-python
# MB-1780: Jul 2018 FTP and notifications added.        Sheila Needham
# MB-1638: May 2018 Original.                           Sheila Needham
#
#-----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2019 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
#-----------------------------------------------------------------------

export BASE_DIR=/var/moods/metdb_retrieval
export ECCODES=/var/moods/eccodes
# This is not run by cylc but the following variables are assumed to be
# defined by get_data and send_data
export CYLC_SUITE_NAME=WOW         
export CYLC_TASK_ID=$0  
export CONTACT=metdb_wow@metoffice.gov.uk

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <full_path_to_config_file>"
  exit 8
fi
CONFIG=$1

if [[ ! -s $CONFIG ]]; then
  echo "Error: $CONFIG file not found"
  exit 8
fi

#
# Retrieve data - get_data handles error messages
#
$BASE_DIR/scripts/get_data.sh $CONFIG dummy


#
# Transfer output files to Dart - if there were get_data errors
# still run send_data in case there were files left from a previous run.
# Again, errors are handled inside send_data.
#
$BASE_DIR/scripts/send_data.sh $CONFIG dummy

