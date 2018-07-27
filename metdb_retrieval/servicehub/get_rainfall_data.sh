#!/bin/bash -l   

#-----------------------------------------------------------------------
#
# SCRIPT        : get_rainfall_data.sh   
#
# PURPOSE       : Run python retrieval script to get RAINFALL data for
#                 ServiceHub.
#
# CALLED BY     : moodsf cron  
#
# ARGUMENTS     : (1) config file
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

module load scitools
module display scitools 

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <full_path_to_config_file>"
  exit 8
fi
CONFIG=$1

if [[ ! -s $CONFIG ]]; then
  echo "Error: $CONFIG file not found"
  exit 8
fi

# Get the base directory from the config file
# (gets the line starting base_dir; gets the bit after the equals; trims white space)

base_dir=$(grep ^base_dir $CONFIG | cut -d'=' -f2 | sed 's/^ *//;s/ *$//')


# Get the output directory from the config file in a similar fashion
output_dir=$(grep ^output_dir $CONFIG | cut -d'=' -f2 | sed 's/^ *//;s/ *$//')

# Get the python path from the config file
pypath=$(grep ^pythonpath $CONFIG | cut -d'=' -f2 | sed 's/^ *//;s/ *$//')
echo 'pypath is '$pypath
export PYTHONPATH=$PYTHONPATH:$pypath

# Path to ecCodes code/flag tables
export ECCODES_DEFINITION_PATH=/var/moods/eccodes/share/eccodes/definitions:\
/var/moods/metdb_retrieval/local_defs/


#
# Run the retrieval
#
python $base_dir/python/get_data.py -c $CONFIG



