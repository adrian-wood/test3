#!/bin/bash -l   

#-----------------------------------------------------------------------
#
# SCRIPT        : get_data_for_wow.sh
#
# PURPOSE       : Run python retrieval script to get LNDSYN data for
#                 a list of African countries, format it as csv files
#                 and ftp it to DART for onward transmission to the WOW
#                 servers.
#
# CALLED BY     : moodsf cron  
#
# ARGUMENTS     : (1) config file
#
# REVISION INFO :
# MB-1638: May 2018 Original.                           Sheila Needham
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

if [[ ! -d $base_dir ]]; then
  echo "Error: invalid config - base_dir=$base_dir"
  exit 8
fi

# Get the output directory from the config file in a similar fashion
output_dir=$(grep ^output_dir $CONFIG | cut -d'=' -f2 | sed 's/^ *//;s/ *$//')

if [[ ! -d $output_dir ]]; then
  echo "Error: invalid config - output_dir=$output_dir"
  exit 8
fi

#
# Run the retrieval
#
python $base_dir/get_data_for_wow.py -c $CONFIG

exit
# Not ready for this bit yet
#
# transfer output files to...
#
for f in $output_dir/wow*.csv
do

  echo "copying $f to ..."
  sftp dart.metoffice.gov.uk <<EOF
cd /metdb/wow
ascii
put $f $f.tmp
rename $f.tmp $f
quit
EOF

  rm $f
done


