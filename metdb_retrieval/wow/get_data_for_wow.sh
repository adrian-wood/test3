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
# MB-1827: Jan 2019 switch to experimental version for metdb-python
# MB-1780: Jul 2018 FTP and notifications added.        Sheila Needham
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

module load scitools/experimental_legacy-current
module display scitools/experimental_legacy-current

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

if [[ ! -e "$base_dir" ]]; then
  echo "Error: invalid base_dir" $base_dir
  exit 8
fi

# Get the output directory from the config file in a similar fashion
output_dir=$(grep ^output_dir $CONFIG | cut -d'=' -f2 | sed 's/^ *//;s/ *$//')

# Get the python path from the config file
pypath=$(grep ^pythonpath $CONFIG | cut -d'=' -f2 | sed 's/^ *//;s/ *$//')
export PYTHONPATH=$PYTHONPATH:$pypath

#
# Run the retrieval
#
python $base_dir/python/get_data.py -c $CONFIG
rc=$?
if [[ $rc -ne 0 ]]; then
  echo "Errors in retrieval"
  mailx -s "African WOW retrieval error" metdb@metoffice.gov.uk < $base_dir/wow/email.txt
  exit 8
fi

#
# transfer output files to Dart
#
num_files=$(ls -1 $output_dir/wow*.csv 2>/dev/null | wc -l)
echo "$num_files files to transfer"

#  ... check that there are some to copy

if [ "$num_files" -gt 0 ]
then

  ftplog=$(mktemp /tmp/ftplog.wow.XXXXXX)
  echo "ftp logging to $ftplog"

# Copy one at a time - to a temporary name first 

  for f in $output_dir/wow*.csv
  do
    output=${f##/*/}
    echo "copying $f to FTPCDN:$output"

ftp -v excftpcdn <<EOF > $ftplog
ascii
put $f $output.tmp
rename $output.tmp $output
quit
EOF
    cat $ftplog
    grep 'Rename successful' $ftplog >/dev/null 2>&1
    rc=$?
    echo "rc from ftp = $rc"
    if [ "$rc" -eq 0 ]
    then
      echo "deleting $f"
      rm $f
    fi

  done


fi

