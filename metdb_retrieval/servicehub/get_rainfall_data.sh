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
# MB-1790: Added FTP step
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

. /var/moods/metdb_retrieval/servicehub/sendfile.sh

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

rc=$?

if [[ $rc -ne 0 ]]; then
  echo "Errors in retrieval"
  mailx -s "ServiceHub RAINFALL retrieval error" metdb@metoffice.gov.uk < $base_dir/servicehub/email.txt
  exit 8
fi

#
# transfer output files to Cloud Transfer Service
#
CTS1=ssaftp01-zvopaph1
CTS2=ssaftp02-zvopaph2
DEST=ea-rain-csv

num_files=$(ls -1 $output_dir/rainfall*.csv 2>/dev/null | wc -l)
echo "$num_files files to transfer"

#  ... check that there are some to copy

if [ "$num_files" -gt 0 ]
then

# Copy one at a time - trying the secondary server if the first
# one fails.

  for infile in $output_dir/rainfall*.csv
  do
    outfile=${infile##/*/}
    sendfile $CTS1 $infile $DEST $outfile
    rc=$?

    if [ "$rc" -ne 0 ]
    then
      echo "trying secondary server"
      sendfile $CTS2 $infile $DEST $outfile
      rc=$?
      if [ "$rc" -ne 0 ]
      then
        echo "FTP failed on both servers"
      else
        rm $infile
      fi
    else
      rm $infile
    fi

  done

fi


