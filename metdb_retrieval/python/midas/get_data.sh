#!/bin/bash -l

#-----------------------------------------------------------------------
#
# SCRIPT        : get_data.sh
#
# PURPOSE       : Run python retrieval script to get data as specified
#                 by the config file for MIDAS
#
# CALLED BY     : moodsf cron
#
# ARGUMENTS     : (1) config file
#
# REVISION INFO :
#
# MB-1849: Initial version for MIDAS extractions
#-----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2019 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
#-----------------------------------------------------------------------


module load scitools/experimental_legacy-current
module display scitools/experimental_legacy-current

get_config() {

# Parse the config file to get the value of a variable specified
# as the argument.
# (gets the line starting with the arg | gets the bit after the equals | trims white space)
# Echoing the result is a way of getting text back to the caller.

  var=$(grep ^$1 $CONFIG | cut -d'=' -f2 | sed 's/^ *//;s/ *$//')
  echo $var

}

check_config() {

# check a variable has been set

  if [ -z "$2" ]; then
    echo "$1 is undefined in $CONFIG"
    exit 1
  fi
}

#
# Check argument
#
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
# Get details from config file
# 
base_dir=$(get_config "base_dir")
check_config "base_dir" $base_dir

output_dir=$(get_config "output_dir")
check_config "output_dir" $output_dir

dest_dir=$(get_config "dest_dir")
check_config "dest_dir" $dest_dir

output_file=$(get_config "output_file")
check_config "output_file" $output_file

file_pattern=${output_file/<*>/*}
pypath=$(get_config "pythonpath")
check_config "pythonpath" $pypath

subtype=$(get_config "subtype")
check_config "subtype" $subtype

contact=$(get_config "contact")
check_config "contact" $contact

eccodes=$(get_config "eccodes")
check_config "eccodes" $eccodes

echo "MetDB subtype             : $subtype"
echo "Temporary output directory: $output_dir"
echo "CTS destination directory : $dest_dir"
echo "Output file_pattern       : $file_pattern"
echo "PYTHONPATH                : $pypath"
echo "Contact                   : $contact"

. $base_dir/servicehub/sendfile.sh
export PYTHONPATH=$PYTHONPATH:$pypath

# Path to ecCodes code/flag tables
export ECCODES_DEFINITION_PATH=$eccodes/share/eccodes/definitions:\
$base_dir/local_defs/

#
# Run the retrieval
#
echo "Starting retrieval..."
python $base_dir/python/get_data.py -c $CONFIG
rc=$?

if [[ $rc -ne 0 ]]; then
  echo "Errors in retrieval"
  mailx -s "ServiceHub $subtype retrieval error" $contact < $base_dir/servicehub/email.txt
  exit 8
fi

#
# transfer output files to Cloud Transfer Service
#
#CTS1=ssaftp01-zvopaph1
#CTS2=ssaftp02-zvopaph2

#num_files=$(ls -1 $output_dir/$file_pattern 2>/dev/null | wc -l)
#echo "$num_files files to transfer"

#  ... check that there are some to copy

#if [ "$num_files" -gt 0 ]; then

#
# Copy one at a time - trying the secondary server if the first
# one fails. On successful transfer move the temporary file to
# /tmp with a unique name made from concatenating the CTS dest
# with the filename.  These will get deleted by tmpwatch after
# 10 days.
#

#  for infile in $output_dir/$file_pattern
#  do
#    outfile=${infile##/*/}
#    sendfile $CTS1 $infile $dest_dir $outfile
#    rc=$?
#
#    if [ "$rc" -ne 0 ]; then
#      echo "trying secondary server"
#      sendfile $CTS2 $infile $dest_dir $outfile
#      rc=$?
#      if [ "$rc" -ne 0 ];then
#        echo "FTP failed on both servers"
#        mailx -s "ServiceHub $subtype FTP error" $contact < $base_dir/servicehub/email.txt
#      else
#        mv $infile /tmp/$dest_dir$outfile
#      fi
#    else
#      mv $infile /tmp/$dest_dir$outfile
#    fi
#
#  done
#fi

