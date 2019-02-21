#!/bin/bash -l

#-----------------------------------------------------------------------
#
# SCRIPT        : send_data.sh
#
# PURPOSE       : Transfer data from local directories to an FTP site.
#
# CALLED BY     : moodsf from cylc suite
#
# ARGUMENTS     : (1) config file
#                 (2) cycle number, e.g. 20181212T1310Z
#
# ENVIRONMENT   : BASE_DIR must point to the retrieval root, e.g.
#                  export BASE_DIR=/var/moods/metdb_retrieval/
#
# REVISION INFO :
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

: "${BASE_DIR:?BASE_DIR must be set}"

. "$BASE_DIR"/scripts/config_utils.sh  || exit 1
. "$BASE_DIR"/scripts/sendfile.sh || exit 1

#
# Check arguments
#
if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <full_path_to_config_file> <cycle_point>"
  exit 1
fi
CONFIG=$1
CYCLE=$2
echo "CONFIG is $CONFIG"
echo "CYCLE  is $CYCLE"

if [[ ! -s $CONFIG ]]; then
  echo "Error: $CONFIG file not found"
  exit 1
fi

#
# Get details from config file - fail if any not set
#

# location csv of output from get_data retrieval
output_dir=$(get_config "output_dir")
check_config "output_dir" "$output_dir"

# location for successfully copied files to be moved as backup
processed_dir=$(get_config "processed_dir")
check_config "processed_dir" "$processed_dir"
[[ -d "$processed_dir" ]] || mkdir -p "$processed_dir"

# one or more FTP sites - will be tried in order
dest_ftp=$(get_config "dest_ftp")
check_config "dest_ftp" "$dest_ftp"
read -r -a ftp_server <<< "$dest_ftp"

# location for files on FTP site
dest_dir=$(get_config "dest_dir")
check_config "dest_dir" "$dest_dir"

# csv filename template translated to a pattern match
output_file=$(get_config "output_file")
check_config "output_file" "$output_file"
file_pattern=${output_file/<*>/*}

subtype=$(get_config "subtype")
check_config "subtype" "$subtype"

package=$(get_config "package")
check_config "package" "$package"

contact=$(get_config "contact")
check_config "contact" "$contact"

echo "Application package       : $package"
echo "MetDB subtype             : $subtype"
echo "Contact                   : $contact"
echo "Temporary output directory: $output_dir"
echo "FTP destination(s)        : ${ftp_server[@]}"
echo "CTS destination directory : $dest_dir"
echo "Output file_pattern       : $file_pattern"
echo "Processed directory       : $processed_dir"

#
# transfer output files to named system
#

while :
do
    printf "Looking for files in %s\n" $output_dir

    num_files=$(ls -1 $output_dir/$file_pattern 2>/dev/null | wc -l)
    printf "%d files to transfer\n" $num_files

    #  ... check that there are some to copy

    if [ "$num_files" -gt 0 ]
    then

        # Copy one at a time - trying multiple servers if available

        for infile in $output_dir/$file_pattern
        do
            outfile=${infile##/*/}
            for server in "${ftp_server[@]}"
            do
                sendfile $server $infile $dest_dir $outfile
                rc=$?
                if [ "$rc" -eq 0 ]; then
                    mv $infile $processed_dir/$outfile
                    if [ $? -ne 0 ]; then
                         echo "Unable to move file; delete instead."
                         rm $infile
                    fi
                    break
                fi
            done
            if [ "$rc" -ne 0 ]; then
                echo "FTP failed"
                mailx -s "MetDB_retrieval error from $CYLC_SUITE_NAME : $CYLC_TASK_ID" \
                         "$contact" < "$BASE_DIR"/"$package"/email.txt
                exit 0
            fi
        done  # end of loop over files

    else  # no files to send
 
        # Check for a get_data process running for the same CONFIG and CYCLE as this send_data which
        # may still be producing files.

        running=$(ps -fwwu $USER | grep get_data.sh | grep $CONFIG | grep $CYCLE | grep -v grep | wc -l)
        echo "running: " $running
        if [[ "$running" -eq 0 ]]; then
            echo "get_data not running. Stopping."
            exit 0
        fi
        date +"%T";printf " No files found; waiting 5 seconds\n"
        sleep 5
    fi
done
date +"%T";printf " Ending\n"

