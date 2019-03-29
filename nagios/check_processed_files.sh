#!/bin/bash
#-----------------------------------------------------------------------
#
# SCRIPT        : check_processed.sh
#
# PURPOSE       : Looks for files in the metdb_retrieval processed
#                 directories to check something has been produced within
#                 a certain time limit.  This varies by datatype and is
#                 defined in the AGE dictionary in minutes.
#                 If there are no files within the specified period then
#                 a Nagios warning is raised as this suggests there is a
#                 a problem with either the data extraction or FTP
#                 processes.
#
#------------------------------------------------------------------------
# MB-1828: New                                             Sheila Needham
#------------------------------------------------------------------------

echo "$0: starting at `date`"

. /var/moods/cylc-run/servicehub/base_env.rc
echo "BASE_DIR: $BASE_DIR"

# Load/define global Nagios variables...
. /home/moodsf/bin/nagios_vars.sh

if [[ "$HOSTNAME" == *"tst"* ]]; then
    host=mdb-apps-test
else
    host=mdb-apps
fi
echo "host=$host"
checkname=processed_files_status


declare -A AGE
AGE=(['aireps']=60 \
     ['airqal-international']=2880 \
     ['airqal-uk']=2880 \
     ['amdars']=120 \
     ['argob']=120 \
     ['buoyb']=120 \
     ['lndsyn-international']=60 \
     ['lndsyn-uk']=60 \
     ['metars-international']=60 \
     ['metars-uk']=60 \
     ['modes']=60 \
     ['ncm-uk']=720 \
     ['shpsyn']=60 \
     ['sonde-international']=720 \
     ['sonde-uk']=720 \
     ['speci-international']=60 \
     ['speci-uk']=60 \
     ['srew-international']=60 \
     ['srew-uk']=60 \
     ['tafs-international']=60 \
     ['tafs-uk']=60 \
     ['tidegage']=60 \
     ['wavenet-uk']=120 \
     ['ea-rain']=120) 

nag_msg="No processed files:"
nag_flag=0

#------------------------------------------------------------------------
# Check the number of processed files 
#------------------------------------------------------------------------
for DIR in $(find $BASE_DIR/processed -maxdepth 1 -mindepth 1 -type d); do
    echo ">> in $DIR"
    ptype=${DIR##/*/}        # the last part of the path
    age=${AGE[$ptype]}
    if [ -z "$age" ]; then
        # this directory is not monitored - skip it
        continue
    fi
    echo "Checking for files created within $age minutes for $ptype"
    count=$(find $DIR -mmin -$age -type f -printf '.' | wc -c)
    echo $count 
    if [ $count -eq 0 ]; then
        nag_msg="$nag_msg$ptype "
        nag_flag=1
    fi
    
done
# create Nagios message
if [ "$nag_flag" -eq 0 ]; then
    nagios_msg="Processed directories up to date"
    rc=0
else
    nagios_msg="ERROR: $nag_msg"
    rc=1
fi
echo "Sending following message to Nagios: $nagios_msg"

# Raise Nagios passive check NB must have sourced nagios_vars.sh
echo -e "$host\t$checkname\t$rc\t$nagios_msg" | $NAG_SCRIPT -u $NAG_URL -t $NAG_TOKEN

echo "$0: ended at `date`"
exit

