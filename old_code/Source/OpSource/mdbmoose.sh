#!/bin/sh

. /etc/profile

#-----------------------------------------------------------------------
# FILE NAME      : mdbmoose.sh
#
# TYPE           : Script
#
# PURPOSE        : Restore a MetDB file from MASS-R
#
# USAGE          : mdbmoose.sh mdb_dsn restored_mdb_dsn mstream
#
# DESCRIPTION    : This script will restore a file from MASS-R, but
#                  attempts to avoid re-restoring a file that has been
#                  or is in the process of being restored. To do this,
#                  it first checks whether the dataset already exists
#                  and exits if it does. Otherwise it checks for a
#                  <mdb.dsn>.QUEUE file, indicating another restore job
#                  is in action. The script will wait for this QUEUE file
#                  to disappear and the restored dataset to appear before
#                  exiting. If the restored dataset never appears, or 
#                  the QUEUE file is not found in the first place, a 
#                  MOOSE get request is sumbitted.
#                  The script returns with an error code of 8 if the
#                  restore fails.
#
#
# REVISION INFO  :
#
# $Workfile: mdbmoose.sh$ $Folder: OpSource$
# $Date: 15/06/2012 09:53:00$
# $Revision: 5$
#
# $Author: Sheila Needham$
#
# $Log:
#  5    MetDB_Refresh 1.4         15/06/2012 09:53:00    Sheila Needham  Change
#        path to moo_extract script and initialise environment
#  4    MetDB_Refresh 1.3         14/03/2011 10:50:48    Alison Weir     Change
#        use of case conversion command 'tr'
#  3    MetDB_Refresh 1.2         16/11/2010 10:20:39    Alison Weir
#       Correct header
#  2    MetDB_Refresh 1.1         16/11/2010 10:15:19    Alison Weir     Header
#        added.
#  1    MetDB_Refresh 1.0         12/11/2010 11:51:34    Sheila Needham  Unix
#       script for offline retrieval
# $
#-----------------------------------------------------------------------

# Function to add a dated message to a log file
function log_message {
    msg=$1
    logfile=$2
    echo `date` $msg >> $logfile
}

# Function to check the existence of a file on mvs
function check_mvs_file {
    filename=$1
    random_name=$2
    mvs_filename="//'$filename'"
    check_file="/tmp/check."$random_name
    rm -f $check_file
    head -b1 """$mvs_filename""" > $check_file 2>/dev/null
    rc=$?
    if [[ $rc -eq 0 && -s $check_file ]]; then
        found=found
    else
        found="not found"
    fi
    rm -f $check_file
    echo $found
}


mdb_dsn=$1
restored_mdb_dsn=$2
mstream=$3

# -----------------------------------------------
# Set up variables to define temporary file names
# -----------------------------------------------

logdir=/tmp/
tempdir=/tmp/mdb/

datestamp=`date '+%y%j'`
time_now=`date '+%H%M%S'`
random=$RANDOM

id="d"$datestamp"t"$time_now"r"$random

logfile=$logdir"MDBREST2.output."$id

log_message "mdbmoose.sh started" $logfile

moo_col=`echo mdb$mstream.xmit.file | tr '[:upper:]' '[:lower:]'`

# ------------------------------------------------------------------
# Is Restored_MDB_DSN already on disk? Unlikely because MDBALC
# will have already performed an INQUIRE on it. But, just in case
# there was a delay in executing this script...
# ------------------------------------------------------------------

exists=`check_mvs_file $restored_mdb_dsn $id`
if [[ $exists == "found" ]]; then
    exit 0
fi


# ------------------------------------------------------------------
# Is there a $restored_mdb_dsn.QUEUE on disk? If so, it means
# another MetDB offline restore job is in the process of restoring
# it. Wait.
# ------------------------------------------------------------------

queue_mdb_dsn=$tempdir$restored_mdb_dsn".QUEUE"

if [[ -e $queue_mdb_dsn ]]; then
 # wait for $queue_mdb_dsn to disappear, then check for $restored_mdb_dsn

    max_tries=120
    retry_sleep=60
    count=0
    while [[ $count -lt $max_tries ]]; do
        count=$count+1
        log_message "$queue_mdb_dsn on disk; sleeping $retry_sleep secs" $logfile
        sleep $retry_sleep
        if [[ ! -e $queue_mdb_dsn ]]; then
            break
        fi
    done

 # if $queue_mdb_dsn doesn't disappear, check for $restored_mdb_dsn anyway

    if [[ $count -ge $max_tries ]]; then
        log_message "Timed out waiting for $queue_mdb_dsn to be deleted." $logfile
        log_message "Will continue with get anyway." $logfile
    fi

 # if $restored_mdb_dsn appears, exit
 # if $restored_mdb_dsn doesn't appear, continue and restore from mass

    max_tries=20
    retry_sleep=30
    count=0
    while [[ $count -lt $max_tries ]]; do
        count=$count+1
        exists=`check_mvs_file $restored_mdb_dsn $id`
        if [[ $exists == "found" ]]; then
            rm -f $logfile
            rm -f $queue_mdb_dsn
            exit 0
        else
            log_message "$restored_mdb_dsn not ready; sleeping $retry_sleep secs" $logfile
            sleep $retry_sleep
        fi
    done

fi


# -----------------------------------------------
# Create and run MOOSE command
# -----------------------------------------------

touch $queue_mdb_dsn 

cmd="/usr/local/mass/client/dpp_conversions/moo_extract.sh"
mo_cmd=$cmd" -g moose:misc/metdb/"$moo_col"/"$mdb_dsn" //"$restored_mdb_dsn

# Run
$mo_cmd >> $logfile 2>&1
rc=$?

log_message "return code from moose=$rc" $logfile

rm -f $queue_mdb_dsn

# Exit with appropriate return code
if [[ $rc -eq 0 ]]; then
    rm -f $logfile
    mdb_rc=0
else
    mdb_rc=8
fi

exit $mdb_rc


