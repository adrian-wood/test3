#!/bin/bash                                                                      
#-----------------------------------------------------------------------
#
# SCRIPT        : run_hk.sh
#
# PURPOSE       : Tidies up processed files from metdb-retrieval
#                 applications more than AGE days old.
#
#------------------------------------------------------------------------
# MB-1810: New                                             Sheila Needham
#------------------------------------------------------------------------

echo "$0: starting at `date`"

. /var/moods/cylc-run/servicehub/base_env.rc
echo "BASE_DIR: $BASE_DIR"

AGE=5

#------------------------------------------------------------------------
# ServiceHub processed files.                                   
#------------------------------------------------------------------------
echo "> Tidying up ServiceHub processed files..."
for DIR in $(find $BASE_DIR/processed -maxdepth 1 -mindepth 1 -type d); do 
    echo ">> in $DIR"
    find $DIR -maxdepth 1 -name "*.csv" -mtime +$AGE -print -exec rm -fr {} +
    echo "> ...done!"
done

echo "$0: ended at `date`"
exit

