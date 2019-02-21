#!/bin/bash                                                                      
#------------------------------------------------------------------------        
# PURPOSE : Tidy up files from servicehub suite.
#------------------------------------------------------------------------

echo "$0: starting at `date`"

###. ~/cylc-run/servicehub/base_env.rc
echo "BASE_DIR: $BASE_DIR"

# -------------------------------------------------------------------------------        
# cylc suite output (for the user running the suite)
#------------------------------------------------------------------------
echo "> Tidying up cylc suite output files..."
for SUITE in $(cylc scan|cut -f1 -d' '); do
    echo ">> tidying cylc suite $SUITE:"
    find ~/cylc-run/${SUITE}/log/job -mindepth 1 -maxdepth 1 -type d -mmin +120 -print -exec rm -fr {} +
    find ~/cylc-run/${SUITE}/work -mindepth 1 -maxdepth 1 -type d -mmin +120 -print -exec rm -fr {} +
done
echo "> ...done!"
#------------------------------------------------------------------------
# ServiceHub processed files.                                   
#------------------------------------------------------------------------
echo "> Tidying up ServiceHub processed files..."
for DIR in $(find $BASE_DIR/processed -maxdepth 1 -mindepth 1 -type d); do 
    echo ">> in $DIR"
    find $DIR -maxdepth 1 -name "*.csv" -mmin +20 -print -exec rm -fr {} +
    echo "> ...done!"
done
echo "> all...done!"

echo "$0: ended at `date`"
exit

