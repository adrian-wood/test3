#!/bin/bash                                                                      
#------------------------------------------------------------------------        
# PURPOSE : Performs housekeeping for the moodsf account on mdb-apps.
#------------------------------------------------------------------------
# MB-1763 : Initial version.                               Andy Moorhouse
#------------------------------------------------------------------------

echo "$0: starting at `date`"
. ~/.bash_profile
cd /var/moods/housekeeping

#------------------------------------------------------------------------        
# mdb-activity (retrieved data_access.log files):
# Add to monthly zip files, keeping 32 days online
# In /var/www/html/mdb_activity/data_access_logs/<server>/<YYYY>/<MM>
# NB this is a very pedantic routine - it expects the files to be in
# directories named exactly as above.
#------------------------------------------------------------------------
echo "> Tidying up retrieved data_access.log files..."
AGE=32
BASE=/var/www/html/mdb_activity/data_access_logs/
SERVERS="mdbdb-prod mdbapop-prod mdbapus-prod"
for SERVER in $SERVERS; do

    echo ">> tidying data_access.log files for $SERVER:"
    cd $BASE/$SERVER

    # Slightly unusual way of dealing with files returned from "find"
    # because we need to determine which zip file to put them in...
    find . -mtime +$AGE -type f -not -name "*.zip" | while read -r FILE; do

        # Work out which <YEAR>.zip to add it to...
        YEAR=$(dirname "${FILE}"|cut -f2 -d"/")

        # Add the file to the year's zipfile. Options supplied to zip:
        # -u - add to existing xzip file, creating if necessary
        # -m - remove the file after adding it to the zip
        # -T - check it has been successfully added before removing
        zip -umT ${YEAR}.zip $FILE

    done

    # This may leave empty <MM> directories, so remove them if so
    echo ">> removing empty directories for $SERVER:"
    find ./ -type d -empty -print -delete

done

# We may now have broken symlinks in the directory where any 
# processing of the data_access.log files is done, i.e.
# /var/moods/mdb_activity/data/data_access_logs, so remove
# any broken links there...
echo ">> removing broken symlinks from processing area:"
find -L /var/moods/mdb_activity/data/data_access_logs -type l -print -delete
    
echo "> ...done!"


#------------------------------------------------------------------------        
# cylc suite output:
# Just keep 8 days online
#------------------------------------------------------------------------
echo "> Tidying up cylc suite output files..."
AGE=8
for SUITE in `cylc scan|cut -f1 -d' '`; do
    echo ">> tidying cylc suite $SUITE:"
    find ~/cylc-run/${SUITE}/log/job -mindepth 1 -maxdepth 1 -type d -mtime +$AGE -print -exec rm -fr {} +
    find ~/cylc-run/${SUITE}/work -mindepth 1 -maxdepth 1 -type d -mtime +$AGE -print -exec rm -fr {} +
done
echo "> ...done!"

echo "$0: ended at `date`"
exit
