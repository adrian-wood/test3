#!/bin/bash
#------------------------------------------------------------------------        
# PURPOSE : Script to retrieve data_access logs for archiving and
#           potential future use.
#------------------------------------------------------------------------

echo "$0: starting at `date`"

SRC_DIR=/var/moods/logs
BASE_ARCH_DIR=/var/www/html/mdb_activity/data_access_logs
INCOMING=/var/moods/mdb_activity/data/data_access_logs/incoming
SERVERS="mdbapus-prod mdbapop-prod mdbdb-prod"
FILE=data_access.log-`date +%Y%m%d`

for SERVER in $SERVERS; do
  echo "$0: getting $FILE from $SERVER..."
  ARCH_DIR=$BASE_ARCH_DIR/$SERVER/`date +%Y/%m`
  mkdir -p $ARCH_DIR
  scp -q moodsf@$SERVER:$SRC_DIR/$FILE $ARCH_DIR

  # Create a symlink for potential future processing...
  ln -s $ARCH_DIR/$FILE $INCOMING/$SERVER-$FILE
done

echo "$0: ended at `date`"
exit
