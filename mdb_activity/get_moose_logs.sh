#!/bin/bash
#------------------------------------------------------------------------        
# PURPOSE : Script to retrieve yesterday's moose logs for archiving and
#           potential future use.
#------------------------------------------------------------------------

echo "$0: starting at `date`"

SRC_DIR=/var/moods/tmp
BASE_ARCH_DIR=/var/moods/mdb_activity/data/moose_logs/incoming
SERVERS="mdbapus-prod mdbapop-prod"

FILE=MDBREST2.output.d`date --date="yesterday" +%y%j`*
ZIP=moose_logs_`date --date="yesterday" +%Y%m%d`.zip

for SERVER in $SERVERS; do
  echo "$0: retrieving moose logs from $SERVER..."
  ARCH_DIR=$BASE_ARCH_DIR/$SERVER/`date --date="yesterday" +%Y/%m`
  mkdir -p $ARCH_DIR
  scp -rq moodsf@$SERVER:$SRC_DIR/$FILE $ARCH_DIR

  # zip 'em up to save space (any other processes using them will need to unzip)
  echo "$0: zipping up retrieved moose logs from $SERVER..."
  zip -jq $ARCH_DIR/$ZIP $ARCH_DIR/$FILE
  echo "$0: removing retrieved moose logs from $SERVER..."
  rm -f $ARCH_DIR/$FILE

done

echo "$0: ended at `date`"
exit
