#!/bin/bash
#------------------------------------------------------------------------
#
#  SCRIPT      : deploy.sh
# 
#  PURPOSE     : To update metdb_retrieval applications from a source
#                repository.
#                Makes a backup on /tmp first then merges the latest run
#                times from current config files into the source configs
#                before copying the whole source to the required 
#                destination.
#
#  CALLED BY   : user
#
#  ARGUMENTS   : deploy.sh <SOURCE> <DEST>
#    where       SOURCE - location of the metdb_retrieval directory in the      
#                         local repository
#                DEST   - installation directory which does (or will) 
#                         contain a metdb_retrieval directory.
#                                       
#
#  EXAMPLE     : deploy.sh ./metdb-misc/metdb_retrieval /var/moods/
#                 
# 
#  HISTORY  
#  MB-1810: Exclude cylc directories from deployment as these are
#           updated manually. Do not delete unused files from target (to
#           preserve processed and queued directories.                SN 
#
#
#-----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2018 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database
# Team at the above address.
#------------------------------------------------------------------------

# Check for valid arguments

if [ $# -ne 2 ]; then
  echo "USAGE: $0 <SOURCE> <DEST>"
  exit 1
fi

SRC=$1
DEST=$2
TARGET=metdb_retrieval

if [ ! -d "$SRC" ]; then
  echo "ERROR: $0 invalid source $SRC"
  exit 1
fi

if [ ! -d "$DEST" ]; then
  echo "WARNING: $0 destination directory does not exist"
  echo "... creating $DEST"
  mkdir -p $DEST
fi

# converts to absolute path and removes trailing "/"
SRC=$(readlink -f $SRC)
DEST=$(readlink -f $DEST)

echo "{SRC##/*/}"  ${SRC##/*/}
if [ "${SRC##/*/}" != "$TARGET" ]; then
  echo "Source directory must be $TARGET"
  exit 1
fi

# Remove metdb_retrieval from DEST location if present
if [ "${DEST##/*/}" == "$TARGET" ]; then
  DEST=${DEST%/$TARGET}
fi
        
# backup destination if it exists
if [ -d "$DEST/$TARGET" ]; then
  BACKUP=$(mktemp -d)
  echo "Making backup of $DEST/$TARGET in $BACKUP"
  echo "rsync..."
  rsync -av --exclude 'processed' $DEST/$TARGET $BACKUP
  rc=$?
  echo "...complete. RC=$rc"
fi

# find all the new config files

CONFIGS=$(mktemp)
find $SRC -name "*.cfg" > $CONFIGS
echo -e "\nNew config files:"
cat $CONFIGS

echo -e "\nOld config files with run_times:"
find $DEST/$TARGET -name "*.cfg" -exec grep -H run_time {} \;

# update cfg files in SRC so they have the latest run_times from DEST
while read config
do
  filename=${config##/*/}                                  # just the filename from the full path

  current=$(find $DEST/$TARGET -name $filename 2>/dev/null)      # the location of that file in DEST
  if [ -z "$current" ]; then
    echo "new config $filename does not need to be merged"
    continue
  fi
  echo "updating $filename from $current"

  latest_run=$(grep run_time $current)
  new_line=$(grep run_time $config)
  sed -i -e "s/$new_line/$latest_run/g" $config
done < $CONFIGS

# then copy to DEST 
echo "Copying files"
echo "rsync...from $SRC to $DEST"
rsync -rv --exclude 'cylc' $SRC $DEST
rc=$?
echo "...complete. RC=$rc"

# Tidy up
if [ "$rc" -eq 0 ]; then
  rm $CONFIGS
else
  echo "ERROR: rsync error code $rc"
  exit 1
fi

