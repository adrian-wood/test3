#!/bin/bash -l
module load scitools
module display scitools

if [ $# -ne 2 ]
then
  echo 'Usage:$0 <earliest Vol A file> <latest Vol A file>'
  exit 1
fi



old_vola=$1
new_vola=$2

# Location of convert_vola.py file
source=.

# Log file
logfile=$(mktemp /tmp/stnmas_update.XXXXXX)

printf "Converting %s...",$old_vola
python $source/convert_vola.py $old_vola > $logfile 2>&1
rc=$?
if [ $rc -ne 0 ]
then
  echo "error converting $old_vola, see $logfile"
  exit 1
fi  
printf "...Done.\n"

printf "Converting %s...",$new_vola
python $source/convert_vola.py $new_vola  >> $logfile 2>&1
rc=$?
if [ $rc -ne 0 ]
then
  echo "error converting $new_vola, see $logfile"
fi

printf "...Done.\n"

echo "New stations"
diff -y --suppress-common-lines -W200  $old_vola.stnmas $new_vola.stnmas | grep '>' | cut -d'>' -f2 

echo "Updated stations"
diff -y --suppress-common-lines -W200  $old_vola.stnmas $new_vola.stnmas | grep '|' | cut -d'|' -f2

echo "Deleted stations"
diff -y --suppress-common-lines -W200  $old_vola.stnmas $new_vola.stnmas | grep '<' | cut -d'<' -f1

# Tidy up
rm -f $old_vola.stnmas
rm -f $new_vola.stnmas
# leave the logfile as it's on /tmp anyway


