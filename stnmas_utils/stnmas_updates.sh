#!/bin/bash -l
#-------------------------------------------------------
#
# Purpose: Compares two versions of the WMO VolA legacy
#          data and outputs a summary.
#
#        1) Python to convert each to abrv_stnlist format
#        2) Unix diff to compare them
#        3) Gets max size of abrv_stnlist from a clone
#           of the source code that reads it.
#
#--------------------------------------------------------

trap 'echo "EXIT detected with exit status $?"' EXIT

tidy_up () {
  if [[ "$1" -gt 1 ]]; then
  # Delete all temp datasets (except the log)
    rm -f "$new_stations"
    rm -f "$updated_stations"
    rm -f "$deleted_stations"
    rm -f "$old_vola".stnmas
    rm -f "$new_vola".stnmas
    rm -rf "$tmp_dir"
  fi
}

if [[ "$#" -ne 2 ]]; then
  echo "Usage:$0 <earliest Vol A file> <latest Vol A file>"
  exit 1
fi

module load scitools
module display scitools

old_vola=$1
new_vola=$2

# Location of convert_vola.py file
source=/var/moods/stnmas_utils/

logfile=$(mktemp /tmp/stnmas_update.XXXXXX)
new_stations=$(mktemp /tmp/new_stations.XXXXXX)
updated_stations=$(mktemp /tmp/updated_stations.XXXXXX)
deleted_stations=$(mktemp /tmp/deleted_stations.XXXXXX)
tmp_dir=$(mktemp -d /tmp/stnmas-XXXXX)

printf "Converting %s...$old_vola"
python "$source"/convert_vola.py "$old_vola" >  "$logfile" 2>&1
rc=$?
if [ $rc -ne 0 ]
then
  echo "\nerror converting $old_vola, see $logfile"
  exit 2
fi  
printf "...Done.\n"

printf "Converting %s...$new_vola"
python "$source"/convert_vola.py "$new_vola"  >> "$logfile" 2>&1
rc=$?
if [ "$rc" -ne 0 ]; then
  echo "\nerror converting $new_vola, see $logfile"
  exit 2
fi
printf "...Done.\n"

# Find max number of lines allowed in abrv_stnmas by getting the
# parameter from source code

source_repo=git@bitbucket.org:metoffice/metdb.git
branch=develop
git clone "$source_repo" "$tmp_dir"
if [[ "$?" -ne 0 ]]; then
  echo "Failure cloning repo. Try later"
  exit 2
fi

git -C "$tmp_dir" checkout "$branch"
git -C "$tmp_dir" status
maxstns_line=$(grep 'MAXSTNS =' "$tmp_dir"/MOODS/source/stapos.f90)
maxstns=$(echo "$maxstns_line" | grep -o -E '[0-9]+' | cut -d' ' -f1)
echo "Max entries: $maxstns"

num_entries=$(wc -l "$tmp_dir"/install/abrv_stnlist | cut -d' ' -f1)
echo "Num entries: $num_entries"


diff -y --suppress-common-lines -W200  "$old_vola".stnmas "$new_vola".stnmas \
      | grep '>' | cut -d'>' -f2 > "$new_stations"

new_count=$(wc -l "$new_stations" | cut -d' ' -f1)
if [[ "$new_count" -gt 0 ]]; then
  echo "New stations"
  cat "$new_stations"
else
  echo "No new stations"
fi

new_total=$(( num_entries + new_count ))
if [[ "$new_total" -ge "$maxstns" ]]; then
  echo "Warning - too many new stations $new_total"
fi

diff -y --suppress-common-lines -W200  "$old_vola".stnmas "$new_vola".stnmas \
      | grep '|' | cut -d'|' -f2 > "$updated_stations"

upd_count=$(wc -l "$updated_stations" | cut -d' ' -f1)
if [[ "$upd_count" -gt 0 ]]; then
  echo "Updated stations"
  cat "$updated_stations"
else
  echo "No stations updated"
fi

diff -y --suppress-common-lines -W200  "$old_vola".stnmas "$new_vola".stnmas \
      | grep '<' | cut -d'<' -f1 > "$deleted_stations"

del_count=$(wc -l "$deleted_stations" | cut -d' ' -f1)
if [[ "$del_count" -gt 0 ]]; then
  echo "Old stations which may be deleted (but check on OSCAR first)"
  cat "$deleted_stations"
else
  echo "No stations deleted"
fi

tidy_up 2

echo "Completed"
