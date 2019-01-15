#!/bin/sh
# 
# Runs a retrieval job on two systems and compares the output.
# Required arguments:                     
# 1 - input request file
# 2 - first system to run the retreival on
# 3 - second system to run the retreival on

echo "BASE $BASE"
echo "SCRIPTS $SCRIPTS"
echo "INDIR $INDIR"
echo "OUTDIR $OUTDIR"
echo "BINDIR $BINDIR"

if [ $# -ne 3 ]
then
  echo 'Usage:diff_grbrtvl.sh <input request> <system one> <system two>'
  exit 1
fi  

REQUEST=$1
inp=${REQUEST##/*/}
SYS=( $2 $3 )

datedir=$(date +"%Y/%m/%d")

declare -a out_dirs
count=0
for system in ${SYS[*]}
do
  echo "Running retrieval on $system"
  out_dir=$OUTDIR/$system/$datedir
  out_dirs[$count]=$out_dir
  mkdir -p $out_dir

  echo "set up RPC using set_rpc.sh $system"
  . $SCRIPTS/set_rpc.sh $system
  env | grep METDB
  rm -f $out_dir/$inp.log
  $BINDIR/getgrib.exe \
               $out_dir/$inp.out \
	       < $REQUEST \
	        $out_dir/$inp.out >> $out_dir/$inp.log 2>&1
  count=$(( count + 1 ))		
done 
echo "Retrievals complete"
echo "Comparing outputs"

diff ${out_dirs[0]}/$inp.out ${out_dirs[1]}/$inp.out >/dev/null 2>&1
rc=$?
echo "diff rc is $rc"
echo "complete"
exit $rc

