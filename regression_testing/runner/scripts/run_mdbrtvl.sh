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
  echo 'Usage:run_mdbrtvl.sh <input request> <system one> <system two>'
  exit 1
fi  

REQUEST=$1
inp=${REQUEST##/*/}
SYS=( $2 $3 )

declare -a out_dirs

for system in ${SYS[*]}
do
  echo "Running retrieval on $system"
  out_dirs=("${out_dir[@]}" $OUTDIR/$system )
  out_dir=$OUTDIR/$system
  mkdir -p $out_dir

  echo "set up RPC using set_rpc_$system.sh"
  . $SCRIPTS/set_rpc_$system.sh
  env | grep METDB
  rm -f $out_dir/$inp.log
  $BINDIR/mdbrtvl.exe \
               $REQUEST \
               $out_dir/$inp.out >> $out_dir/$inp.log 2>&1
done 
echo "Retrievals complete"
echo "Comparing outputs"

diff ${out_dirs[0]}/$inp.out ${out_dirs[1]}/$inp.out >/dev/null 2>&1
rc=$?
echo "diff rc is $rc"
echo "complete"
exit $rc

