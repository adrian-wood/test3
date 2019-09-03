#!/bin/bash
# 
# Runs a retrieval job on two systems or versions and compares the output.
# Required arguments:                     
# 1 - request input file

## These exports just for testing ##
#export SYS1=mdbap-preprod
#export SYS2=mdbap-preprod
#export VER1=5.20.0
#export VER2=5.19.0
#export SCRIPTS=/home/sneedham/regressionV2/runner/scripts/
#export INDIR=/home/sneedham/regressionV2/runner/input/
#export OUTDIR=/var/www/html/sheila/regression/output/
#export BINDIR=/home/sneedham/regressionV2/runner/bin/
## testing above ##
echo "SCRIPTS $SCRIPTS"
echo "INDIR $INDIR"
echo "OUTDIR $OUTDIR"
echo "BINDIR $BINDIR"
echo "SYS1 $SYS1"
echo "SYS2 $SYS2"
echo "VER1 $VER1"
echo "VER2 $VER2"

while getopts ":hmnrgp" opt; do
  case ${opt} in
    h ) 
        echo "Usage: $0 -h    Display this message"
        echo "       $0 -m    Run mdbrtvl.exe"
        echo "       $0 -n    Run ncdfrtvl.exe"
        echo "       $0 -r    Run retbufr.exe"
        echo "       $0 -g    Run gribret.exe"    
        echo "       $0 -p    Run python unittest"
      ;;
    m )
        EXE=mdbrtvl.exe
        SYS=( $SYS1 $SYS2 )
      ;;
    n )
        EXE=ncdfrtvl.exe
        SYS=( $SYS1 $SYS2 )
      ;;
    r )
        EXE=retbufr.exe
        SYS=( $SYS1 $SYS2 )
      ;;
    g )
        EXE=getgrib.exe
        SYS=( $SYS1 $SYS2 )
      ;;
    p )
        EXE=python
        SYS=( $WEB1 $WEB2 )
      ;;
     
    \? ) echo "Invalid option"
      ;;
  esac
done

shift $((OPTIND-1))

if [ "$#" -lt 1 ]
then
  echo "Required argument is request input file"
  exit 1
fi

if [ -z ${EXE+x} ]
then 
  echo "No option selected"
  exit 1
fi

set -x
#  REQUEST is absolute or relative path to the input file
REQUEST=$1

# inp is just the filename and will be used to generate
# output filenames
fullname=$(readlink -f $REQUEST)
inp=${fullname##/*/}

# Retrievals will be done on SYS1 with VER1 and SYS2 with VER2
VER=( $VER1 $VER2)

datedir=$(date +"%Y/%m/%d")
out_dir=$OUTDIR/$datedir
mkdir -p $out_dir
echo "Sending output to $out_dir"

count=0
for system in ${SYS[*]}
do
  ver=${VER[$count]}
  echo "set up RPC using set_rpc.sh $system and $ver"
  . $SCRIPTS/set_rpc.sh $system $ver

  # output filenames have _0 for sys1 and _1 for sys2
  filestub=${inp}_${count}

  # remove it in case this is a re-run
  rm -f $out_dir/${filestub}.log

 echo "Mode is ${EXE}"
  case "${EXE}" in

    "mdbrtvl.exe" )
                    echo "Running mdbrtvl"
                    $BINDIR/mdbrtvl.exe \
                            $REQUEST \
                            $out_dir/${filestub}.out >>\
                            $out_dir/${filestub}.log 2>&1
                   ;;
    "ncdfrtvl.exe" )
                    echo "Running ncdfrtvl"
                    ln -sf $REQUEST INPUT
                    touch OUTPUT
                    $BINDIR/ncdfrtvl.exe >>\
                            $out_dir/${filestub}.log 2>&1
                    cp OUTPUT $out_dir/${filestub}.out           
                    rm INPUT OUTPUT
                   ;;
    "retbufr.exe"  )
                     echo "Running retbufr"
                     $BINDIR/retbufr.exe $REQUEST \
                             $out_dir/${filestub}.out \
	                     $out_dir/${filestub}.data \
	                  >> $out_dir/${filestub}.log 2>&1
                   ;;
    "getgrib.exe"  )
                     echo "Running getgrib"
                     $BINDIR/getgrib.exe \
                             $out_dir/${filestub}.out \
	                     < $REQUEST \
	                     $out_dir/${filestub}.out >>\
                             $out_dir/${filestub}.log 2>&1
                   ;;
    "python"       )
                     echo "Running python unittest"
                     python $REQUEST $system > \
                            $out_dir/${filestub}.out 2>&1
  esac


  count=$(( count + 1 ))	       
done 
echo "Retrievals complete"
echo "Comparing outputs"

diff ${out_dir}/${inp}_0.out  ${out_dir}/${inp}_1.out >/dev/null 2>&1
rc=$?

echo "diff rc is $rc"
echo "complete"
exit $rc

