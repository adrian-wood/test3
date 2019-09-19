#!/bin/bash -l  
module load scitools
module display scitools
#
# this bit just for testing - env will be set by cylc
# set -x
# setenv=/home/moodsf/cylc-run/regression/set_env.rc
# source $setenv
# export $(cut -d= -f1 $setenv)
# set +x

oldpath=$PYTHONPATH
unset PYTHONPATH
python $CGIBIN/TestDisplay.py
export PYTHONPATH=$oldpath


