#!/bin/bash -l  
module load scitools
module display scitools
#
# this bit just for testing - env will be set by cylc
#set -x
#setenv=/home/sneedham/cylc-run/regression/set_env.rc
#source $setenv
#export $(cut -d= -f1 $setenv)
#set +x

python $SCRIPTS/TestRunner.py

