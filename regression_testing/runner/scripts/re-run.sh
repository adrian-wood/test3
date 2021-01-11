#!/bin/bash   
#
# This sources the environment for running outside of cylc
set -x
setenv=/home/moodsf/cylc-run/regression/set_env.rc
source $setenv
export $(cut -d= -f1 $setenv)
set +x

./daily_run.sh 
