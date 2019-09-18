#!/bin/bash -l  
module load scitools
module display scitools
#
# Source the environment - default is the operational
# one but you could set up something different. 
#
# Python script has one option: -t [testno]
#

setenv=/home/moodsf/cylc-run/regression/set_env.rc
source $setenv
export $(cut -d= -f1 $setenv)

python ./TestOne.py $*

echo "Clean up /tmp/TestOne when you're finished"
find /tmp/TestOne -type f -ls

