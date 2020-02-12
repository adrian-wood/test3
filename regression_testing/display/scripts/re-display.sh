#!/bin/bash -l  
module load scitools
module display scitools
#
set -x
setenv=/home/moodsf/cylc-run/regression/set_env.rc
source $setenv
export $(cut -d= -f1 $setenv)
set +x

# This removes the cylc version of python so that
# the scitools version is used instead.  Cylc still
# uses python 2.7 libraries which have an incompatible
# version of jinja2.

oldpath=$PYTHONPATH
unset PYTHONPATH

python $CGIBIN/TestDisplay.py

export PYTHONPATH=$oldpath

