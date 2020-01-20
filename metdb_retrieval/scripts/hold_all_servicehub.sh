#!/bin/bash
#-----------------------------------------------------------------------
#
# SCRIPT        : hold_all_servicehub.sh
#
# PURPOSE       : Pauses all servicehub suites running under the current
#                 userid. 
#
#------------------------------------------------------------------------
# MB-1810: New                                             Sheila Needham
#------------------------------------------------------------------------

for suite in $(cylc print -xy servicehub); do
    echo "Pausing $suite..."
    cylc hold $suite 
done
