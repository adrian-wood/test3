#!/bin/bash
#-----------------------------------------------------------------------
#
# SCRIPT        : release_all_servicehub.sh
#
# PURPOSE       : Releases all held servicehub suites running under the current
#                 userid. 
#
#------------------------------------------------------------------------
# MB-1810: New                                             Sheila Needham
#------------------------------------------------------------------------

for suite in $(cylc print -xy servicehub); do
    echo "Releasing $suite..."
    cylc release $suite 
done
