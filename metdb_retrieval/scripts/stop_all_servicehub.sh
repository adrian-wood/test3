#!/bin/bash
#-----------------------------------------------------------------------
#
# SCRIPT        : stop_all_servicehub.sh
#
# PURPOSE       : Stops servicehub suites running under the current
#                 userid.  Currently running tasks will complete.
#
#------------------------------------------------------------------------
# MB-1810: New                                             Sheila Needham
#------------------------------------------------------------------------

for suite in $(cylc print -xy servicehub); do
    echo "Stopping $suite..."
    cylc stop $suite 
done
