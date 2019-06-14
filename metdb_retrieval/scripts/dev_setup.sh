#!/bin bash -l
# 
# This initialises your test environment when running outside of cycl.
# Set BASE_DIR and ECCODES according to your test requirements.
#
# Run from the command line:
# . ./dev_setup.sh
# Then you can run get_data.sh and send_data.sh from the command line instead
# of from cylc.
# 

module load scitools/experimental-current
module display scitools/experimental-current

export BASE_DIR=/home/sneedham/metdb-misc//metdb_retrieval
export ECCODES=/var/moods/eccodes
export ECCODES_DEFINITION_PATH=$ECCODES/share/eccodes/definitions:$BASE_DIR/local_defs
export CONTACT=sheila.needham@metoffice.gov.uk

echo "Testing MetDB Retrievals from $BASE_DIR"

