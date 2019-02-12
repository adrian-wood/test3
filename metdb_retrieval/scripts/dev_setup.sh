#!/bin bash -l
# 
# Run this to initialise your test environment when running outside of cycl
# 

module load scitools/experimental_legacy-current
module display scitools/experimental_legacy-current

export BASE_DIR=/home/sneedham/metdb-misc-clean/metdb_retrieval
export ECCODES=/var/moods/eccodes

echo "Testing MetDB Retrievals from $BASE_DIR"

