#!/bin/bash -l
module load  scitools/experimental_legacy-current
export ECCODES=/var/moods/eccodes
export ECCODES_DEFINITION_PATH=$ECCODES/share/eccodes/definitions:\
/var/moods/metdb_retrieval/local_defs/

python /var/www/cgi-bin/webret3/web_elements.py $QUERY_STRING

