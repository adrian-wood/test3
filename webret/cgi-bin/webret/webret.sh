#!/bin/bash -l
module load  scitools/experimental_legacy-current

python /var/www/cgi-bin/webret/webret.py $QUERY_STRING

