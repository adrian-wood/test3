#!/bin/bash -l
#------------------------------------------------------------------------------        
# PURPOSE : Script to create previous month's webpage analysis of datatypes
#           retrieved by reading data_access.log files.
#           Does a wget of the current retrieval_table from BitBucket to
#           determine what are "valid" datatypes, and which datatypes have not
#           been retrieved.
# ENV VARS: The following environment variables must/may be set:
#           RETRIEVAL_TABLE (mandatory): location of the MetDB retrieval table
#             file to be used.
#           MISC_BASE_DIR (optional): location of code. Defaults to /var/moods,
#             can be set to <cloned_repo> during development. 
#           DATA_ACCESS_LOG_DIR (optional): location of data_access log files.
#             Useful for creating historical reports from old data. Defaults to
#             /var/www/html/mdb_activity/monthly_datatype_retrievals_archive.
#           HTML_PAGE (optional): location of resulting web page. If omitted,
#             page <YYYY>/<Mon>.html will be created in directory 
#             /var/www/html/mdb_activity/monthly_datatype_retrievals_archive
#             with symlink to it at
#             /var/www/html/mdb_activity/monthly_datatype_retrievals.html
#------------------------------------------------------------------------------

echo "`date "+%Y/%m/%d %H:%M:%S"`: $0 running... "

module load scitools
module display scitools

tmp_dir=$(mktemp -d -t md-XXXXX)
wget -q https://bitbucket:8443/projects/MOOD/repos/metdb/raw/TABLES/retrieval_table?at=refs%2Fheads%2Fmaster -O $tmp_dir/retrieval_table
export RETRIEVAL_TABLE=$tmp_dir/retrieval_table

python /var/moods/mdb_activity/monthly_datatypes.py `date '+%Y %m' -d 'last month'`

rm -fr $tmp_dir

echo "`date "+%Y/%m/%d %H:%M:%S"`: $0 finished."
exit $rc
