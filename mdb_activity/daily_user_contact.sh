#!/bin/bash -l
#------------------------------------------------------------------------------        
# PURPOSE : Wrapper to call daily_user_contact.py every day.
# ENV VARS: The following environment variables must/may be set:
#           MISC_BASE_DIR (optional): location of code. Defaults to /var/moods,
#             can be set to <cloned_repo> during development. 
#           DATA_ACCESS_LOG_DIR (optional): location of data_access log files.
#             Useful for creating historical reports from old data. Defaults to
#             /var/www/html/mdb_activity/data_access_logs.
#           HTML_PAGE (optional): location of resulting web page. If omitted,
#             page <YYYY>/<MM>/<DD>.html will be created in directory 
#             /var/www/html/mdb_activity/daily_user_contact_archive with
#             symlink to it at
#             /var/www/html/mdb_activity/daily_user_contact.html
#------------------------------------------------------------------------------

echo "`date "+%Y/%m/%d %H:%M:%S"`: $0 running... "

module load scitools
module display scitools

python /var/moods/mdb_activity/daily_user_contact.py

echo "`date "+%Y/%m/%d %H:%M:%S"`: $0 finished."
exit $rc
