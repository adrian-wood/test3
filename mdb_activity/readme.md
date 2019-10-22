## MetDB Activity Collection and Analysis

### Purpose

These two scripts collect the Data Access logfiles and the MOOSE logfiles. The reason for this is that we aim at some point to be able to feed these logs into a further tool (hopefully LASER/ELK) for analysis. This will reveal useful information on MetDB usage such as most popular datatypes, user activity etc.

For now we are just using `scp` to copy the logfiles every night to a central location for future analysis.

### Running the Scripts
1. The scripts run on the Linux Server `mdb-apps` as the `moodsf` account via `cron`.
1. There are two scripts `get_data_access_logs.sh` and `get_moose_logs.sh`.
1. The `get_data_access_logs.sh` script scps the file `/var/moods/logs/data_access_log.<today>` to a datestamp subdirectory under `/var/www/html/mdb_activity/data_access_logs` and creates a symlink to that file in the `$INCOMING` directory, to ease future processing.
1. The reason for putting the data_access logs in `/var/www/html` is to make them browseable at http://www-mdb-apps/mdb_activity/data_access_logs/ - NB each file can be quite large and can make your browser very slow.
1. The `get_moose_logs.sh` script scp's all yesterday's logfiles that have been created by Moose restores (named `MDBREST2.output.d<datestamp>` in the `/var/moods/tmp` directory) and zips them into a single file at `/var/moods/mdb_activity/data/moose_logs/incoming`.
1. `cron` jobs are set up as follows:
```# mdb_activity suite...
00 02 * * * /var/moods/mdb_activity/get_data_access_logs.sh >> /tmp/mdb_activity_get_data_access_logs_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
00 03 * * * /var/moods/mdb_activity/get_moose_logs.sh >> /tmp/mdb_activity_get_moose_logs_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1```
