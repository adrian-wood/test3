## MetDB Activity Collection and Analysis

### Purpose

These two scripts collect the Data Access logfiles and the MOOSE logfiles. The reason for this is that we aim at some point to be able to feed these logs into a further tool (hopefully LASER/ELK) for analysis. This will reveal useful information on MetDB usage such as most popular datatypes, user activity etc.

For now we are just using `scp` to copy the logfiles every night to a central location for future analysis.

### Running the Scripts
1. The scripts run on the Linux Server `mdb-apps-test` as the `moodsf` account via `cron`.
1. There are two scripts `get_data_access_logs.sh` and `get_moose_logs.sh`.
1. The `get_data_access_logs.sh` script scps the file `/var/moods/logs/data_access_log.<today>` to a datestamp subdirectory under `/var/www/html/mdb_activity/data_access_logs` and creates a symlink to that file in the `$INCOMING` directory, to ease future processing.
1. The reason for putting the data_access logs in `/var/www/html` is to make them browseable at http://www-mdb-apps/mdb_activity/data_access_logs/ - NB each file can be quite large and can make your browser very slow.
1. The `get_moose_logs.sh` script scp's all yesterday's logfiles that have been created by Moose restores (named `MDBREST2.output.d<datestamp>` in the `/var/moods/tmp` directory) and zips them into a single file at `/var/moods/mdb_activity/data/moose_logs/incoming`.
1. `cron` jobs are set up as follows:
```# mdb_activity suite...
00 02 * * * /var/moods/mdb_activity/get_data_access_logs.sh >> /tmp/mdb_activity_get_data_access_logs_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
00 03 * * * /var/moods/mdb_activity/get_moose_logs.sh >> /tmp/mdb_activity_get_moose_logs_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
```

## Monthly Datatype Retrieval Analysis

### Purpose

This Python program (with a wrapper shell) analyses a number of Data Access logfiles and creates a web page from the results. It shows the number of times each datatype has been retrieved on each server. It also has a section listing datatypes that have never been retrieved.

### Running the Program
1. The script runs on the Linux Server `mdb-apps-test` as the `moodsf` account via `cron` on the 1st of every month.
1. The wrapper script is `monthly_datatypes.sh` which calls `monthly_datatypes.py` with calculated parameters.
1. The python script creates the web page from the Jinja template `monthly_datatypes_template.html`.
1. `cron` job is set up to run on the 1st of every month at 5am as follows:
```# Create monthly datatype analysis webpage...
00 05 01 * * /var/moods/mdb_activity/monthly_datatypes.sh >> /tmp/monthly_datatypes.log 2>&1
```

## Daily UserID/Contact Retrieval Analysis

### Purpose

This Python program (with a wrapper shell) analyses a number of Data Access logfiles and creates a web page from the results. It shows the retrieval counts by userid with subtotals by "contact", and the same data displayed the opposite
way, i.e. by contact with userid subtotals. It does this for the two servers mdbapus-prod and mdbapop-prod.

### Running the Program
1. The script runs on the Linux Server `mdb-apps-test` as the `moodsf` account via `cron` on the 1st of every month.
1. The wrapper script is `daily_user_contact.sh` which calls `daily_user_contact.py`.
1. The python script creates the web page from the Jinja template `daily_user_contact_template.html`.
1. `cron` job is set up to run every day at on the 1st of every month at 7am as follows:
```# Create daily userid and contact analysis webpage...
00 07 * * * /var/moods/mdb_activity/daily_user_contact.sh >> /tmp/daily_user_contact_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
```