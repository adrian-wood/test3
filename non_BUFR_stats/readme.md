## `non-BUFR_stats` Python script

### Purpose

This Python script produces a summary web page of statistics for `gribdat` and `storedf` MetDB jobs, i.e. those which store GRIB, NetCDF, HDF5 and XML. The page is composed of sections, each of which gives details for a particular set of MetDB jobs. There can be any number of jobs in a section but they must be the same type of job, `gribdat` or `storedf`.

Each job in the section has a line displayed for it. The line is a row of daily "cells" which provide a quick visual indicator as to whether or not the job has run as expected that day. If the cell is green then all is well; if yellow or red then further investigation should be done. Clicking on the content of the cell gives a summary of the jobs that have run on that particular day.

Each "column" of the page shows the details for a particular date. Five date columns are displayed, starting with "yesterday" on the left-hand side, and then yesterday-1, yesterday-2 etc. In this way the results for all these jobs can be seen on a single page, even after a long weekend.

The page also displays the current `MONITOR.errorlog` output at the start.

### Configuration
The script must be supplied with a configuration file by running it with the `-c <config_file>` option. The file is a typical config file with sections of items. In the file there must be a `General` section, followed by a `Layout` section, and finally a section for each MetDB job section.
1. `General` section
   1. `webBase` - this is the fully-qualifed "base" directory for web pages.
   1. `archiveBase` - the "archive" directory, where the actual HTML page will be created.
   1. `server` - a "suffix" for the above two directories, and also used when constructing URLs to retrieve job details from. For the Production MetDB system this should be `prod`, for Pre-Prod system it should be `preprod`.
   1. `logLevel` - set to `debug` for more information in the log file. If omitted then no output will be logged.
1. `Layout` section
   1. `sections` - this should just contain a list of the `<section_name>`s which follow. The page will be ordered depending on this configuration item.
1. `<section name>` section
   1. `type` - either `gribdat` or `storedf` - could be expanded in the future.
   1. `title` - the heading for this section.
   1. `freq` - the frequency of the job (only for display purposes).
   1. `job<n>` - a series of items, one per line in the summary. For `storedf` jobs, this should just be the MetDB job name, e.g. `MDBBDF1`. For `gribdat` jobs, it should be the MetDB job name and the expected number of files to receive in a day, e.g. `MDBGRIB0, 192`.

### Criteria for success
1. `storedf` jobs
   1. Success (green) - some data has been received for every datatype that the job is handling, and none of that data has been rejected. No counts are done.
   1. Warning (yellow) - data for one or more datatypes has *not* been received.
   1. Danger (red) - data for one or ore datatypes has been *rejected* by the job.
1. `gribdat` jobs
   1. Success (green) - the expected number of datasets have been received and "copied" (i.e. stored) by the job.
   1. Warning (yellow) - the expected number of datasets have not been "copied", though the expected number has been received.
   1. Danger (red) - the expected number of datasets have not been "copied", *and* the expected number of datasets have not been received.

### Running the Script
This is how the script is being run, locations etc. The script is running on the Linux Server `mdb-apps` as the `moodsf` account.
1. The script itself, both the `prod.cfg` and `preprod.cfg` files and the `non_BUFR_stats_template.html` Jinja2 template are in the directory `/var/moods/non_BUFR_stats`.
1. The base "archive" directory `/var/www/html/non_BUFR_stats_archive/` was created, with subdirectories of `prod` and `preprod`.
1. A "wrapper" shell script `run_non_BUFR_stats.sh` was written to execute the Python script. *NB the Python code is currentl not compatible with the Scientific Software Stack due to the absence of the `lxml` package in the `scitools/production-os41-1` environment. This should be addressed in the next release, when the script could/should be amended to use the SSS.*
1. `cron` jobs are set up as follows:
```# non-BUFR stats pages...
55 07 * * * /var/moods/non_BUFR_stats/run_non_BUFR_stats.sh -c /var/moods/non_BUFR_stats/prod.cfg >>/tmp/non_BUFR_stats_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
58 07 * * * /var/moods/non_BUFR_stats/run_non_BUFR_stats.sh -c /var/moods/non_BUFR_stats/preprod.cfg >>/tmp/non_BUFR_stats_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
```
