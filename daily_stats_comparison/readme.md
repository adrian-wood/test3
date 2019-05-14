Compare Daily Stats from Pre and Prod, and Produce Webpage
----------------------------------------------------------

This program reads the daily stats summary pages from both Pre and Prod,
creates a dictionary of subtype/value for each, processes the two
dictionaries and produces a web page of the results.

It creates an archive of these pages - each time it runs, the resulting page
is placed in `/compare_stats_archive/<yyyy>/<mm>/<dd>`, and the symlink
`new_compare_stats.html` is re-created to link to the most recent page.

The resulting webpage uses Bootstrap CSS for prettifying. No JS or anything else.
A Jinja2 template is used as the basis for the page.

The script should be run via cron on server metdb-apps as user moodsf as follows:
```
# Produce Stats Comparison web page
45 06 * * * /var/moods/daily_stats_comparison/run_new_compare_stats.sh > /tmp/daily_stats_comparison_`date "+\%Y\%m\%d_\%H\%M\%S"`.log 2>&1
```
